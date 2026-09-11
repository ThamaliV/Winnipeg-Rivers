# Winnipeg rivers water quality dashboard


suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(DT)
  library(readr)
})
source("R/parameters.R")

# Data ------------------------------------------------------------------------

sites <- read_csv("data/sites.csv", show_col_types = FALSE) |>
  arrange(river != "Assiniboine", river_order)

data_path <- "data/river_quality.csv"
has_data <- file.exists(data_path)

if (has_data) {
  rq <- read_csv(data_path, show_col_types = FALSE,
                 col_types = cols(qualifier = col_character(), unit = col_character())) |>
    mutate(qualifier = coalesce(qualifier, ""),
           censored = qualifier %in% c("<", ">"),
           year = as.integer(format(survey_date, "%Y"))) |>
    filter(!is.na(survey_date), site_id %in% sites$site_id) |>
    left_join(select(sites, site_id, lat, lon), by = "site_id")

  param_info <- rq |>
    filter(!is.na(value)) |>
    count(parameter, unit) |>
    group_by(parameter) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    inner_join(param_lookup, by = "parameter") |>
    arrange(match(parameter, param_lookup$parameter))

  param_choices <- setNames(
    param_info$parameter,
    ifelse(is.na(param_info$unit), param_info$display,
           paste0(param_info$display, " (", param_info$unit, ")"))
  )
  survey_dates <- sort(unique(rq$survey_date), decreasing = TRUE)
  year_range <- range(rq$year)
}

# Colours: each river gets its own ramp, pale upstream to dark downstream
river_dark <- c(Assiniboine = "#3F5E37", Red = "#6E3419")
site_colours <- c(
  setNames(colorRampPalette(c("#A9C094", river_dark[["Assiniboine"]]))(sum(sites$river == "Assiniboine")),
           sites$site_name[sites$river == "Assiniboine"]),
  setNames(colorRampPalette(c("#DDB08A", river_dark[["Red"]]))(sum(sites$river == "Red")),
           sites$site_name[sites$river == "Red"])
)
ink <- "#1D2A2E"
ref_colour <- "#3E6A8A"
heat_scale <- list(c(0, "#F4EEE4"), c(0.5, "#C8925F"), c(1, "#5A2A14"))

# Helpers ---------------------------------------------------------------------

fmt_date <- function(d, month = "%B") {
  paste0(format(d, month), " ", as.integer(format(d, "%d")), ", ", format(d, "%Y"))
}

fmt_value <- function(d) paste0(d$value_raw, ifelse(is.na(d$unit), "", paste0(" ", d$unit)))

base_layout <- function(p, ylab, log = FALSE) {
  p |>
    layout(
      font = list(family = "Public Sans, sans-serif", color = ink, size = 13),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      yaxis = list(title = ylab, type = if (log) "log" else "linear",
                   dtick = if (log) 1 else NULL,
                   gridcolor = "#DCE2E0", zeroline = FALSE),
      hoverlabel = list(font = list(family = "Public Sans, sans-serif")),
      margin = list(t = 30)
    ) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

ref_shapes <- function(param, show, log = FALSE) {
  if (!show) return(list(shapes = list(), annotations = list()))
  refs <- reference_lines |> filter(parameter == param)
  # On log axes plotly takes shape y in data units but annotation y in log10 units
  ann_y <- if (log) log10(refs$y) else refs$y
  list(
    shapes = lapply(refs$y, function(y) list(
      type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = y, y1 = y,
      line = list(color = ref_colour, dash = "dot", width = 1.5))),
    annotations = lapply(seq_len(nrow(refs)), function(i) list(
      xref = "paper", x = 1, xanchor = "right", yref = "y", y = ann_y[i],
      yanchor = "bottom", text = refs$label[i], showarrow = FALSE,
      font = list(color = ref_colour, size = 11)))
  )
}

# Colour bars for log-coloured plots show real values (1, 10, 100...) not log10
colour_bar <- function(log) {
  if (!log) return(list(title = list(text = ""), thickness = 12))
  list(title = list(text = ""), thickness = 12,
       tickvals = -3:6, ticktext = format(10^(-3:6), scientific = FALSE, drop0trailing = TRUE, trim = TRUE))
}

empty_plot <- function(msg) {
  plotly_empty() |>
    layout(annotations = list(text = msg, showarrow = FALSE,
                              font = list(size = 14, color = "#6B7A7E"))) |>
    config(displayModeBar = FALSE)
}

# UI --------------------------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bg = "#F2F4F3", fg = ink, primary = "#2F5D6B",
  base_font = font_collection(font_google("Public Sans", local = FALSE),
                              "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  "border-radius" = "0.4rem"
)

css <- "
  .bslib-page-title { font-weight: 700; letter-spacing: -0.01em; }
  .sidebar-note { font-size: 0.82rem; color: #56666B; line-height: 1.45; }
  .river-key span { display: inline-block; width: 0.8rem; height: 0.8rem;
                    border-radius: 50%; margin-right: 0.35rem; vertical-align: -0.1rem; }
  .card-header { font-weight: 600; }
"

if (!has_data) {
  ui <- page_fillable(
    theme = theme, tags$style(css),
    card(
      card_header("Winnipeg rivers water quality"),
      p("There's no data yet. From this folder, run:"),
      tags$pre("Rscript run_pipeline.R"),
      p("It downloads the City's Rivers Survey reports, turns them into ",
        code("data/river_quality.csv"), ", and logs anything odd in ",
        code("data/parse_log.csv"), ". Then start the app again.")
    )
  )
} else {
  ui <- page_sidebar(
    title = "Winnipeg rivers water quality",
    theme = theme,
    tags$head(tags$style(css)),
    sidebar = sidebar(
      width = 290,
      selectInput("param", "Measurement", param_choices,
                  selected = if ("e_coli" %in% param_choices) "e_coli" else param_choices[1]),
      selectInput("date", "Survey date", setNames(as.character(survey_dates),
                                                   fmt_date(survey_dates))),
      sliderInput("years", "Years shown over time", min = year_range[1], max = year_range[2],
                  value = c(max(year_range[1], year_range[2] - 2), year_range[2]),
                  step = 1, sep = ""),
      checkboxGroupInput("rivers", "Rivers", c("Assiniboine", "Red"),
                         selected = c("Assiniboine", "Red"), inline = TRUE),
      input_switch("log", "Log scale", TRUE),
      input_switch("ref", "Show reference lines", TRUE),
      div(class = "river-key sidebar-note",
          div(tags$span(style = paste0("background:", river_dark[["Assiniboine"]])), "Assiniboine River"),
          div(tags$span(style = paste0("background:", river_dark[["Red"]])), "Red River")),
      p(class = "sidebar-note",
        "Hollow markers are below the lab's detection limit; they're plotted at the limit. ",
        "Reference lines are for orientation only, not a compliance check."),
      p(class = "sidebar-note",
        "Source: City of Winnipeg Water and Waste, Rivers Survey Monitoring Reports."),
      p(class = "sidebar-note", HTML("&copy; 2026 Thamali Vidanage. All rights reserved."))
    ),
    navset_card_underline(
      nav_panel("Along the river", plotlyOutput("profile", height = "520px")),
      nav_panel("Over time",
                plotlyOutput("trend", height = "380px"),
                plotlyOutput("heat", height = "340px")),
      nav_panel("Map", plotlyOutput("map", height = "560px")),
      nav_panel("Data",
                div(class = "mb-2", downloadButton("download", "Download full dataset (CSV)")),
                DTOutput("table"))
    )
  )
}

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  if (!has_data) return()

  observeEvent(input$param, {
    update_switch("log", value = param_lookup$log_default[param_lookup$parameter == input$param])
  })

  ylab <- reactive({
    info <- param_info[param_info$parameter == input$param, ]
    if (is.na(info$unit)) info$display else paste0(info$display, " (", info$unit, ")")
  })

  param_data <- reactive({
    rq |> filter(parameter == input$param, !is.na(value))
  })

  # Along the river: one survey, sites in downstream order -------------------
  output$profile <- renderPlotly({
    d <- param_data() |> filter(survey_date == as.Date(input$date))
    if (nrow(d) == 0) return(empty_plot("No results for this measurement on this date."))

    refs <- ref_shapes(input$param, input$ref, input$log)
    # The Assiniboine enters the Red at The Forks, between Norwood and Redwood.
    # Shapes are added per panel: subplot() drops shapes set after it.
    forks_x <- mean(sites$river_order[sites$site_name %in% c("Norwood Bridge", "Redwood Bridge")])
    forks <- list(type = "line", xref = "x", x0 = forks_x, x1 = forks_x,
                  yref = "paper", y0 = 0, y1 = 1,
                  line = list(color = river_dark[["Assiniboine"]], dash = "dash", width = 1.5))
    forks_label <- list(xref = "x", x = forks_x, yref = "paper", y = 1, yanchor = "bottom",
                        text = "Assiniboine joins at The Forks", showarrow = FALSE,
                        font = list(color = river_dark[["Assiniboine"]], size = 11))

    panel <- function(rv) {
      s <- sites |> filter(river == rv)
      dd <- d |> filter(river == rv) |> arrange(river_order)
      is_red <- rv == "Red"
      if (nrow(dd) == 0) {
        p <- plot_ly(x = s$river_order, y = NA_real_, type = "scatter", mode = "markers",
                     hoverinfo = "none", showlegend = FALSE)
      } else p <- plot_ly(dd, x = ~river_order, y = ~value, type = "scatter", mode = "lines+markers",
              line = list(color = river_dark[[rv]], width = 2.5),
              marker = list(color = river_dark[[rv]], size = 11,
                            symbol = ifelse(dd$censored, "circle-open", "circle"),
                            line = list(width = 2, color = river_dark[[rv]])),
              text = paste0("<b>", dd$site_name, "</b><br>", fmt_value(dd)),
              hoverinfo = "text", name = paste(rv, "River"), showlegend = FALSE)
      p |>
        layout(
          xaxis = list(tickvals = s$river_order, ticktext = gsub(" ", "<br>", s$site_name),
                       range = c(0.5, max(s$river_order) + 0.5),
                       title = list(text = paste(rv, "River, flowing \u2192"),
                                    font = list(color = river_dark[[rv]])),
                       showgrid = FALSE, fixedrange = TRUE),
          shapes = c(refs$shapes, if (is_red) list(forks)),
          annotations = c(if (is_red) refs$annotations, if (is_red) list(forks_label))
        )
    }

    subplot(panel("Assiniboine"), panel("Red"), widths = c(0.38, 0.62),
            shareY = TRUE, titleX = TRUE, margin = 0.03) |>
      base_layout(ylab(), input$log) |>
      layout(margin = list(t = 40, b = 90))
  })

  # Over time --------------------------------------------------------------
  trend_data <- reactive({
    param_data() |>
      filter(year >= input$years[1], year <= input$years[2], river %in% input$rivers)
  })

  output$trend <- renderPlotly({
    d <- trend_data()
    if (nrow(d) == 0) return(empty_plot("No results for this selection."))
    # Break lines over winter so seasons don't join up
    breaks <- expand.grid(site_name = unique(d$site_name),
                          survey_date = as.Date(paste0(unique(d$year), "-01-01")),
                          stringsAsFactors = FALSE)
    d <- bind_rows(d, breaks) |> arrange(survey_date)

    p <- plot_ly()
    for (s in intersect(sites$site_name, d$site_name)) {
      ds <- d |> filter(site_name == s)
      p <- p |> add_trace(
        data = ds, x = ~survey_date, y = ~value, type = "scatter", mode = "lines+markers",
        name = s, legendgroup = ds$river[!is.na(ds$river)][1],
        line = list(color = site_colours[[s]], width = 1.8),
        marker = list(color = site_colours[[s]], size = 6,
                      symbol = ifelse(coalesce(ds$censored, FALSE), "circle-open", "circle")),
        text = paste0("<b>", s, "</b><br>", fmt_date(ds$survey_date, "%b"), "<br>",
                      ifelse(is.na(ds$value_raw), "", fmt_value(ds))),
        hoverinfo = "text", connectgaps = FALSE)
    }
    # Collapse the winters (no sampling under ice) so seasons sit side by side
    season <- trend_data() |> group_by(year) |>
      summarise(first = min(survey_date), last = max(survey_date)) |> arrange(year)
    winters <- if (nrow(season) > 1) lapply(seq_len(nrow(season) - 1), function(i) {
      list(bounds = list(as.character(season$last[i] + 10),
                         as.character(season$first[i + 1] - 10)))
    }) else list()

    ticks <- as.Date(unlist(lapply(season$year, function(y) paste0(y, c("-06-01", "-09-01")))))
    refs <- ref_shapes(input$param, input$ref, input$log)
    p |>
      base_layout(ylab(), input$log) |>
      layout(xaxis = list(title = "", showgrid = FALSE, rangebreaks = winters,
                          tickvals = ticks, ticktext = format(ticks, "%b %Y"), tickangle = 0),
             legend = list(font = list(size = 11), tracegroupgap = 12),
             shapes = refs$shapes, annotations = refs$annotations)
  })

  output$heat <- renderPlotly({
    d <- trend_data()
    if (nrow(d) == 0) return(empty_plot(""))
    order <- rev(intersect(sites$site_name, d$site_name))  # upstream at the top
    d <- d |> mutate(
      z = if (input$log) log10(pmax(value, 1e-6)) else value,
      site_f = factor(site_name, levels = order),
      date_lab = as.character(survey_date),
      tip = paste0("<b>", site_name, "</b><br>", fmt_date(survey_date, "%b"), "<br>",
                   paste0(value_raw, ifelse(is.na(unit), "", paste0(" ", unit))))
    )
    plot_ly(d, x = ~date_lab, y = ~site_f, z = ~z, text = ~tip,
            type = "heatmap", colorscale = heat_scale, xgap = 2, ygap = 2,
            hoverinfo = "text",
            colorbar = colour_bar(input$log)) |>
      layout(
        font = list(family = "Public Sans, sans-serif", color = ink, size = 12),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", type = "category", tickangle = -45,
                     tickfont = list(size = 10), nticks = 20),
        yaxis = list(title = "", type = "category"),
        margin = list(l = 10, t = 10)
      ) |>
      config(displaylogo = FALSE)
  })

  # Map --------------------------------------------------------------------
  output$map <- renderPlotly({
    d <- param_data() |> filter(survey_date == as.Date(input$date), river %in% input$rivers)
    missing <- sites |> filter(river %in% input$rivers, !site_id %in% d$site_id)
    colour_val <- if (input$log) log10(pmax(d$value, 1e-6)) else d$value

    p <- plot_ly() |>
      add_trace(data = d, type = "scattermapbox", mode = "markers",
                lat = ~lat, lon = ~lon,
                marker = list(size = 18, color = colour_val, colorscale = heat_scale,
                              showscale = nrow(d) > 0, opacity = 0.95,
                              colorbar = colour_bar(input$log)),
                text = paste0("<b>", d$site_name, "</b><br>", fmt_value(d)),
                hoverinfo = "text", name = "Sampled")
    if (nrow(missing)) {
      p <- p |> add_trace(data = missing, type = "scattermapbox", mode = "markers",
                          lat = ~lat, lon = ~lon, marker = list(size = 12, color = "#9AA5A8"),
                          text = paste0("<b>", missing$site_name, "</b><br>No result on this date"),
                          hoverinfo = "text", name = "No result")
    }
    p |>
      layout(mapbox = list(style = "open-street-map", zoom = 9.2,
                           center = list(lat = 49.93, lon = -97.17)),
             margin = list(l = 0, r = 0, t = 0, b = 0), showlegend = FALSE,
             font = list(family = "Public Sans, sans-serif")) |>
      config(displaylogo = FALSE)
  })

  # Data -------------------------------------------------------------------
  output$table <- renderDT({
    trend_data() |>
      arrange(desc(survey_date), match(site_id, sites$site_id)) |>
      transmute(Date = survey_date, Site = site_name, River = river,
                Result = value_raw, Unit = unit) |>
      datatable(rownames = FALSE, filter = "top",
                options = list(pageLength = 15, dom = "tip"))
  })

  output$download <- downloadHandler(
    filename = function() "winnipeg_river_quality.csv",
    content = function(file) file.copy(data_path, file)
  )
}

shinyApp(ui, server)




