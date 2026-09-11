# Build data/river_quality.csv from the City's Rivers Survey PDFs
#
#   Rscript run_pipeline.R              # 2015 to this year
#   Rscript run_pipeline.R 2020 2024    # a year range
#   Rscript run_pipeline.R --offline    # just re-parse PDFs already in data/raw

needed <- c("pdftools", "rvest", "xml2", "dplyr", "stringr", "readr", "purrr", "tibble")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))",
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/parameters.R")
source("R/parse.R")
source("R/scrape.R")

args <- commandArgs(trailingOnly = TRUE)
offline <- "--offline" %in% args
yrs <- suppressWarnings(as.integer(args[args != "--offline"]))
yrs <- yrs[!is.na(yrs)]
years <- if (length(yrs) == 2) seq(yrs[1], yrs[2]) else if (length(yrs) == 1) yrs else
  2015:as.integer(format(Sys.Date(), "%Y"))

sites <- read_csv("data/sites.csv", show_col_types = FALSE)

if (!offline) {
  reports <- list_report_urls(years)
  invisible(download_reports(reports))
}

pdfs <- list.files("data/raw", "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (length(pdfs) == 0) stop("No PDFs in data/raw. Run without --offline first.")
message("Parsing ", length(pdfs), " PDFs...")

results <- lapply(pdfs, function(f) {
  tryCatch(
    list(data = parse_report(f, sites), error = NA_character_),
    error = function(e) list(data = NULL, error = conditionMessage(e))
  )
})

parsed <- bind_rows(lapply(results, `[[`, "data"))
errors <- tibble(file = basename(pdfs),
                 folder_year = basename(dirname(pdfs)),
                 error = vapply(results, `[[`, character(1), "error"))

if (nrow(parsed) == 0) {
  write_csv(errors, "data/parse_log.csv")
  message("\nNo PDF could be parsed. Most common errors:")
  print(count(errors, error, sort = TRUE), n = 10)
  stop("Nothing parsed; see data/parse_log.csv and the errors above.", call. = FALSE)
}

river_quality <- parsed |>
  left_join(select(sites, site_id, site_name, river, river_order), by = "site_id") |>
  arrange(survey_date, river, river_order, parameter)

# A per-file log so odd layouts are easy to spot
parse_log <- errors |>
  left_join(
    river_quality |>
      group_by(file = source_file) |>
      summarise(survey_date = first(survey_date),
                n_sites = n_distinct(site_id),
                n_parameters = n_distinct(parameter),
                unmatched_rows = sum(startsWith(parameter, "unmatched")),
                unknown_sites = sum(!site_id %in% sites$site_id)),
    by = "file"
  ) |>
  mutate(status = case_when(
    !is.na(error) ~ "failed",
    is.na(survey_date) ~ "check: no date",
    format(survey_date, "%Y") != folder_year ~ "check: date doesn't match folder year",
    n_parameters < 12 | unmatched_rows > 0 | unknown_sites > 0 ~ "check",
    TRUE ~ "ok"
  ))

write_csv(river_quality, "data/river_quality.csv")
write_csv(parse_log, "data/parse_log.csv")

message(sprintf(
  "Done: %d rows from %d surveys (%s to %s). %d ok, %d to check, %d failed. See data/parse_log.csv.",
  nrow(river_quality), n_distinct(river_quality$survey_date),
  min(river_quality$survey_date, na.rm = TRUE), max(river_quality$survey_date, na.rm = TRUE),
  sum(parse_log$status == "ok"), sum(startsWith(parse_log$status, "check")),
  sum(parse_log$status == "failed")
))
