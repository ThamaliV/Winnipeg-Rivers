# Winnipeg rivers water quality dashboard

A Shiny dashboard for the City of Winnipeg's biweekly Rivers Survey Monitoring
Reports: 17-ish parameters sampled at bridges along the Assiniboine and Red
rivers through the open-water season.

The City only publishes these as one PDF per survey date, so there are two
parts: a pipeline that downloads and parses the PDFs into a tidy CSV, and the
app that reads it.

## Setup

```r
install.packages(c("pdftools", "rvest", "xml2", "dplyr", "stringr", "readr",
                   "purrr", "tibble", "shiny", "bslib", "plotly", "DT"))
```

## Run

From this folder:

```sh
Rscript run_pipeline.R              # all years from 2015 to now
Rscript run_pipeline.R 2020 2024    # or a range
Rscript run_pipeline.R --offline    # re-parse PDFs already downloaded
```

Then in R:

```r
shiny::runApp()
```

The pipeline caches PDFs in `data/raw/<year>/`, so re-runs only fetch new
reports. It writes:

- `data/river_quality.csv`: one row per survey date, site and parameter.
  `value` is numeric; `qualifier` is `<` / `>` for results beyond the lab's
  detection limits, or `no result`; `value_raw` keeps the text exactly as printed.
- `data/parse_log.csv`: one row per PDF with a status of `ok`, `check` or
  `failed`. **Look at this after the first run.**

## How the parser works

The report template has changed over the years. Older reports print site codes
like `(R1)` under each bridge name; 2025 reports drop the codes, add a
"Sample Number" row, and print a "Report Date" as well as the survey date.
Plain text extraction also scrambles the row labels. So `R/parse.R` works from
word coordinates via `pdftools::pdf_data()` rather than any one template:

1. Table rows are lines with several number-like cells.
2. Site columns come from clustering those numbers horizontally.
3. Each column is matched to a bridge by the name printed above it (never by
   position, since the set of sites on the page varies between reports).
4. Labels and units on the left are attached to the nearest row.

The survey date comes from the "Survey Date:" line, falling back to the file
name. It has been tested on the real 2015 and 2025 report contents rebuilt as
PDFs, not the full archive, so check `data/parse_log.csv` after a run: anything
unusual is marked `check` or `failed`. The usual fixes are a new label pattern
in `R/parameters.R` or a new unit token in `unit_token_re` in `R/parse.R`.

## Site coordinates

`data/sites.csv` has **approximate** bridge coordinates. For the City's exact
points, open their My Map, use the menu > Download KML (tick "Export as KML"),
save it as `data/sites.kml`, and run:

```r
source("R/scrape.R"); update_sites_from_kml()
```

## Reference lines

The dotted lines (dissolved oxygen 5 mg/L, E. coli 200 per 100 mL, pH 6.5 and
9.0) are for orientation, not a compliance check. Confirm them against the
current Manitoba Water Quality Standards, Objectives and Guidelines before
presenting them as guidelines. Edit them in `R/parameters.R`.

## Files

```
run_pipeline.R      download + parse + validate
app.R               the Shiny app
R/scrape.R          find/download report PDFs; KML import for site coords
R/parse.R           PDF table parser
R/parameters.R      parameter names, label patterns, reference lines
data/sites.csv      site names, rivers, downstream order, coordinates
```
