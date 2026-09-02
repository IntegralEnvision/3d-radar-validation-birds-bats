# Main entry point for the positional-discrepancy analysis.
# This pipeline intentionally does not run the detection-rate analysis.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(ggplot2)
  library(plotly)
  library(scales)
})
source(file.path("R", "analysis_config.R"))
analysis_config <- load_analysis_config()
source(file.path("R", "positional_discrepancy_metrics.R"))
source(file.path("R", "flight_metadata.R"))
source(file.path("R", "positional_discrepancy_bundle.R"))
source(file.path("R", "positional_discrepancy_flight.R"))
source(file.path("R", "positional_discrepancy_pipeline.R"))

arguments <- commandArgs(trailingOnly = TRUE)
argument_value <- function(name, default) {
  prefix <- paste0("--", name, "=")
  match <- arguments[startsWith(arguments, prefix)]
  if (!length(match)) return(default)
  sub(prefix, "", match[[1]], fixed = TRUE)
}
results_path <- argument_value("results-file", analysis_config$positional_discrepancy$results_file)
radar_y_offsets_by_type <- c(
  "9090" = as.numeric(argument_value("radar-y-offset-9090", as.character(analysis_config$positional_discrepancy$radar_y_offsets_m[["9090"]]))),
  "7360" = as.numeric(argument_value("radar-y-offset-7360", as.character(analysis_config$positional_discrepancy$radar_y_offsets_m[["7360"]])))
)
run_positional_discrepancy_analysis(
  metadata_path = project_path(analysis_config$metadata_file),
  radar_y_offsets_by_type = radar_y_offsets_by_type,
  results_path = results_path
)
message("Saved positional-discrepancy analysis results: ", results_path)
