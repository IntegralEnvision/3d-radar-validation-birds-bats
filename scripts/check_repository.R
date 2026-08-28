# Lightweight repository and analysis-contract checks.

source(file.path("R", "analysis_config.R"))
source(file.path("R", "flight_metadata.R"))
source(file.path("R", "positional_deviation_bundle.R"))
source(file.path("R", "detection_rate_bundle.R"))

arguments <- commandArgs(trailingOnly = TRUE)
require_data <- "--require-data" %in% arguments
config <- load_analysis_config()

stopifnot(
  identical(config$expected_number_of_flights, 85L),
  identical(config$positional_deviation$radar_y_offsets_m, c("9090" = 20, "7360" = 0)),
  identical(config$detection_rate$cone_of_silence_m, c("9090" = 300, "7360" = 150)),
  identical(config$detection_rate$bin_widths_seconds, c(1, 2, 3, 4, 5)),
  identical(config$detection_rate$shifted_bin_fraction, 0.5)
)

metadata <- read_flight_metadata(
  path = project_path(config$metadata_file),
  check_files = require_data
)
stopifnot(
  nrow(metadata) == config$expected_number_of_flights,
  !anyDuplicated(metadata$flight_id),
  all(metadata$radar_type %in% c("9090", "7360")),
  all(startsWith(metadata$radar_file, "data/")),
  all(startsWith(metadata$drone_file, "data/")),
  all(startsWith(metadata$radar_location_file, "data/"))
)

r_files <- c(
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  list.files("scripts", pattern = "[.]R$", full.names = TRUE)
)
for (file in r_files) parse(file)

baseline_files <- list.files(
  file.path("validation", "baseline_figures"),
  pattern = "_baseline[.]pdf$",
  full.names = TRUE
)
stopifnot(length(baseline_files) == 12L)

if (file.exists(positional_deviation_bundle_path())) {
  validate_positional_deviation_bundle(
    readRDS(positional_deviation_bundle_path()),
    require_all_behaviors = TRUE
  )
}
if (file.exists(detection_rate_bundle_path())) {
  validate_detection_rate_bundle(readRDS(detection_rate_bundle_path()))
}

message(
  "Repository checks passed (", nrow(metadata), " metadata rows; ",
  length(r_files), " R files; ", length(baseline_files), " baseline figures)."
)
