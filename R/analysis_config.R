# Read and validate publication-facing analysis settings.

load_analysis_config <- function(path = file.path("config", "analysis_config.yml")) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read ", path, ".", call. = FALSE)
  }
  if (!file.exists(path)) stop("Analysis configuration not found: ", path, call. = FALSE)
  config <- yaml::read_yaml(path)

  required_top_level <- c(
    "metadata_file", "expected_number_of_flights", "projected_crs",
    "outputs", "positional_discrepancy", "detection_rate"
  )
  missing <- setdiff(required_top_level, names(config))
  if (length(missing)) stop("Configuration is missing: ", paste(missing, collapse = ", "), call. = FALSE)

  config$expected_number_of_flights <- as.integer(config$expected_number_of_flights)
  config$projected_crs <- as.integer(config$projected_crs)

  offsets <- unlist(config$positional_discrepancy$radar_y_offsets_m, use.names = TRUE)
  config$positional_discrepancy$radar_y_offsets_m <- setNames(as.numeric(offsets), names(offsets))
  cones <- unlist(config$detection_rate$cone_of_silence_m, use.names = TRUE)
  config$detection_rate$cone_of_silence_m <- setNames(as.numeric(cones), names(cones))
  config$detection_rate$bin_widths_seconds <-
    as.numeric(unlist(config$detection_rate$bin_widths_seconds, use.names = FALSE))
  config$detection_rate$shifted_bin_fraction <-
    as.numeric(config$detection_rate$shifted_bin_fraction)
  config$detection_rate$drone_evaluation_step_seconds <-
    as.numeric(config$detection_rate$drone_evaluation_step_seconds)

  radar_types <- c("9090", "7360")
  if (!setequal(names(config$positional_discrepancy$radar_y_offsets_m), radar_types)) {
    stop("Positional radar Y offsets must be named for radar types 9090 and 7360.", call. = FALSE)
  }
  if (!setequal(names(config$detection_rate$cone_of_silence_m), radar_types)) {
    stop("Detection cone distances must be named for radar types 9090 and 7360.", call. = FALSE)
  }
  if (any(!is.finite(config$positional_discrepancy$radar_y_offsets_m))) {
    stop("Radar Y offsets must be finite numbers.", call. = FALSE)
  }
  if (any(!is.finite(config$detection_rate$cone_of_silence_m)) ||
      any(config$detection_rate$cone_of_silence_m <= 0)) {
    stop("Cone-of-silence distances must be positive numbers.", call. = FALSE)
  }
  if (!length(config$detection_rate$bin_widths_seconds) ||
      any(config$detection_rate$bin_widths_seconds <= 0)) {
    stop("Detection bin widths must be positive numbers.", call. = FALSE)
  }
  if (!is.finite(config$detection_rate$shifted_bin_fraction) ||
      config$detection_rate$shifted_bin_fraction < 0 ||
      config$detection_rate$shifted_bin_fraction >= 1) {
    stop("Shifted-bin fraction must be at least 0 and less than 1.", call. = FALSE)
  }
  if (!is.finite(config$detection_rate$drone_evaluation_step_seconds) ||
      config$detection_rate$drone_evaluation_step_seconds <= 0) {
    stop("Drone evaluation step must be a positive number.", call. = FALSE)
  }
  config
}
