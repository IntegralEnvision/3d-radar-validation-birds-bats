# Utilities for the consolidated detection-rate result bundle.

detection_rate_bundle_path <- function() {
  project_path("derived_data", "detection_rate_results.rds")
}

detection_rate_result_names <- function() {
  c(
    "flight_configuration", "analysis_failures", "bin_option_failures",
    "bin_option_availability_summary", "all_detection_bins",
    "all_flight_detection_summary", "all_gap_events",
    "all_flight_gap_statistics", "behavior_detection_summary",
    "behavior_gap_summary", "radar_type_detection_summary",
    "radar_type_gap_summary", "behavior_radar_detection_summary"
  )
}

validate_detection_rate_bundle <- function(bundle) {
  required <- c("schema_version", "analysis", "opportunity_method", "parameters", "results")
  missing <- setdiff(required, names(bundle))
  if (length(missing)) stop("Detection-rate bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(bundle$analysis, "detection_rate")) stop("Unexpected analysis type.", call. = FALSE)
  if (!identical(bundle$opportunity_method, "complete_drone_coverage_bins")) stop("Unexpected detection-opportunity method.", call. = FALSE)
  missing <- setdiff(detection_rate_result_names(), names(bundle$results))
  if (length(missing)) stop("Detection-rate results are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  for (name in detection_rate_result_names()) {
    if (!is.data.frame(bundle$results[[name]])) stop("Result is not a data frame: ", name, call. = FALSE)
  }
  if (!nrow(bundle$results$flight_configuration)) stop("No configured detection-rate flights.", call. = FALSE)
  if (!nrow(bundle$results$all_detection_bins)) stop("No detection-opportunity bins.", call. = FALSE)
  if (!nrow(bundle$results$all_flight_detection_summary)) stop("No flight detection summaries.", call. = FALSE)
  invisible(bundle)
}

write_detection_rate_bundle <- function(envir = parent.frame(), path = detection_rate_bundle_path()) {
  result_names <- detection_rate_result_names()
  missing <- result_names[!vapply(result_names, exists, logical(1), envir = envir, inherits = FALSE)]
  if (length(missing)) stop("Cannot save detection-rate bundle; objects are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  results <- setNames(lapply(result_names, get, envir = envir, inherits = FALSE), result_names)
  bundle <- list(
    schema_version = "1.0",
    analysis = "detection_rate",
    opportunity_method = "complete_drone_coverage_bins",
    parameters = list(
      bin_width_options_seconds = get("BIN_WIDTH_OPTIONS_SECONDS", envir = envir),
      shifted_bins = "half_bin_width_forward",
      cone_of_silence_meters_by_radar = get("CONE_OF_SILENCE_METERS_BY_RADAR", envir = envir),
      drone_evaluation_step_seconds = get("DRONE_EVALUATION_STEP_SECONDS", envir = envir),
      projected_crs = get("PROJECTED_CRS", envir = envir)
    ),
    results = results,
    updated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  validate_detection_rate_bundle(bundle)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, path)
  saved <- readRDS(path)
  validate_detection_rate_bundle(saved)
  if (!identical(bundle$results, saved$results)) stop("Saved detection-rate results failed round-trip validation.", call. = FALSE)
  invisible(path)
}

read_detection_rate_bundle <- function(path = detection_rate_bundle_path()) {
  if (!file.exists(path)) stop("Detection-rate results not found: ", path, "\nRun detection_rate_analysis.R first.", call. = FALSE)
  bundle <- readRDS(path)
  validate_detection_rate_bundle(bundle)
  bundle
}

load_detection_rate_results <- function(bundle = read_detection_rate_bundle(), envir = parent.frame()) {
  list2env(bundle$results, envir = envir)
  assign("BIN_WIDTH_OPTIONS_SECONDS", bundle$parameters$bin_width_options_seconds, envir = envir)
  cone_by_radar <- bundle$parameters$cone_of_silence_meters_by_radar
  if (is.null(cone_by_radar)) {
    cone_by_radar <- setNames(
      rep(bundle$parameters$cone_of_silence_meters, 2L),
      c("9090", "7360")
    )
  }
  assign("CONE_OF_SILENCE_METERS_BY_RADAR", cone_by_radar, envir = envir)
  assign("DRONE_EVALUATION_STEP_SECONDS", bundle$parameters$drone_evaluation_step_seconds, envir = envir)
  assign("PROJECTED_CRS", bundle$parameters$projected_crs, envir = envir)
  invisible(bundle)
}
