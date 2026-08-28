# Utilities for the consolidated positional-deviation result bundle.

positional_deviation_bundle_path <- function() {
  project_path("derived_data", "positional_deviation_results.rds")
}

new_positional_deviation_bundle <- function() {
  list(schema_version = "1.0", analysis = "positional_deviation",
       alignment_method = "approved_common_grid", behaviors = list())
}

validate_positional_deviation_bundle <- function(bundle, require_all_behaviors = FALSE) {
  required <- c("schema_version", "analysis", "alignment_method", "behaviors")
  missing <- setdiff(required, names(bundle))
  if (length(missing)) stop("Bundle is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(bundle$analysis, "positional_deviation")) stop("Unexpected analysis type.", call. = FALSE)
  if (!identical(bundle$alignment_method, "approved_common_grid")) stop("Unexpected alignment method.", call. = FALSE)
  expected <- c("cubes", "chasing", "foraging", "soaring", "transiting")
  missing <- setdiff(expected, names(bundle$behaviors))
  if (require_all_behaviors && length(missing)) stop("Bundle is missing behaviors: ", paste(missing, collapse = ", "), call. = FALSE)
  for (behavior in names(bundle$behaviors)) {
    result <- bundle$behaviors[[behavior]]
    missing <- setdiff(c("paired_data", "rmse", "flight_metadata"), names(result))
    if (length(missing)) stop("Behavior ", behavior, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    if (!is.data.frame(result$paired_data) || !nrow(result$paired_data)) stop("No paired data for ", behavior, call. = FALSE)
    if (!is.data.frame(result$rmse) || !nrow(result$rmse)) stop("No RMSE data for ", behavior, call. = FALSE)
  }
  invisible(bundle)
}

read_positional_deviation_bundle <- function(path = positional_deviation_bundle_path(), require_all_behaviors = TRUE) {
  if (!file.exists(path)) stop("Results not found: ", path, "\nRun behavior analyses first.", call. = FALSE)
  bundle <- readRDS(path)
  validate_positional_deviation_bundle(bundle, require_all_behaviors)
  bundle
}

update_positional_deviation_bundle <- function(behavior, paired_data, rmse, flight_metadata,
                                                path = positional_deviation_bundle_path()) {
  expected <- c("cubes", "chasing", "foraging", "soaring", "transiting")
  if (!behavior %in% expected) stop("Unknown behavior: ", behavior, call. = FALSE)
  bundle <- if (file.exists(path)) read_positional_deviation_bundle(path, FALSE) else new_positional_deviation_bundle()
  bundle$behaviors[[behavior]] <- list(
    paired_data = paired_data, rmse = rmse, flight_metadata = flight_metadata,
    updated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  bundle$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  validate_positional_deviation_bundle(bundle)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, path)
  invisible(path)
}

positional_deviation_behavior_result <- function(bundle, behavior) {
  if (!behavior %in% names(bundle$behaviors)) stop("Behavior not in bundle: ", behavior, call. = FALSE)
  bundle$behaviors[[behavior]]
}
