# Reusable all-behavior pipeline for the positional-deviation analysis only.

positional_deviation_behavior_configuration <- data.frame(
  metadata_behavior = c("Foraging", "DynoSoaring", "Transiting", "Chasing", "Cube"),
  result_key = c("foraging", "soaring", "transiting", "chasing", "cubes"),
  analysis_label = c("Foraging", "Soaring", "Transiting", "Chasing", "Cubes"),
  stringsAsFactors = FALSE
)

run_positional_deviation_behavior <- function(
    metadata_behavior,
    metadata = read_flight_metadata(),
    radar_y_offsets_by_type = c("9090" = 20, "7360" = 0)) {
  config <- positional_deviation_behavior_configuration[
    positional_deviation_behavior_configuration$metadata_behavior == metadata_behavior,
    , drop = FALSE
  ]
  if (nrow(config) != 1L) stop("Unknown positional-deviation behavior: ", metadata_behavior, call. = FALSE)
  flights <- metadata[
    metadata$behavior == metadata_behavior & metadata$positional_deviation_include,
    , drop = FALSE
  ]
  if (!nrow(flights)) stop("No included positional-deviation flights for: ", metadata_behavior, call. = FALSE)
  root <- find_project_root()
  flight_results <- vector("list", nrow(flights))
  display_times <- character(nrow(flights))
  for (i in seq_len(nrow(flights))) {
    row <- flights[i, , drop = FALSE]
    local_start <- as.POSIXct(row$configured_start_time[[1]], tz = "UTC")
    attr(local_start, "tzone") <- "America/Los_Angeles"
    time_digits <- sub("^.*_", "", row$radar_object[[1]])
    meridiem <- if (as.integer(format(local_start, "%H", tz = "America/Los_Angeles")) < 12) "AM" else "PM"
    display_times[[i]] <- paste0(substr(time_digits, 1, 2), "-", substr(time_digits, 3, 4), meridiem)
    message("Positional deviation: ", row$flight_id[[1]], "...")
    flight_results[[i]] <- analyze_positional_deviation_flight(
      radar_df = read.csv(file.path(root, row$radar_file[[1]])),
      drone_df = read.csv(file.path(root, row$drone_file[[1]])),
      offset_GPS = row$offset_GPS[[1]],
      radar_y_offset = unname(radar_y_offsets_by_type[[row$radar_type[[1]]]]),
      foraging_id = as.character(as.integer(sub("^.*_", "", row$flight_id[[1]]))),
      foraging_date = format(local_start, "%Y-%m-%d", tz = "America/Los_Angeles"),
      foraging_time = display_times[[i]],
      drone_indices = row$drone_indices[[1]],
      start_time_str = format(row$configured_start_time[[1]], "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
      end_time_str = format(row$configured_end_time[[1]], "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
      radar_type = row$radar_type[[1]]
    )
  }
  paired_data <- dplyr::bind_rows(lapply(flight_results, `[[`, "paired_data")) |>
    dplyr::mutate(
      behavior = config$analysis_label[[1]],
      unique_flight = paste(config$analysis_label[[1]], flight_id, sep = "_"),
      error_sqrt = sqrt(euclidean_distance)
    )
  rmse <- dplyr::bind_rows(lapply(seq_along(flight_results), function(i) {
    result <- flight_results[[i]]
    data.frame(
      behavior = config$analysis_label[[1]],
      flight = paste("Flight", i),
      date = result$flight_info$date,
      time = result$flight_info$time,
      measure = result$rmse_data$measure,
      RMSE = result$rmse_data$RMSE,
      stringsAsFactors = FALSE
    )
  }))
  list(
    result_key = config$result_key[[1]],
    paired_data = paired_data,
    rmse = rmse,
    flight_metadata = flights
  )
}

run_positional_deviation_analysis <- function(
    behaviors = positional_deviation_behavior_configuration$metadata_behavior,
    metadata_path = project_path("metadata", "flights.csv"),
    radar_y_offsets_by_type = c("9090" = 20, "7360" = 0),
    results_path = positional_deviation_bundle_path()) {
  metadata <- read_flight_metadata(path = metadata_path)
  selected <- lapply(behaviors, run_positional_deviation_behavior,
                     metadata = metadata, radar_y_offsets_by_type = radar_y_offsets_by_type)
  bundle <- new_positional_deviation_bundle()
  for (result in selected) {
    bundle$behaviors[[result$result_key]] <- list(
      paired_data = result$paired_data,
      rmse = result$rmse,
      flight_metadata = result$flight_metadata,
      updated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  }
  bundle$updated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  validate_positional_deviation_bundle(bundle, require_all_behaviors = length(behaviors) == 5L)
  dir.create(dirname(results_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, results_path)
  saved <- readRDS(results_path)
  validate_positional_deviation_bundle(saved, require_all_behaviors = length(behaviors) == 5L)
  invisible(saved)
}
