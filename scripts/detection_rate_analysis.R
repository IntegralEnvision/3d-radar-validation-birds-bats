# ==============================================================================
# RADAR DETECTION-RATE ANALYSIS: ALL BEHAVIORS AND ALL CONFIGURED FLIGHTS
# ==============================================================================
#
# PURPOSE
# -------
# Estimate how consistently the radar detected the drone during every flight
# listed in the shared flight metadata registry.
#
# A detection opportunity is a COMPLETE bin interval during which:
#   1. drone positions cover the entire interval; and
#   2. the drone remains at least the configured distance horizontally from the physical radar.
#
# A qualifying interval is a detection when at least one time-corrected radar
# record occurs in it. Otherwise it is a gap. Multiple returns in one interval
# still count as one detected interval.
#
# Five bin durations are evaluated: 1, 2, 3, 4, and 5 seconds. Each duration is
# calculated twice:
#   * primary bins beginning at the first selected drone timestamp; and
#   * sensitivity bins shifted forward by half that duration.
#
# IMPORTANT: bins excluded by the configured cone-of-silence rule are removed from
# both the numerator and denominator. They also break consecutive gap events.
# Thus, two gaps separated by an excluded interval remain two distinct gaps.
#
# OUTPUTS
# -------
# Results remain available as data frames and are also saved in one consolidated RDS bundle.
#
#   flight_configuration
#   analysis_failures
#   bin_option_failures
#   bin_option_availability_summary
#   all_detection_bins
#   all_flight_detection_summary
#   all_gap_events
#   all_flight_gap_statistics
#   behavior_detection_summary
#   behavior_gap_summary
#   radar_type_detection_summary
#   radar_type_gap_summary
#   behavior_radar_detection_summary
#
# The script assumes that the working directory is the flight_analysis root.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(sf)
  library(stringr)
  library(tibble)
})


# ==============================================================================
# 1. ANALYSIS CONSTANTS
# ==============================================================================

source(file.path("R", "analysis_config.R"))
analysis_config <- load_analysis_config()

analysis_arguments <- commandArgs(trailingOnly = TRUE)
argument_value <- function(name, default) {
  prefix <- paste0("--", name, "=")
  match <- analysis_arguments[startsWith(analysis_arguments, prefix)]
  if (length(match) == 0L) return(default)
  sub(prefix, "", match[[1]], fixed = TRUE)
}

BIN_WIDTH_OPTIONS_SECONDS <- analysis_config$detection_rate$bin_widths_seconds
SHIFTED_BIN_FRACTION <- analysis_config$detection_rate$shifted_bin_fraction
# User-adjustable radar-specific cone-of-silence distances.
CONE_OF_SILENCE_METERS_BY_RADAR <- c(
  "9090" = as.numeric(argument_value("cone-9090-meters", as.character(analysis_config$detection_rate$cone_of_silence_m[["9090"]]))),
  "7360" = as.numeric(argument_value("cone-7360-meters", as.character(analysis_config$detection_rate$cone_of_silence_m[["7360"]])))
)
DETECTION_RATE_RESULTS_PATH <- argument_value(
  "results-file", analysis_config$detection_rate$results_file
)
if (any(!is.finite(CONE_OF_SILENCE_METERS_BY_RADAR)) ||
    any(CONE_OF_SILENCE_METERS_BY_RADAR <= 0)) {
  stop("Radar-specific cone distances must be positive numbers.", call. = FALSE)
}

# Evaluate the interpolated drone position every 0.1 second inside each bin,
# including both bin boundaries. This is more conservative than evaluating only
# the midpoint: if any evaluated time is inside the configured distance, the entire bin is
# excluded so all retained opportunities of a given width have equal duration.
DRONE_EVALUATION_STEP_SECONDS <- analysis_config$detection_rate$drone_evaluation_step_seconds

# All project flights occur in UTM zone 11N. Using a projected CRS makes the
# drone-to-device horizontal distance calculation directly interpretable in m.
PROJECTED_CRS <- analysis_config$projected_crs


# ==============================================================================
# 2. READ THE MASTER FLIGHT REGISTRY
# ==============================================================================

# The checked-in registry is the sole source of flight paths and configuration.
# The two analyses can select different inclusion flags while sharing metadata.
source(file.path("R", "flight_metadata.R"))
source(file.path("R", "detection_rate_bundle.R"))

behavior_order <- c("Foraging", "DynoSoaring", "Transiting", "Chasing", "Cube")

flight_configuration <- read_flight_metadata(path = project_path(analysis_config$metadata_file)) %>%
  filter(detection_rate_include) %>%
  arrange(factor(behavior, levels = behavior_order), flight_id)

# This check is intentional. If a flight is later added or removed, update the
# expected count after confirming that the change was deliberate.
EXPECTED_NUMBER_OF_FLIGHTS <- analysis_config$expected_number_of_flights
if (nrow(flight_configuration) != EXPECTED_NUMBER_OF_FLIGHTS) {
  stop(
    "Expected ", EXPECTED_NUMBER_OF_FLIGHTS,
    " configured flights but found ", nrow(flight_configuration), ".",
    call. = FALSE
  )
}

# ==============================================================================
# 3. READ AND PROJECT A SITE-SPECIFIC PHYSICAL RADAR LOCATION
# ==============================================================================

prepare_radar_location <- function(radar_location_file) {
  if (!file.exists(radar_location_file)) {
    stop(
      "Radar-location file not found: ",
      radar_location_file,
      call. = FALSE
    )
  }

  location_text <- readLines(radar_location_file, warn = FALSE)
  latitude_line <- location_text[
    str_detect(location_text, fixed("Latitude (deg):"))
  ][1]
  longitude_line <- location_text[
    str_detect(location_text, fixed("Longitude (deg):"))
  ][1]

  latitude <- as.numeric(
    str_trim(str_remove(latitude_line, fixed("Latitude (deg):")))
  )
  longitude <- as.numeric(
    str_trim(str_remove(longitude_line, fixed("Longitude (deg):")))
  )

  if (is.na(latitude) || is.na(longitude)) {
    stop(
      "Could not parse latitude/longitude from: ",
      radar_location_file,
      call. = FALSE
    )
  }

  radar_location_projected <- st_as_sf(
    tibble(
      longitude = longitude,
      latitude = latitude
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>%
    st_transform(PROJECTED_CRS)

  projected_coordinates <- st_coordinates(radar_location_projected)[1, ]

  list(
    longitude = longitude,
    latitude = latitude,
    x_m = unname(projected_coordinates[["X"]]),
    y_m = unname(projected_coordinates[["Y"]])
  )
}


# ==============================================================================
# 4. HELPER: PREPARE ONE DRONE TRACK
# ==============================================================================

prepare_drone_track <- function(
    drone_file,
    drone_indices,
    configured_start_time,
    configured_end_time) {

  if (!file.exists(drone_file)) {
    stop("Drone file not found: ", drone_file, call. = FALSE)
  }

  drone_raw <- read.csv(
    drone_file,
    stringsAsFactors = FALSE,
    check.names = TRUE
  )

  required_columns <- c(
    "datetime.utc.",
    "time.millisecond.",
    "longitude",
    "latitude"
  )
  missing_columns <- setdiff(required_columns, names(drone_raw))

  if (length(missing_columns) > 0) {
    stop(
      "Drone file is missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  valid_indices <- drone_indices[
    drone_indices >= 1 & drone_indices <= nrow(drone_raw)
  ]

  if (length(valid_indices) == 0) {
    stop("No configured drone indices occur in the drone file.", call. = FALSE)
  }

  drone_selected <- drone_raw[valid_indices, , drop = FALSE] %>%
    transmute(
      drone_source_row = valid_indices,
      timestamp_whole_second = as.POSIXct(
        datetime.utc.,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      subsecond_milliseconds =
        as.numeric(time.millisecond.) %% 1000,
      drone_timestamp =
        timestamp_whole_second + subsecond_milliseconds / 1000,
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude)
    ) %>%
    filter(
      !is.na(drone_timestamp),
      !is.na(longitude),
      !is.na(latitude),
      drone_timestamp >= configured_start_time,
      drone_timestamp <= configured_end_time
    ) %>%
    arrange(drone_timestamp)

  if (nrow(drone_selected) < 2) {
    stop("Fewer than two usable drone observations remain.", call. = FALSE)
  }

  # Convert each drone coordinate to the same projected coordinate system as the
  # physical radar, then retain ordinary numeric x/y columns for interpolation.
  drone_projected <- drone_selected %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) %>%
    st_transform(PROJECTED_CRS)

  projected_coordinates <- st_coordinates(drone_projected)

  drone_projected %>%
    st_drop_geometry() %>%
    mutate(
      drone_x_m = projected_coordinates[, 1],
      drone_y_m = projected_coordinates[, 2]
    ) %>%
    # Duplicate timestamps would make interpolation ambiguous. Averaging their
    # coordinates preserves the timestamp while providing one interpolation
    # point. Ordinarily each drone timestamp is already unique.
    group_by(drone_timestamp) %>%
    summarise(
      drone_x_m = mean(drone_x_m),
      drone_y_m = mean(drone_y_m),
      .groups = "drop"
    ) %>%
    arrange(drone_timestamp)
}


# ==============================================================================
# 5. HELPER: PREPARE AND TIME-CORRECT ONE RADAR TRACK
# ==============================================================================

prepare_radar_track <- function(
    radar_file,
    radar_type,
    offset_GPS,
    first_drone_timestamp,
    last_drone_timestamp) {

  if (!file.exists(radar_file)) {
    stop("Radar file not found: ", radar_file, call. = FALSE)
  }

  radar_raw <- read.csv(
    radar_file,
    stringsAsFactors = FALSE,
    check.names = TRUE
  )

  if (!("TrackId" %in% names(radar_raw))) {
    stop("Radar file is missing TrackId.", call. = FALSE)
  }

  # Current cleaned files use update_time_1. The UpdateTime alternative keeps
  # the method compatible with older matched files in this project.
  radar_time_column <- case_when(
    "update_time_1" %in% names(radar_raw) ~ "update_time_1",
    "UpdateTime" %in% names(radar_raw) ~ "UpdateTime",
    TRUE ~ NA_character_
  )

  if (is.na(radar_time_column)) {
    stop("Radar file has no recognized timestamp column.", call. = FALSE)
  }

  raw_time_text <- radar_raw[[radar_time_column]]

  # update_time_1 is month/day/year; UpdateTime is normally ISO year-month-day.
  timestamp_format <- if (radar_time_column == "update_time_1") {
    "%m/%d/%Y %H:%M:%OS"
  } else {
    "%Y-%m-%d %H:%M:%OS"
  }

  tibble(
    TrackId = as.character(radar_raw$TrackId),
    radar_timestamp_raw = as.POSIXct(
      raw_time_text,
      format = timestamp_format,
      tz = "UTC"
    )
  ) %>%
    mutate(
      # Match the established positional-deviation timestamp handling exactly:
      # corrected timestamp = raw radar timestamp - offset_GPS.
      radar_timestamp = radar_timestamp_raw - offset_GPS
    ) %>%
    filter(
      str_detect(TrackId, paste0("^A:", radar_type, ":")),
      !is.na(radar_timestamp),
      radar_timestamp >= first_drone_timestamp,
      radar_timestamp < last_drone_timestamp
    ) %>%
    arrange(radar_timestamp)
}


# ==============================================================================
# 6. HELPER: CALCULATE ONE BIN ALIGNMENT FOR ONE FLIGHT
# ==============================================================================

calculate_alignment <- function(
    behavior,
    flight_id,
    radar_type,
    radar_site,
    radar_location_file,
    radar_location,
    drone_track,
    radar_track,
    shift_seconds,
    bin_width_seconds,
    minimum_distance_m) {

  first_drone_timestamp <- min(drone_track$drone_timestamp)
  last_drone_timestamp <- max(drone_track$drone_timestamp)
  analysis_start <- first_drone_timestamp + shift_seconds

  coverage_seconds <- as.numeric(
    difftime(last_drone_timestamp, analysis_start, units = "secs")
  )
  n_complete_bins <- floor(coverage_seconds / bin_width_seconds)

  if (n_complete_bins < 1) {
    stop("Drone coverage is shorter than one complete bin.", call. = FALSE)
  }

  bins <- tibble(
    behavior = behavior,
    flight_id = flight_id,
    radar_type = radar_type,
    radar_site = radar_site,
    radar_location_file = radar_location_file,
    radar_longitude = radar_location$longitude,
    radar_latitude = radar_location$latitude,
    cone_of_silence_m = minimum_distance_m,
    bin_width_seconds = bin_width_seconds,
    bin_start_shift_seconds = shift_seconds,
    bin_id = seq_len(n_complete_bins),
    bin_start =
      analysis_start + (bin_id - 1) * bin_width_seconds,
    bin_end =
      analysis_start + bin_id * bin_width_seconds
  )
  analysis_end <- tail(bins$bin_end, 1)

  # Create evaluation times every 0.1 second across the selected bin width.
  # Both boundaries are included. Interpolated x/y positions are based only on
  # drone observations; radar detections play no role in cone eligibility.
  evaluation_fraction <- seq(
    0,
    bin_width_seconds,
    by = DRONE_EVALUATION_STEP_SECONDS
  )
  position_evaluation <- tibble(
    bin_id = rep(bins$bin_id, each = length(evaluation_fraction)),
    evaluation_time = rep(bins$bin_start, each = length(evaluation_fraction)) +
      rep(evaluation_fraction, times = nrow(bins))
  )

  drone_numeric_time <- as.numeric(drone_track$drone_timestamp)
  evaluation_numeric_time <- as.numeric(position_evaluation$evaluation_time)

  position_evaluation <- position_evaluation %>%
    mutate(
      drone_x_m = approx(
        x = drone_numeric_time,
        y = drone_track$drone_x_m,
        xout = evaluation_numeric_time,
        rule = 1,
        ties = mean
      )$y,
      drone_y_m = approx(
        x = drone_numeric_time,
        y = drone_track$drone_y_m,
        xout = evaluation_numeric_time,
        rule = 1,
        ties = mean
      )$y,
      drone_distance_to_radar_m = sqrt(
        (drone_x_m - radar_location$x_m)^2 +
          (drone_y_m - radar_location$y_m)^2
      )
    )

  bin_eligibility <- position_evaluation %>%
    group_by(bin_id) %>%
    summarise(
      minimum_drone_distance_m = if (all(is.na(drone_distance_to_radar_m))) {
        NA_real_
      } else {
        min(drone_distance_to_radar_m, na.rm = TRUE)
      },
      maximum_drone_distance_m = if (all(is.na(drone_distance_to_radar_m))) {
        NA_real_
      } else {
        max(drone_distance_to_radar_m, na.rm = TRUE)
      },
      has_complete_drone_support = all(!is.na(drone_distance_to_radar_m)),
      outside_cone_of_silence =
        has_complete_drone_support &&
        minimum_drone_distance_m >= minimum_distance_m,
      .groups = "drop"
    )

  radar_counts <- radar_track %>%
    filter(
      radar_timestamp >= analysis_start,
      radar_timestamp < analysis_end
    ) %>%
    mutate(
      bin_id = floor(
        as.numeric(difftime(
          radar_timestamp,
          analysis_start,
          units = "secs"
        )) / bin_width_seconds
      ) + 1L
    ) %>%
    filter(bin_id >= 1, bin_id <= n_complete_bins) %>%
    count(bin_id, name = "n_radar_records")

  bins <- bins %>%
    left_join(bin_eligibility, by = "bin_id") %>%
    left_join(radar_counts, by = "bin_id") %>%
    mutate(
      n_radar_records = coalesce(n_radar_records, 0L),
      eligible = has_complete_drone_support & outside_cone_of_silence,
      exclusion_reason = case_when(
        !has_complete_drone_support ~ "Incomplete drone position support",
        !outside_cone_of_silence ~ paste0("Drone within ", minimum_distance_m, " m cone of silence"),
        TRUE ~ NA_character_
      ),
      detected = eligible & n_radar_records > 0,
      detection_numeric = case_when(
        !eligible ~ NA_integer_,
        detected ~ 1L,
        TRUE ~ 0L
      ),
      bin_status = case_when(
        !eligible ~ "Excluded",
        detected ~ "Detection",
        TRUE ~ "Gap"
      )
    )

  eligible_bins <- bins %>% filter(eligible)
  n_eligible <- nrow(eligible_bins)

  if (n_eligible < 1) {
    stop("No bins remain after the ", minimum_distance_m, " m eligibility filter.", call. = FALSE)
  }

  n_detected <- sum(eligible_bins$detection_numeric == 1)
  n_gap <- sum(eligible_bins$detection_numeric == 0)

  detection_summary <- tibble(
    behavior = behavior,
    flight_id = flight_id,
    radar_type = radar_type,
    radar_site = radar_site,
    radar_location_file = radar_location_file,
    radar_longitude = radar_location$longitude,
    radar_latitude = radar_location$latitude,
    cone_of_silence_m = minimum_distance_m,
    bin_width_seconds = bin_width_seconds,
    bin_start_shift_seconds = shift_seconds,
    analysis_start = analysis_start,
    analysis_end = analysis_end,
    n_candidate_bins = nrow(bins),
    n_excluded_bins = sum(!bins$eligible),
    n_eligible_bins = n_eligible,
    n_detection_bins = n_detected,
    n_gap_bins = n_gap,
    detection_rate = n_detected / n_eligible,
    gap_rate = n_gap / n_eligible
  )

  # Run identifiers change when either detection status changes OR eligible bin
  # IDs cease to be consecutive. The latter condition prevents an excluded cone
  # interval from connecting otherwise separate gaps.
  eligible_runs <- eligible_bins %>%
    arrange(bin_id) %>%
    mutate(
      begins_new_run =
        row_number() == 1L |
        detection_numeric != lag(detection_numeric, default = first(detection_numeric)) |
        bin_id != lag(bin_id, default = first(bin_id) - 1L) + 1L,
      run_id = cumsum(begins_new_run)
    ) %>%
    group_by(run_id) %>%
    summarise(
      run_value = first(detection_numeric),
      start_bin_id = first(bin_id),
      end_bin_id = last(bin_id),
      n_bins = n(),
      .groups = "drop"
    )

  gap_events <- eligible_runs %>%
    filter(run_value == 0) %>%
    mutate(
      behavior = behavior,
      flight_id = flight_id,
      radar_type = radar_type,
      radar_site = radar_site,
      radar_location_file = radar_location_file,
      bin_width_seconds = bin_width_seconds,
      bin_start_shift_seconds = shift_seconds,
      gap_id = row_number(),
      gap_start = bins$bin_start[match(start_bin_id, bins$bin_id)],
      gap_end = bins$bin_end[match(end_bin_id, bins$bin_id)],
      gap_duration_seconds = n_bins * bin_width_seconds
    ) %>%
    select(
      behavior,
      flight_id,
      radar_type,
      radar_site,
      radar_location_file,
      bin_width_seconds,
      bin_start_shift_seconds,
      gap_id,
      gap_start,
      gap_end,
      start_bin_id,
      end_bin_id,
      n_gap_bins = n_bins,
      gap_duration_seconds
    )

  if (nrow(gap_events) == 0) {
    gap_statistics <- tibble(
      behavior = behavior,
      flight_id = flight_id,
      radar_type = radar_type,
      radar_site = radar_site,
      radar_location_file = radar_location_file,
      bin_width_seconds = bin_width_seconds,
      bin_start_shift_seconds = shift_seconds,
      n_gaps = 0L,
      total_gap_seconds = 0,
      minimum_gap_seconds = NA_real_,
      maximum_gap_seconds = NA_real_,
      median_gap_seconds = NA_real_,
      percentile_75_gap_seconds = NA_real_,
      percentile_95_gap_seconds = NA_real_,
      mean_gap_seconds = NA_real_,
      sd_gap_seconds = NA_real_
    )
  } else {
    gap_statistics <- gap_events %>%
      summarise(
        behavior = first(behavior),
        flight_id = first(flight_id),
        radar_type = first(radar_type),
        radar_site = first(radar_site),
        radar_location_file = first(radar_location_file),
        bin_width_seconds = first(bin_width_seconds),
        bin_start_shift_seconds = first(bin_start_shift_seconds),
        n_gaps = n(),
        total_gap_seconds = sum(gap_duration_seconds),
        minimum_gap_seconds = min(gap_duration_seconds),
        maximum_gap_seconds = max(gap_duration_seconds),
        median_gap_seconds = median(gap_duration_seconds),
        percentile_75_gap_seconds = as.numeric(
          quantile(gap_duration_seconds, 0.75, names = FALSE)
        ),
        percentile_95_gap_seconds = as.numeric(
          quantile(gap_duration_seconds, 0.95, names = FALSE)
        ),
        mean_gap_seconds = mean(gap_duration_seconds),
        sd_gap_seconds = sd(gap_duration_seconds)
      )
  }

  # Fail immediately if future edits violate the denominator, bin-width, or gap
  # identities on which the method depends.
  stopifnot(
    all(abs(as.numeric(difftime(
      bins$bin_end,
      bins$bin_start,
      units = "secs"
    )) - bin_width_seconds) < 1e-9),
    tail(bins$bin_end, 1) <= last_drone_timestamp,
    n_detected + n_gap == n_eligible,
    sum(gap_events$n_gap_bins) == n_gap,
    all(bins$minimum_drone_distance_m[bins$eligible] >= minimum_distance_m),
    abs(detection_summary$detection_rate +
      detection_summary$gap_rate - 1) < 1e-12
  )

  list(
    detection_bins = bins,
    detection_summary = detection_summary,
    gap_events = gap_events,
    gap_statistics = gap_statistics
  )
}


# ==============================================================================
# 7. HELPER: RUN BOTH ALIGNMENTS FOR ONE CONFIGURED FLIGHT
# ==============================================================================

analyze_configured_flight <- function(configuration_row) {
  behavior <- configuration_row$behavior[[1]]
  flight_id <- configuration_row$flight_id[[1]]
  radar_type <- configuration_row$radar_type[[1]]
  radar_site <- configuration_row$radar_site[[1]]
  radar_location_file <- configuration_row$radar_location_file[[1]]
  minimum_distance_m <- unname(CONE_OF_SILENCE_METERS_BY_RADAR[[radar_type]])
  if (is.null(minimum_distance_m)) stop("No cone distance configured for radar type: ", radar_type, call. = FALSE)

  message("Analyzing ", behavior, " / ", flight_id, "...")

  drone_track <- prepare_drone_track(
    drone_file = configuration_row$drone_file[[1]],
    drone_indices = configuration_row$drone_indices[[1]],
    configured_start_time = configuration_row$configured_start_time[[1]],
    configured_end_time = configuration_row$configured_end_time[[1]]
  )

  radar_track <- prepare_radar_track(
    radar_file = configuration_row$radar_file[[1]],
    radar_type = radar_type,
    offset_GPS = configuration_row$offset_GPS[[1]],
    first_drone_timestamp = min(drone_track$drone_timestamp),
    last_drone_timestamp = max(drone_track$drone_timestamp)
  )

  # Read the physical radar position associated with this flight's study site.
  # This is deliberately independent of the radar detections themselves.
  radar_location <- prepare_radar_location(radar_location_file)

  # Every duration receives an unshifted analysis and a half-bin shift. For
  # example, 2-second bins use shifts of 0 and 1 second; 5-second bins use
  # shifts of 0 and 2.5 seconds.
  alignment_options <- map_dfr(
    BIN_WIDTH_OPTIONS_SECONDS,
    function(bin_width_seconds) {
      tibble(
        bin_width_seconds = bin_width_seconds,
        shift_seconds = c(0, bin_width_seconds * SHIFTED_BIN_FRACTION)
      )
    }
  )

  alignment_results <- vector("list", nrow(alignment_options))
  alignment_failures <- vector("list", nrow(alignment_options))

  for (option_index in seq_len(nrow(alignment_options))) {
    selected_width <- alignment_options$bin_width_seconds[[option_index]]
    selected_shift <- alignment_options$shift_seconds[[option_index]]

    alignment_results[[option_index]] <- tryCatch(
      calculate_alignment(
        behavior = behavior,
        flight_id = flight_id,
        radar_type = radar_type,
        radar_site = radar_site,
        radar_location_file = radar_location_file,
        radar_location = radar_location,
        drone_track = drone_track,
        radar_track = radar_track,
        shift_seconds = selected_shift,
        bin_width_seconds = selected_width,
        minimum_distance_m = minimum_distance_m
      ),
      error = function(error_condition) {
        # Long bins are legitimately unavailable when a short flight cannot
        # supply one complete interval. Record that combination explicitly
        # without discarding the flight's shorter-bin results.
        alignment_failures[[option_index]] <<- tibble(
          behavior = behavior,
          flight_id = flight_id,
          radar_type = radar_type,
          radar_site = radar_site,
          radar_location_file = radar_location_file,
          bin_width_seconds = selected_width,
          bin_start_shift_seconds = selected_shift,
          error_message = conditionMessage(error_condition)
        )
        NULL
      }
    )
  }

  successful_alignments <- compact(alignment_results)

  list(
    detection_bins = map_dfr(successful_alignments, "detection_bins"),
    detection_summary = map_dfr(successful_alignments, "detection_summary"),
    gap_events = map_dfr(successful_alignments, "gap_events"),
    gap_statistics = map_dfr(successful_alignments, "gap_statistics"),
    alignment_failures = bind_rows(compact(alignment_failures))
  )
}


# ==============================================================================
# 8. RUN ALL 85 FLIGHTS WITHOUT LOSING SUCCESSFUL RESULTS IF ONE FLIGHT FAILS
# ==============================================================================

# Each failure is captured with its flight identity and error message. This is
# preferable to silently skipping a flight, and it allows successful flights to
# remain available for diagnosis. A final warning makes incomplete analysis
# impossible to overlook.
flight_results <- vector("list", nrow(flight_configuration))
failure_records <- vector("list", nrow(flight_configuration))

for (flight_index in seq_len(nrow(flight_configuration))) {
  one_configuration <- flight_configuration[
    flight_index,
    ,
    drop = FALSE
  ]

  flight_results[[flight_index]] <- tryCatch(
    analyze_configured_flight(one_configuration),
    error = function(error_condition) {
      failure_records[[flight_index]] <<- tibble(
        behavior = one_configuration$behavior[[1]],
        flight_id = one_configuration$flight_id[[1]],
        radar_type = one_configuration$radar_type[[1]],
        radar_file = one_configuration$radar_file[[1]],
        drone_file = one_configuration$drone_file[[1]],
        error_message = conditionMessage(error_condition)
      )
      NULL
    }
  )
}

analysis_failures <- bind_rows(compact(failure_records))
successful_flight_results <- compact(flight_results)

if (length(successful_flight_results) == 0) {
  stop("Every configured flight failed; no summaries can be calculated.")
}

all_detection_bins <- map_dfr(
  successful_flight_results,
  "detection_bins"
)
all_flight_detection_summary <- map_dfr(
  successful_flight_results,
  "detection_summary"
)
all_gap_events <- map_dfr(
  successful_flight_results,
  "gap_events"
)
all_flight_gap_statistics <- map_dfr(
  successful_flight_results,
  "gap_statistics"
)
bin_option_failures <- map_dfr(
  successful_flight_results,
  "alignment_failures"
)

# If every requested bin/alignment combination succeeds, map_dfr() returns an
# empty tibble with no columns. Preserve the expected failure-table schema so
# the availability summary below can still count and join on these fields.
if (ncol(bin_option_failures) == 0) {
  bin_option_failures <- tibble(
    behavior = character(),
    flight_id = character(),
    radar_type = character(),
    radar_site = character(),
    radar_location_file = character(),
    bin_width_seconds = numeric(),
    bin_start_shift_seconds = numeric(),
    error_message = character()
  )
}

# Show how many flights contributed to every duration/alignment combination.
# This reports how many flights contributed to each configured duration and
# alignment, including the all-successful case.
all_bin_options <- map_dfr(
  BIN_WIDTH_OPTIONS_SECONDS,
  function(bin_width_seconds) {
    tibble(
      bin_width_seconds = bin_width_seconds,
      bin_start_shift_seconds = c(0, bin_width_seconds * SHIFTED_BIN_FRACTION)
    )
  }
)

bin_option_availability_summary <- all_bin_options %>%
  left_join(
    all_flight_detection_summary %>%
      count(
        bin_width_seconds,
        bin_start_shift_seconds,
        name = "n_available_flights"
      ),
    by = c("bin_width_seconds", "bin_start_shift_seconds")
  ) %>%
  left_join(
    bin_option_failures %>%
      count(
        bin_width_seconds,
        bin_start_shift_seconds,
        name = "n_unavailable_flights"
      ),
    by = c("bin_width_seconds", "bin_start_shift_seconds")
  ) %>%
  mutate(
    n_available_flights = coalesce(n_available_flights, 0L),
    n_unavailable_flights = coalesce(n_unavailable_flights, 0L)
  )


# ==============================================================================
# 9. SUMMARIZE DETECTION RATE BY BEHAVIOR TYPE
# ==============================================================================

# pooled_detection_rate answers:
# "Across all eligible seconds belonging to this behavior, what proportion
# contained a detection?"
#
# mean_flight_detection_rate gives every flight equal weight instead. Reporting
# both makes clear whether long flights dominate the pooled result.
behavior_detection_summary <- all_flight_detection_summary %>%
  group_by(behavior, bin_width_seconds, bin_start_shift_seconds) %>%
  summarise(
    n_flights = n_distinct(flight_id),
    n_candidate_bins = sum(n_candidate_bins),
    n_excluded_bins = sum(n_excluded_bins),
    n_eligible_bins = sum(n_eligible_bins),
    n_detection_bins = sum(n_detection_bins),
    n_gap_bins = sum(n_gap_bins),
    pooled_detection_rate = n_detection_bins / n_eligible_bins,
    mean_flight_detection_rate = mean(detection_rate),
    sd_flight_detection_rate = sd(detection_rate),
    median_flight_detection_rate = median(detection_rate),
    minimum_flight_detection_rate = min(detection_rate),
    maximum_flight_detection_rate = max(detection_rate),
    .groups = "drop"
  )

behavior_gap_summary <- all_gap_events %>%
  group_by(behavior, bin_width_seconds, bin_start_shift_seconds) %>%
  summarise(
    n_flights_with_gaps = n_distinct(flight_id),
    n_gaps = n(),
    total_gap_seconds = sum(gap_duration_seconds),
    minimum_gap_seconds = min(gap_duration_seconds),
    maximum_gap_seconds = max(gap_duration_seconds),
    median_gap_seconds = median(gap_duration_seconds),
    percentile_75_gap_seconds = as.numeric(
      quantile(gap_duration_seconds, 0.75, names = FALSE)
    ),
    percentile_95_gap_seconds = as.numeric(
      quantile(gap_duration_seconds, 0.95, names = FALSE)
    ),
    mean_gap_seconds = mean(gap_duration_seconds),
    sd_gap_seconds = sd(gap_duration_seconds),
    .groups = "drop"
  )


# ==============================================================================
# 10. SUMMARIZE DETECTION RATE BY RADAR TYPE
# ==============================================================================

radar_type_detection_summary <- all_flight_detection_summary %>%
  group_by(radar_type, bin_width_seconds, bin_start_shift_seconds) %>%
  summarise(
    n_flights = n_distinct(flight_id),
    n_candidate_bins = sum(n_candidate_bins),
    n_excluded_bins = sum(n_excluded_bins),
    n_eligible_bins = sum(n_eligible_bins),
    n_detection_bins = sum(n_detection_bins),
    n_gap_bins = sum(n_gap_bins),
    pooled_detection_rate = n_detection_bins / n_eligible_bins,
    mean_flight_detection_rate = mean(detection_rate),
    sd_flight_detection_rate = sd(detection_rate),
    median_flight_detection_rate = median(detection_rate),
    minimum_flight_detection_rate = min(detection_rate),
    maximum_flight_detection_rate = max(detection_rate),
    .groups = "drop"
  )

radar_type_gap_summary <- all_gap_events %>%
  group_by(radar_type, bin_width_seconds, bin_start_shift_seconds) %>%
  summarise(
    n_flights_with_gaps = n_distinct(flight_id),
    n_gaps = n(),
    total_gap_seconds = sum(gap_duration_seconds),
    minimum_gap_seconds = min(gap_duration_seconds),
    maximum_gap_seconds = max(gap_duration_seconds),
    median_gap_seconds = median(gap_duration_seconds),
    percentile_75_gap_seconds = as.numeric(
      quantile(gap_duration_seconds, 0.75, names = FALSE)
    ),
    percentile_95_gap_seconds = as.numeric(
      quantile(gap_duration_seconds, 0.95, names = FALSE)
    ),
    mean_gap_seconds = mean(gap_duration_seconds),
    sd_gap_seconds = sd(gap_duration_seconds),
    .groups = "drop"
  )

# This cross-tabulation is important because radar type and behavior are not
# fully crossed in the current design. In particular, Cube flights use 7360,
# while the other configured behaviors use 9090. A raw radar-type difference
# may therefore also reflect a behavior difference.
behavior_radar_detection_summary <- all_flight_detection_summary %>%
  group_by(
    behavior,
    radar_type,
    bin_width_seconds,
    bin_start_shift_seconds
  ) %>%
  summarise(
    n_flights = n_distinct(flight_id),
    n_eligible_bins = sum(n_eligible_bins),
    n_detection_bins = sum(n_detection_bins),
    n_gap_bins = sum(n_gap_bins),
    pooled_detection_rate = n_detection_bins / n_eligible_bins,
    mean_flight_detection_rate = mean(detection_rate),
    sd_flight_detection_rate = sd(detection_rate),
    .groups = "drop"
  )


# ==============================================================================
# 11. FINAL QUALITY CONTROL AND CONSOLE OUTPUT
# ==============================================================================

n_successful_flights <- n_distinct(all_flight_detection_summary$flight_id)
n_failed_flights <- nrow(analysis_failures)

stopifnot(
  nrow(all_flight_detection_summary) +
    nrow(bin_option_failures) +
    n_failed_flights * length(BIN_WIDTH_OPTIONS_SECONDS) * 2 ==
    nrow(flight_configuration) * length(BIN_WIDTH_OPTIONS_SECONDS) * 2,
  nrow(all_flight_detection_summary) ==
    nrow(distinct(
      all_flight_detection_summary,
      flight_id,
      bin_width_seconds,
      bin_start_shift_seconds
    )),
  all(
    all_flight_detection_summary$n_detection_bins +
      all_flight_detection_summary$n_gap_bins ==
      all_flight_detection_summary$n_eligible_bins
  ),
  all(
    all_flight_detection_summary$n_candidate_bins ==
      all_flight_detection_summary$n_excluded_bins +
      all_flight_detection_summary$n_eligible_bins
  ),
  all(
    all_detection_bins$minimum_drone_distance_m[
      all_detection_bins$eligible
    ] >= all_detection_bins$cone_of_silence_m[
      all_detection_bins$eligible
    ]
  )
)

message("")
message("Detection-rate analysis finished.")
message("Configured flights: ", nrow(flight_configuration))
message("Successful flights: ", n_successful_flights)
message("Failed flights: ", n_failed_flights)
message(
  "Unavailable flight/bin/alignment combinations: ",
  nrow(bin_option_failures)
)
message(
  "Eligible bins use radar-specific thresholds: ",
  paste(names(CONE_OF_SILENCE_METERS_BY_RADAR), CONE_OF_SILENCE_METERS_BY_RADAR, "m", collapse = "; "),
  "."
)

if (n_failed_flights > 0) {
  warning(
    n_failed_flights,
    " flight(s) failed. Inspect analysis_failures before using summaries.",
    call. = FALSE
  )
  print(analysis_failures)
}

message("")
message("Primary behavior summaries (all unshifted bin durations):")
print(
  behavior_detection_summary %>%
    filter(bin_start_shift_seconds == 0)
)

message("")
message("Primary radar-type summaries (all unshifted bin durations):")
print(
  radar_type_detection_summary %>%
    filter(bin_start_shift_seconds == 0)
)

# Useful interactive commands:
#
# View(flight_configuration)
# View(analysis_failures)
# View(bin_option_failures)
# # View(bin_option_availability_summary)
# View(all_flight_detection_summary)
# View(behavior_detection_summary)
# View(behavior_gap_summary)
# View(radar_type_detection_summary)
# View(radar_type_gap_summary)
# View(behavior_radar_detection_summary)
# View(all_detection_bins %>% filter(!eligible))
# ==============================================================================

# Save one consolidated intermediate bundle before creating figures.
write_detection_rate_bundle(path = DETECTION_RATE_RESULTS_PATH)
message("Saved consolidated detection-rate results: ", DETECTION_RATE_RESULTS_PATH)

# Create diagnostic figures and the radar-type/bin-length summary table.
source('scripts/detection_rate_figures.R')
