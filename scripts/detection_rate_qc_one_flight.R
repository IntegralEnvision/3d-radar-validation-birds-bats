# ============================================================================
# INDEPENDENT ONE-FLIGHT DETECTION-RATE QUALITY CONTROL
# ============================================================================
# Run this file one section at a time. This script intentionally DOES NOT source
# detection_rate_analysis.R and DOES NOT call any of its helper functions.
# Every methodological calculation is written out below so its inputs and
# intermediary products can be inspected directly.
#
# Suggested cases spanning all behaviors, both radars/sites, all bin widths,
# and both alignment types:
#   Flight           Width (s)   Shift (s)
#   Foraging_01          1          0
#   DynoSoaring_01       2          1
#   Transiting_01        3          0
#   Chasing_01           4          2
#   Cube_01              5          0
#
# Friendly flight IDs such as "foraging_1" are accepted.
# Step by step process:
# 1. Read the selected flight’s metadata.
# 2. Read and prepare drone observations.
# 3. Read radar data and apply the GPS time correction.
# 4. Read and project the physical radar location.
# 5. Construct complete bins.
# 6. Interpolate drone positions every 0.1 seconds.
# 7. Apply drone-support and radar-specific eligibility rules.
# 8. Assign radar records to bins.
# 9. Classify detections/gaps and identify gap events.
# 10. Calculate final products and compare them with the main analysis.

library(dplyr)
library(stringr)
library(sf)
library(tibble)

source(file.path("R", "analysis_config.R"))
source(file.path("R", "flight_metadata.R"))
analysis_config <- load_analysis_config()
source(file.path("R", "detection_rate_bundle.R"))
if (file.exists(project_path(analysis_config$detection_rate$results_file))) {
  load_detection_rate_results(
    read_detection_rate_bundle(project_path(analysis_config$detection_rate$results_file))
  )
}


# ----------------------------------------------------------------------------
# USER SETTINGS: edit these three values, then run downward section by section
# ----------------------------------------------------------------------------
# PASSED TESTS
QC_FLIGHT_ID <- "Foraging_01"
QC_BIN_WIDTH_SECONDS <- 1
QC_SHIFT_SECONDS <- 0 #Shift is the sensitivity validation
#
# QC_FLIGHT_ID <- "DynoSoaring_01"
# QC_BIN_WIDTH_SECONDS <- 2
# QC_SHIFT_SECONDS <- 1 #Shift is the sensitivity validation

# QC_FLIGHT_ID <- "Transiting_01"
# QC_BIN_WIDTH_SECONDS <- 3
# QC_SHIFT_SECONDS <- 0 #Shift is the sensitivity validation

# QC_FLIGHT_ID <- "Chasing_01"
# QC_BIN_WIDTH_SECONDS <- 4
# QC_SHIFT_SECONDS <- 2 #Shift is the sensitivity validation

# FLIGHT_ID <- "Cube_01"
# QC_BIN_WIDTH_SECONDS <- 5
# QC_SHIFT_SECONDS <- 0 #Shift is the sensitivity validation

# QC_FLIGHT_ID <- "Foraging_03"
# QC_BIN_WIDTH_SECONDS <- 1
# QC_SHIFT_SECONDS <- 0 #Shift is the sensitivity validation



METADATA_CSV <- project_path(analysis_config$metadata_file)
CONE_OF_SILENCE_METERS_BY_RADAR <- analysis_config$detection_rate$cone_of_silence_m
DRONE_EVALUATION_STEP_SECONDS <- analysis_config$detection_rate$drone_evaluation_step_seconds
PROJECTED_CRS <- analysis_config$projected_crs
COMPARISON_TOLERANCE <- 1e-12

if (!(QC_BIN_WIDTH_SECONDS %in% analysis_config$detection_rate$bin_widths_seconds)) {
  stop("QC_BIN_WIDTH_SECONDS must be 1, 2, 3, 4, or 5.")
}
if (!(QC_SHIFT_SECONDS %in% c(0, QC_BIN_WIDTH_SECONDS * analysis_config$detection_rate$shifted_bin_fraction))) {
  stop("QC_SHIFT_SECONDS must be zero or half of QC_BIN_WIDTH_SECONDS.")
}


# ============================================================================
# STEP 1: READ AND INSPECT THE SELECTED RUNNER PARAMETERS
# ============================================================================
# The metadata CSV is deliberately separate from this script so it can be
# reviewed independently before any raw flight data are processed.
if (!file.exists(METADATA_CSV)) {
  stop("Metadata CSV was not found: ", METADATA_CSV)
}

qc_metadata <- read_flight_metadata(path = METADATA_CSV, check_files = TRUE)

normalize_flight_id <- function(x) {
  x <- trimws(x)
  pieces <- regmatches(x, regexec("^(.+)_([0-9]+)$", x))[[1]]
  if (length(pieces) == 3) {
    paste0(pieces[[2]], "_", sprintf("%02d", as.integer(pieces[[3]])))
  } else {
    x
  }
}

requested_flight_id <- normalize_flight_id(QC_FLIGHT_ID)
qc_configuration <- qc_metadata |>
  filter(tolower(flight_id) == tolower(requested_flight_id))

if (nrow(qc_configuration) != 1) {
  stop("QC_FLIGHT_ID did not identify exactly one metadata row.")
}
CONE_OF_SILENCE_METERS <- unname(
  CONE_OF_SILENCE_METERS_BY_RADAR[[qc_configuration$radar_type[[1]]]]
)
if (is.null(CONE_OF_SILENCE_METERS)) {
  stop("No cone distance configured for radar type: ", qc_configuration$radar_type[[1]])
}

qc_drone_indices <- qc_configuration$drone_indices[[1]]
qc_configured_start <- qc_configuration$configured_start_time[[1]]
qc_configured_end <- qc_configuration$configured_end_time[[1]]

print(qc_configuration)
summary(qc_drone_indices)
# View(qc_configuration)


# ============================================================================
# STEP 2: READ AND PREPARE THE SELECTED DRONE OBSERVATIONS
# ============================================================================
qc_drone_raw <- read.csv(
  qc_configuration$drone_file[[1]],
  stringsAsFactors = FALSE,
  check.names = TRUE
)

qc_required_drone_columns <- c(
  "datetime.utc.",
  "time.millisecond.",
  "longitude",
  "latitude"
)
qc_missing_drone_columns <- setdiff(
  qc_required_drone_columns,
  names(qc_drone_raw)
)
if (length(qc_missing_drone_columns) > 0) {
  stop("Drone columns are missing: ", paste(qc_missing_drone_columns, collapse = ", "))
}

qc_valid_drone_indices <- qc_drone_indices[
  qc_drone_indices >= 1 & qc_drone_indices <= nrow(qc_drone_raw)
]
if (length(qc_valid_drone_indices) == 0) {
  stop("None of the configured drone indices occur in the drone file.")
}

qc_drone_selected <- qc_drone_raw[
  qc_valid_drone_indices,
  ,
  drop = FALSE
] |>
  transmute(
    drone_source_row = qc_valid_drone_indices,
    timestamp_whole_second = as.POSIXct(
      datetime.utc.,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    subsecond_milliseconds = as.numeric(time.millisecond.) %% 1000,
    drone_timestamp = timestamp_whole_second + subsecond_milliseconds / 1000,
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) |>
  filter(
    !is.na(drone_timestamp),
    !is.na(longitude),
    !is.na(latitude),
    drone_timestamp >= qc_configured_start,
    drone_timestamp <= qc_configured_end
  ) |>
  arrange(drone_timestamp)

if (nrow(qc_drone_selected) < 2) {
  stop("Fewer than two usable drone observations remain.")
}

qc_drone_sf <- qc_drone_selected |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_transform(PROJECTED_CRS)

qc_drone_xy <- st_coordinates(qc_drone_sf)
qc_drone_track <- qc_drone_sf |>
  st_drop_geometry() |>
  mutate(
    drone_x_m = qc_drone_xy[, 1],
    drone_y_m = qc_drone_xy[, 2]
  ) |>
  group_by(drone_timestamp) |>
  summarise(
    drone_x_m = mean(drone_x_m),
    drone_y_m = mean(drone_y_m),
    .groups = "drop"
  ) |>
  arrange(drone_timestamp)

print(qc_drone_track)
summary(qc_drone_track)
# View(qc_drone_selected)
# View(qc_drone_track)


# ============================================================================
# STEP 3: READ THE RADAR DATA, CORRECT TIME, AND RETAIN THE SELECTED TRACK TYPE
# ============================================================================
qc_radar_raw <- read.csv(
  qc_configuration$radar_file[[1]],
  stringsAsFactors = FALSE,
  check.names = TRUE
)
if (!("TrackId" %in% names(qc_radar_raw))) {
  stop("Radar file is missing TrackId.")
}

qc_radar_time_column <- if ("update_time_1" %in% names(qc_radar_raw)) {
  "update_time_1"
} else if ("UpdateTime" %in% names(qc_radar_raw)) {
  "UpdateTime"
} else {
  stop("Radar file has no recognized timestamp column.")
}
qc_radar_time_format <- if (qc_radar_time_column == "update_time_1") {
  "%m/%d/%Y %H:%M:%OS"
} else {
  "%Y-%m-%d %H:%M:%OS"
}

qc_radar_track <- tibble(
  TrackId = as.character(qc_radar_raw$TrackId),
  radar_timestamp_raw = as.POSIXct(
    qc_radar_raw[[qc_radar_time_column]],
    format = qc_radar_time_format,
    tz = "UTC"
  )
) |>
  mutate(
    # GPS correction used by the main method:
    # corrected radar time = raw radar time - offset_GPS
    radar_timestamp = radar_timestamp_raw - qc_configuration$offset_GPS[[1]]
  ) |>
  filter(
    str_detect(
      TrackId,
      paste0("^A:", qc_configuration$radar_type[[1]], ":")
    ),
    !is.na(radar_timestamp),
    radar_timestamp >= min(qc_drone_track$drone_timestamp),
    radar_timestamp < max(qc_drone_track$drone_timestamp)
  ) |>
  arrange(radar_timestamp)

print(qc_radar_track)
summary(qc_radar_track)
# View(qc_radar_track)

# Check with original
#qc_radar_raw_sorted <- qc_radar_raw |> arrange(as.POSIXct(update_time_1), format ="%m/%d/%Y %H:%M:%OS", tz = "UTC")
qc_radar_raw_aligned <- qc_radar_raw |>
  mutate(radar_timestamp_raw = as.POSIXct(update_time_1, format = "%m/%d/%Y %H:%M:%OS", tz = "UTC"),
         radar_timestamp = radar_timestamp_raw - qc_configuration$offset_GPS[[1]]) |>
  filter(str_detect(as.character(TrackId), paste0("^A:", qc_configuration$radar_type[[1]], ":")), !is.na(radar_timestamp),
         radar_timestamp >= min(qc_drone_track$drone_timestamp), radar_timestamp < max(qc_drone_track$drone_timestamp)) |>
  arrange(radar_timestamp)

# ============================================================================
# STEP 4: READ AND PROJECT THE PHYSICAL RADAR LOCATION
# ============================================================================
qc_location_text <- readLines(
  qc_configuration$radar_location_file[[1]],
  warn = FALSE
)
qc_latitude_line <- qc_location_text[
  str_detect(qc_location_text, fixed("Latitude (deg):"))
][1]
qc_longitude_line <- qc_location_text[
  str_detect(qc_location_text, fixed("Longitude (deg):"))
][1]

qc_radar_latitude <- as.numeric(
  str_trim(str_remove(qc_latitude_line, fixed("Latitude (deg):")))
)
qc_radar_longitude <- as.numeric(
  str_trim(str_remove(qc_longitude_line, fixed("Longitude (deg):")))
)
if (is.na(qc_radar_latitude) || is.na(qc_radar_longitude)) {
  stop("Could not parse the physical radar latitude/longitude.")
}

qc_radar_location_sf <- tibble(
  longitude = qc_radar_longitude,
  latitude = qc_radar_latitude
) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(PROJECTED_CRS)
qc_radar_xy <- st_coordinates(qc_radar_location_sf)[1, ]
qc_radar_location <- tibble(
  longitude = qc_radar_longitude,
  latitude = qc_radar_latitude,
  radar_x_m = unname(qc_radar_xy[["X"]]),
  radar_y_m = unname(qc_radar_xy[["Y"]])
)

print(qc_radar_location)


# ============================================================================
# STEP 5: CONSTRUCT COMPLETE BINS FOR THE CHOSEN WIDTH AND ALIGNMENT
# ============================================================================

qc_first_drone_timestamp <- min(qc_drone_track$drone_timestamp)
qc_last_drone_timestamp <- max(qc_drone_track$drone_timestamp)
qc_analysis_start <- qc_first_drone_timestamp + QC_SHIFT_SECONDS #QC_SHIFT SECONDS IS THE SENSITIVITY ANALYSIS
qc_coverage_seconds <- as.numeric(
  difftime(qc_last_drone_timestamp, qc_analysis_start, units = "secs")
)
qc_number_complete_bins <- floor(qc_coverage_seconds / QC_BIN_WIDTH_SECONDS)
if (qc_number_complete_bins < 1) {
  stop("Drone coverage is shorter than one complete bin.")
}

qc_bins_initial <- tibble(
  bin_id = seq_len(qc_number_complete_bins),
  bin_start = qc_analysis_start +
    (bin_id - 1) * QC_BIN_WIDTH_SECONDS,
  bin_end = qc_analysis_start + bin_id * QC_BIN_WIDTH_SECONDS
)
qc_analysis_end <- tail(qc_bins_initial$bin_end, 1)

print(qc_bins_initial)
# View(qc_bins_initial)

### JPS Note and potential TODO:
# The drone does seem to start before the radar has any positions. The code removes points where the drone is within the radar's
#  zone of silence but if there are any bins still before the radar starts then it assumes that these are misses.

# ============================================================================
# STEP 6: INTERPOLATE DRONE POSITION EVERY 0.1 SECOND AT BOTH BIN BOUNDARIES
# ============================================================================
qc_evaluation_fraction <- seq(
  0,
  QC_BIN_WIDTH_SECONDS,
  by = DRONE_EVALUATION_STEP_SECONDS
)
qc_position_evaluation <- tibble(
  bin_id = rep(
    qc_bins_initial$bin_id,
    each = length(qc_evaluation_fraction)
  ),
  evaluation_time = rep(
    qc_bins_initial$bin_start,
    each = length(qc_evaluation_fraction)
  ) + rep(qc_evaluation_fraction, times = nrow(qc_bins_initial))
)

qc_drone_numeric_time <- as.numeric(qc_drone_track$drone_timestamp)
qc_evaluation_numeric_time <- as.numeric(qc_position_evaluation$evaluation_time)
qc_position_evaluation <- qc_position_evaluation |>
  mutate(
    drone_x_m = approx(
      x = qc_drone_numeric_time,
      y = qc_drone_track$drone_x_m,
      xout = qc_evaluation_numeric_time,
      rule = 1,
      ties = mean
    )$y,
    drone_y_m = approx(
      x = qc_drone_numeric_time,
      y = qc_drone_track$drone_y_m,
      xout = qc_evaluation_numeric_time,
      rule = 1,
      ties = mean
    )$y,
    drone_distance_to_radar_m = sqrt(
      (drone_x_m - qc_radar_location$radar_x_m[[1]])^2 +
        (drone_y_m - qc_radar_location$radar_y_m[[1]])^2
    )
  )

print(qc_position_evaluation)
# View(qc_position_evaluation)

# Look at the shape of the filtered bins - should look like things are in the cone of silence at the beginning and end
plot(qc_position_evaluation$evaluation_time, qc_position_evaluation$drone_distance_to_radar_m)

# ============================================================================
# STEP 7: APPLY COMPLETE-SUPPORT AND 250 M CONE-OF-SILENCE ELIGIBILITY RULES
# ============================================================================
qc_bin_eligibility <- qc_position_evaluation |>
  group_by(bin_id) |>
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
      minimum_drone_distance_m >= CONE_OF_SILENCE_METERS,
    .groups = "drop"
  )

print(count(qc_bin_eligibility, has_complete_drone_support, outside_cone_of_silence))
# View(qc_bin_eligibility)


# ============================================================================
# STEP 8: ASSIGN RADAR RECORDS TO BINS
# ============================================================================
qc_radar_bin_assignments <- qc_radar_track |> #Initial filter
  filter(
    radar_timestamp >= qc_analysis_start,
    radar_timestamp < qc_analysis_end
  ) |>
  mutate(
    seconds_from_analysis_start = as.numeric(
      difftime(radar_timestamp, qc_analysis_start, units = "secs")
    ),
    bin_id = floor(seconds_from_analysis_start / QC_BIN_WIDTH_SECONDS) + 1L
  ) |>
  filter(bin_id >= 1, bin_id <= qc_number_complete_bins)

qc_radar_counts <- qc_radar_bin_assignments |>
  count(bin_id, name = "n_radar_records")

print(qc_radar_bin_assignments)
print(qc_radar_counts)
# View(qc_radar_bin_assignments)
# View(qc_radar_counts)

################# STEP 8b #############################################
########## JPS: Checking drone and radar bin time alignment ####
#For a compact comparison:

  qc_bin_time_check <- qc_bins_initial |>
  left_join(
    qc_position_evaluation |>
      group_by(bin_id) |>
      summarise(
        first_drone_evaluation = min(evaluation_time),
        last_drone_evaluation = max(evaluation_time),
        .groups = "drop"
      ),
    by = "bin_id"
  ) |>
  left_join(
    qc_radar_bin_assignments |>
      group_by(bin_id) |>
      summarise(
        first_radar_timestamp = min(radar_timestamp),
        last_radar_timestamp = max(radar_timestamp),
        n_radar_records = n(),
        .groups = "drop"
      ),
    by = "bin_id"
  )

  qc_bin_time_check |> filter(bin_id == 221)
## Checks out - yay!!!

# ============================================================================
# STEP 9: CLASSIFY EACH BIN AND IDENTIFY GAP EVENTS
# ============================================================================
qc_detection_bins <- qc_bins_initial |>
  left_join(qc_bin_eligibility, by = "bin_id") |>
  left_join(qc_radar_counts, by = "bin_id") |>
  mutate(
    n_radar_records = coalesce(n_radar_records, 0L),
    eligible = has_complete_drone_support & outside_cone_of_silence,
    exclusion_reason = case_when(
      !has_complete_drone_support ~ "Incomplete drone position support",
      !outside_cone_of_silence ~ paste0("Drone within ", CONE_OF_SILENCE_METERS, " m cone of silence"),
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

qc_eligible_bins <- qc_detection_bins |>
  filter(eligible)
if (nrow(qc_eligible_bins) < 1) {
  stop("No bins remain after the ", CONE_OF_SILENCE_METERS, " m eligibility filter.")
}

# Calculates gap events and duration below.. compresses bins into "gap events"
qc_eligible_runs <- qc_eligible_bins |>
  arrange(bin_id) |>
  mutate(
    begins_new_run =
      row_number() == 1L |
      detection_numeric != lag(
        detection_numeric,
        default = first(detection_numeric)
      ) |
      bin_id != lag(bin_id, default = first(bin_id) - 1L) + 1L,
    run_id = cumsum(begins_new_run)
  ) |>
  group_by(run_id) |>
  summarise(
    run_value = first(detection_numeric),
    start_bin_id = first(bin_id),
    end_bin_id = last(bin_id),
    n_bins = n(),
    .groups = "drop"
  )

qc_gap_events <- qc_eligible_runs |>
  filter(run_value == 0) |>
  transmute(
    behavior = qc_configuration$behavior[[1]],
    flight_id = qc_configuration$flight_id[[1]],
    radar_type = qc_configuration$radar_type[[1]],
    radar_site = qc_configuration$radar_site[[1]],
    bin_width_seconds = QC_BIN_WIDTH_SECONDS,
    bin_start_shift_seconds = QC_SHIFT_SECONDS,
    gap_id = row_number(),
    gap_start = qc_detection_bins$bin_start[
      match(start_bin_id, qc_detection_bins$bin_id)
    ],
    gap_end = qc_detection_bins$bin_end[
      match(end_bin_id, qc_detection_bins$bin_id)
    ],
    start_bin_id,
    end_bin_id,
    n_gap_bins = n_bins,
    gap_duration_seconds = n_bins * QC_BIN_WIDTH_SECONDS
  )

print(count(qc_detection_bins, bin_status))
print(qc_gap_events)
# View(qc_detection_bins)
# View(qc_detection_bins |> filter(!eligible))
# View(qc_gap_events)

### Cross validate gap events with raw data
qc_gap_window <- qc_detection_bins |>
  filter(bin_id >= 399, bin_id <= 508) |>
  summarise(
    gap_start = min(bin_start),
    gap_end = max(bin_end)
  )

qc_gap_drone_records <- qc_drone_selected |>
  filter(
    drone_timestamp >= qc_gap_window$gap_start,
    drone_timestamp <= qc_gap_window$gap_end
  )

qc_gap_radar_context <- qc_radar_raw_aligned |>
  filter(
    radar_timestamp >= qc_gap_window$gap_start - 8,
    radar_timestamp < qc_gap_window$gap_end + 3
  ) |>
  arrange(radar_timestamp)

qc_gap_drone_context <- qc_drone_selected |>
  filter(
    drone_timestamp >= qc_gap_window$gap_start - 3,
    drone_timestamp <= qc_gap_window$gap_end + 3
  ) |>
  arrange(drone_timestamp)

qc_gap_radar_records <- qc_radar_raw_aligned |>
  filter(
    radar_timestamp >= qc_gap_window$gap_start,
    radar_timestamp < qc_gap_window$gap_end
  )



# ============================================================================
# STEP 10: CALCULATE FINAL PRODUCTS AND COMPARE WITH THE LARGE ANALYSIS
# ============================================================================
qc_number_eligible <- nrow(qc_eligible_bins)
qc_number_detected <- sum(qc_eligible_bins$detection_numeric == 1)
qc_number_gap <- sum(qc_eligible_bins$detection_numeric == 0)

qc_detection_summary <- tibble(
  behavior = qc_configuration$behavior[[1]],
  flight_id = qc_configuration$flight_id[[1]],
  radar_type = qc_configuration$radar_type[[1]],
  radar_site = qc_configuration$radar_site[[1]],
  radar_location_file = qc_configuration$radar_location_file[[1]],
  radar_longitude = qc_radar_longitude,
  radar_latitude = qc_radar_latitude,
  bin_width_seconds = QC_BIN_WIDTH_SECONDS,
  bin_start_shift_seconds = QC_SHIFT_SECONDS,
  cone_of_silence_m = CONE_OF_SILENCE_METERS,
  analysis_start = qc_analysis_start,
  analysis_end = qc_analysis_end,
  n_candidate_bins = nrow(qc_detection_bins),
  n_excluded_bins = sum(!qc_detection_bins$eligible),
  n_eligible_bins = qc_number_eligible,
  n_detection_bins = qc_number_detected,
  n_gap_bins = qc_number_gap,
  detection_rate = qc_number_detected / qc_number_eligible,
  gap_rate = qc_number_gap / qc_number_eligible
)

if (nrow(qc_gap_events) == 0) {
  qc_gap_statistics <- tibble(
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
  qc_gap_statistics <- qc_gap_events |>
    summarise(
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

stopifnot(
  all(abs(as.numeric(difftime(
    qc_detection_bins$bin_end,
    qc_detection_bins$bin_start,
    units = "secs"
  )) - QC_BIN_WIDTH_SECONDS) < 1e-9),
  tail(qc_detection_bins$bin_end, 1) <= qc_last_drone_timestamp,
  qc_number_detected + qc_number_gap == qc_number_eligible,
  sum(qc_gap_events$n_gap_bins) == qc_number_gap,
  all(
    qc_detection_bins$minimum_drone_distance_m[qc_detection_bins$eligible] >=
      CONE_OF_SILENCE_METERS
  ),
  abs(qc_detection_summary$detection_rate + qc_detection_summary$gap_rate - 1) <
    COMPARISON_TOLERANCE
)

print(qc_detection_summary, width = Inf)
print(qc_gap_statistics, width = Inf)

# The canonical detection-rate bundle is loaded above to activate these
# comparisons. The independent QC objects above do not overwrite its objects.
if (exists("all_flight_detection_summary", envir = .GlobalEnv, inherits = FALSE)) {
  qc_main_summary <- get(
    "all_flight_detection_summary",
    envir = .GlobalEnv,
    inherits = FALSE
  ) |>
    filter(
      flight_id == qc_configuration$flight_id[[1]],
      bin_width_seconds == QC_BIN_WIDTH_SECONDS,
      abs(bin_start_shift_seconds - QC_SHIFT_SECONDS) < COMPARISON_TOLERANCE
    )

  qc_summary_comparison <- tibble(
    field = c(
      "n_candidate_bins",
      "n_excluded_bins",
      "n_eligible_bins",
      "n_detection_bins",
      "n_gap_bins",
      "detection_rate",
      "gap_rate"
    ),
    independent_qc = c(
      qc_detection_summary$n_candidate_bins,
      qc_detection_summary$n_excluded_bins,
      qc_detection_summary$n_eligible_bins,
      qc_detection_summary$n_detection_bins,
      qc_detection_summary$n_gap_bins,
      qc_detection_summary$detection_rate,
      qc_detection_summary$gap_rate
    ),
    main_analysis = c(
      qc_main_summary$n_candidate_bins,
      qc_main_summary$n_excluded_bins,
      qc_main_summary$n_eligible_bins,
      qc_main_summary$n_detection_bins,
      qc_main_summary$n_gap_bins,
      qc_main_summary$detection_rate,
      qc_main_summary$gap_rate
    )
  ) |>
    mutate(
      absolute_difference = abs(independent_qc - main_analysis),
      matches = absolute_difference <= COMPARISON_TOLERANCE
    )

  print(qc_summary_comparison)
  if (nrow(qc_main_summary) != 1 || !all(qc_summary_comparison$matches)) {
    warning("Independent QC summary differs from the main analysis.", call. = FALSE)
  } else {
    message("QC SUMMARY PASS: all final fields match the main analysis.")
  }
} else {
  message(
    "Main summary is not present. Run detection_rate_analysis.R in this R ",
    "session, then rerun STEP 10 to create qc_summary_comparison."
  )
}

if (exists("all_detection_bins", envir = .GlobalEnv, inherits = FALSE)) {
  qc_main_bins <- get("all_detection_bins", envir = .GlobalEnv, inherits = FALSE) |>
    filter(
      flight_id == qc_configuration$flight_id[[1]],
      bin_width_seconds == QC_BIN_WIDTH_SECONDS,
      abs(bin_start_shift_seconds - QC_SHIFT_SECONDS) < COMPARISON_TOLERANCE
    ) |>
    arrange(bin_id)

  qc_bin_comparison <- qc_detection_bins |>
    select(
      bin_id,
      bin_start_qc = bin_start,
      bin_end_qc = bin_end,
      minimum_distance_qc = minimum_drone_distance_m,
      maximum_distance_qc = maximum_drone_distance_m,
      radar_records_qc = n_radar_records,
      eligible_qc = eligible,
      detected_qc = detected,
      status_qc = bin_status
    ) |>
    full_join(
      qc_main_bins |>
        select(
          bin_id,
          bin_start_main = bin_start,
          bin_end_main = bin_end,
          minimum_distance_main = minimum_drone_distance_m,
          maximum_distance_main = maximum_drone_distance_m,
          radar_records_main = n_radar_records,
          eligible_main = eligible,
          detected_main = detected,
          status_main = bin_status
        ),
      by = "bin_id"
    ) |>
    mutate(
      minimum_distance_difference_m = abs(
        minimum_distance_qc - minimum_distance_main
      ),
      maximum_distance_difference_m = abs(
        maximum_distance_qc - maximum_distance_main
      ),
      classification_matches =
        radar_records_qc == radar_records_main &
        eligible_qc == eligible_main &
        detected_qc == detected_main &
        status_qc == status_main
    )

  print(count(qc_bin_comparison, classification_matches))
  # View(qc_bin_comparison |> filter(!classification_matches))
}

qc_main_gap_events <- all_gap_events |>
  filter(
    flight_id == qc_configuration$flight_id[[1]],
    bin_width_seconds == QC_BIN_WIDTH_SECONDS,
    abs(bin_start_shift_seconds - QC_SHIFT_SECONDS) <=
      COMPARISON_TOLERANCE
  ) |>
  arrange(gap_id) |>
  select(
    gap_id,
    gap_start,
    gap_end,
    start_bin_id,
    end_bin_id,
    n_gap_bins,
    gap_duration_seconds
  )

qc_gap_events_for_comparison <- qc_gap_events |>
  arrange(gap_id) |>
  select(
    gap_id,
    gap_start,
    gap_end,
    start_bin_id,
    end_bin_id,
    n_gap_bins,
    gap_duration_seconds
  )

qc_gap_comparison <- all.equal(
  qc_gap_events_for_comparison,
  qc_main_gap_events,
  tolerance = COMPARISON_TOLERANCE,
  check.attributes = FALSE
)

qc_gap_events_match <- isTRUE(qc_gap_comparison)

if (!qc_gap_events_match) {
  print(qc_gap_comparison)
}



# ---- 2. Check distance calculations -----------------------------------------
qc_maximum_minimum_distance_difference_m <- max(
  qc_bin_comparison$minimum_distance_difference_m,
  na.rm = TRUE
)

qc_maximum_maximum_distance_difference_m <- max(
  qc_bin_comparison$maximum_distance_difference_m,
  na.rm = TRUE
)

qc_distances_match <-
  qc_maximum_minimum_distance_difference_m <= COMPARISON_TOLERANCE &&
  qc_maximum_maximum_distance_difference_m <= COMPARISON_TOLERANCE


# ---- 3. Inspect bins closest to the configured eligibility threshold -------------
qc_nearest_threshold_bins <- qc_detection_bins |>
  mutate(
    distance_from_threshold_m = abs(
      minimum_drone_distance_m - CONE_OF_SILENCE_METERS
    )
  ) |>
  arrange(distance_from_threshold_m) |>
  select(
    bin_id,
    bin_start,
    bin_end,
    minimum_drone_distance_m,
    distance_from_threshold_m,
    outside_cone_of_silence,
    eligible,
    n_radar_records,
    bin_status
  ) |>
  slice_head(n = 10)

print(qc_nearest_threshold_bins)


# ---- 4. Confirm all expected bins were compared -----------------------------
qc_candidate_bin_count_matches <-
  nrow(qc_bin_comparison) ==
  qc_detection_summary$n_candidate_bins[[1]]

qc_all_bin_classifications_match <-
  !anyNA(qc_bin_comparison$classification_matches) &&
  all(qc_bin_comparison$classification_matches)

qc_all_summary_fields_match <-
  !anyNA(qc_summary_comparison$matches) &&
  all(qc_summary_comparison$matches)


# ---- 5. Independently recompile the selected behavior summary ---------------
qc_selected_behavior <- qc_configuration$behavior[[1]]

qc_behavior_summary_recalculated <- all_flight_detection_summary |>
  filter(
    behavior == qc_selected_behavior,
    bin_width_seconds == QC_BIN_WIDTH_SECONDS,
    abs(bin_start_shift_seconds - QC_SHIFT_SECONDS) <=
      COMPARISON_TOLERANCE
  ) |>
  summarise(
    n_flights = n_distinct(flight_id),
    n_candidate_bins = sum(n_candidate_bins),
    n_excluded_bins = sum(n_excluded_bins),
    n_eligible_bins = sum(n_eligible_bins),
    n_detection_bins = sum(n_detection_bins),
    n_gap_bins = sum(n_gap_bins),
    pooled_detection_rate =
      n_detection_bins / n_eligible_bins,
    mean_flight_detection_rate = mean(detection_rate),
    sd_flight_detection_rate = sd(detection_rate),
    median_flight_detection_rate = median(detection_rate),
    minimum_flight_detection_rate = min(detection_rate),
    maximum_flight_detection_rate = max(detection_rate)
  )

qc_behavior_summary_from_main <- behavior_detection_summary |>
  filter(
    behavior == qc_selected_behavior,
    bin_width_seconds == QC_BIN_WIDTH_SECONDS,
    abs(bin_start_shift_seconds - QC_SHIFT_SECONDS) <=
      COMPARISON_TOLERANCE
  ) |>
  select(names(qc_behavior_summary_recalculated))

qc_behavior_summary_comparison <- all.equal(
  qc_behavior_summary_recalculated,
  qc_behavior_summary_from_main,
  tolerance = COMPARISON_TOLERANCE,
  check.attributes = FALSE
)

qc_behavior_compilation_matches <- isTRUE(
  qc_behavior_summary_comparison
)

if (!qc_behavior_compilation_matches) {
  print(qc_behavior_summary_comparison)
}


# ---- 6. Produce one compact validation record -------------------------------
qc_validation_result <- tibble(
  flight_id = qc_configuration$flight_id[[1]],
  behavior = qc_selected_behavior,
  radar_type = qc_configuration$radar_type[[1]],
  bin_width_seconds = QC_BIN_WIDTH_SECONDS,
  bin_start_shift_seconds = QC_SHIFT_SECONDS,
  n_bins_compared = nrow(qc_bin_comparison),
  summary_fields_match = qc_all_summary_fields_match,
  candidate_bin_count_matches = qc_candidate_bin_count_matches,
  bin_classifications_match = qc_all_bin_classifications_match,
  gap_events_match = qc_gap_events_match,
  distances_match = qc_distances_match,
  behavior_compilation_matches = qc_behavior_compilation_matches,
  maximum_minimum_distance_difference_m =
    qc_maximum_minimum_distance_difference_m,
  maximum_maximum_distance_difference_m =
    qc_maximum_maximum_distance_difference_m,
  checked_at_utc = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
) |>
  mutate(
    overall_pass =
      summary_fields_match &&
      candidate_bin_count_matches &&
      bin_classifications_match &&
      gap_events_match &&
      distances_match &&
      behavior_compilation_matches
  )

print(qc_validation_result, width = Inf)

if (qc_validation_result$overall_pass[[1]]) {
  message("EXTENDED QC PASS: all checks matched.")
} else {
  warning(
    "EXTENDED QC FAILURE: inspect qc_validation_result and comparison objects.",
    call. = FALSE
  )
}


# Most useful inspection objects:
# View(qc_configuration)
# View(qc_drone_selected)
# View(qc_drone_track)
# View(qc_radar_track)
# View(qc_radar_location)
# View(qc_bins_initial)
# View(qc_position_evaluation)
# View(qc_bin_eligibility)
# View(qc_radar_bin_assignments)
# View(qc_detection_bins)
# View(qc_gap_events)
# View(qc_detection_summary)
# View(qc_summary_comparison)
# View(qc_bin_comparison)
# ============================================================================
