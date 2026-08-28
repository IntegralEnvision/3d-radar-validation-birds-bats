# Utilities for reading and validating the flight registry.

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (length(list.files(current, pattern = "[.]Rproj$")) > 0L) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate the project root from: ", start, call. = FALSE)
    }
    current <- parent
  }
}

project_path <- function(..., root = find_project_root()) {
  file.path(root, ...)
}

parse_drone_indices <- function(specification) {
  if (is.na(specification) || !nzchar(trimws(specification))) {
    return(integer())
  }

  pieces <- strsplit(gsub("\\s+", "", specification), ";", fixed = FALSE)[[1]]
  indices <- unlist(lapply(pieces, function(piece) {
    if (grepl(":", piece, fixed = TRUE)) {
      bounds <- as.integer(strsplit(piece, ":", fixed = TRUE)[[1]])
      if (length(bounds) != 2L || anyNA(bounds)) {
        stop("Invalid drone_indices specification: ", specification, call. = FALSE)
      }
      seq.int(bounds[[1]], bounds[[2]])
    } else {
      value <- as.integer(piece)
      if (is.na(value)) {
        stop("Invalid drone_indices specification: ", specification, call. = FALSE)
      }
      value
    }
  }), use.names = FALSE)

  as.integer(indices)
}

read_flight_metadata <- function(
    path = project_path("metadata", "flights.csv"),
    check_files = TRUE) {
  if (!file.exists(path)) {
    stop("Flight metadata file was not found: ", path, call. = FALSE)
  }

  metadata <- read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE
  )

  required_columns <- c(
    "behavior", "flight_id", "radar_file", "drone_file",
    "radar_location_file", "radar_site", "radar_type", "offset_GPS",
    "configured_start_time", "configured_end_time", "drone_indices",
    "detection_rate_include", "positional_deviation_include"
  )
  missing_columns <- setdiff(required_columns, names(metadata))
  if (length(missing_columns) > 0L) {
    stop(
      "Flight metadata is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(metadata$flight_id)) {
    duplicate_ids <- unique(metadata$flight_id[duplicated(metadata$flight_id)])
    stop("Duplicate flight_id values: ", paste(duplicate_ids, collapse = ", "), call. = FALSE)
  }

  parse_flag <- function(values, column) {
    normalized <- toupper(trimws(as.character(values)))
    result <- normalized == "TRUE"
    invalid <- is.na(values) | !(normalized %in% c("TRUE", "FALSE"))
    if (any(invalid)) {
      stop("Column ", column, " must contain only TRUE or FALSE.", call. = FALSE)
    }
    result
  }

  metadata$detection_rate_include <- parse_flag(
    metadata$detection_rate_include,
    "detection_rate_include"
  )
  metadata$positional_deviation_include <- parse_flag(
    metadata$positional_deviation_include,
    "positional_deviation_include"
  )
  metadata$radar_type <- as.character(metadata$radar_type)
  metadata$offset_GPS <- as.numeric(metadata$offset_GPS)
  metadata$configured_start_time <- as.POSIXct(
    metadata$configured_start_time,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  metadata$configured_end_time <- as.POSIXct(
    metadata$configured_end_time,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )

  if (anyNA(metadata$configured_start_time) || anyNA(metadata$configured_end_time)) {
    stop("Configured flight times must use UTC ISO-8601 format.", call. = FALSE)
  }
  if (any(metadata$configured_end_time <= metadata$configured_start_time)) {
    stop("Every configured_end_time must be later than configured_start_time.", call. = FALSE)
  }

  metadata$drone_indices <- lapply(metadata$drone_indices, parse_drone_indices)
  if (any(lengths(metadata$drone_indices) == 0L)) {
    stop("Every flight must have a non-empty drone_indices specification.", call. = FALSE)
  }

  path_columns <- c("radar_file", "drone_file", "radar_location_file")
  for (column in path_columns) {
    metadata[[column]] <- gsub("\\\\", "/", metadata[[column]])
  }

  if (check_files) {
    missing_files <- unlist(lapply(path_columns, function(column) {
      values <- metadata[[column]]
      values[!file.exists(file.path(find_project_root(), values))]
    }), use.names = FALSE)
    missing_files <- unique(missing_files)
    if (length(missing_files) > 0L) {
      stop(
        "Metadata references missing files:\n  ",
        paste(missing_files, collapse = "\n  "),
        call. = FALSE
      )
    }
  }

  metadata
}

load_positional_deviation_inputs <- function(
    behavior,
    metadata = read_flight_metadata(),
    envir = parent.frame()) {
  required_columns <- c("radar_object", "drone_object")
  missing_columns <- setdiff(required_columns, names(metadata))
  if (length(missing_columns) > 0L) {
    stop(
      "Flight metadata is missing positional input columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  selected <- metadata[
    metadata$behavior == behavior & metadata$positional_deviation_include,
    ,
    drop = FALSE
  ]
  if (nrow(selected) == 0L) {
    stop("No positional-deviation flights selected for behavior: ", behavior, call. = FALSE)
  }
  if (anyDuplicated(c(selected$radar_object, selected$drone_object))) {
    stop("Positional input object names must be unique within ", behavior, ".", call. = FALSE)
  }

  root <- find_project_root()
  for (row_index in seq_len(nrow(selected))) {
    assign(
      selected$radar_object[[row_index]],
      read.csv(file.path(root, selected$radar_file[[row_index]])),
      envir = envir
    )
    assign(
      selected$drone_object[[row_index]],
      read.csv(file.path(root, selected$drone_file[[row_index]])),
      envir = envir
    )
  }

  invisible(selected)
}
