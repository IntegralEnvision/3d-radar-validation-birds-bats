# Metric helpers used by the positional-deviation analysis.

calculate_rmse <- function(deviation) {
  sqrt(mean(deviation^2, na.rm = TRUE))
}

calculate_stats_mph_to_mps <- function(data, column_name, dataframe_name = "Unnamed Dataset") {
  if (!column_name %in% colnames(data)) {
    stop("Column not found in the dataframe.")
  }

  column_data <- data[[column_name]]
  if (!is.numeric(column_data)) {
    stop("The specified column is not numeric.")
  }

  column_data_mps <- column_data * 0.44704
  list(
    dataset_name = dataframe_name,
    column_name = column_name,
    mean_mps = mean(column_data_mps, na.rm = TRUE),
    min_mps = min(column_data_mps, na.rm = TRUE),
    max_mps = max(column_data_mps, na.rm = TRUE)
  )
}
