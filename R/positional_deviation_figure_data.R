# Reusable data preparation, plotting, statistics, and output functions for the
# positional-deviation paper figures. The approved flight-alignment calculation
# occurs upstream and is not changed here.

theme_flight <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.title = ggplot2::element_text(),
      legend.title = ggplot2::element_text()
    )
}

.capture_new_objects <- function(before, environment) {
  object_names <- setdiff(ls(envir = environment), c(before, "before"))
  mget(object_names, envir = environment, inherits = FALSE)
}

prepare_positional_deviation_figure_data <- function(
    bundle = read_positional_deviation_bundle()) {
  positional_results <- bundle
  before <- ls(envir = environment())
# Load the consolidated positional-deviation results.
positional_results <- bundle
cube_result <- positional_deviation_behavior_result(positional_results, "cubes")
foraging_result <- positional_deviation_behavior_result(positional_results, "foraging")
chasing_result <- positional_deviation_behavior_result(positional_results, "chasing")
soaring_result <- positional_deviation_behavior_result(positional_results, "soaring")
transiting_result <- positional_deviation_behavior_result(positional_results, "transiting")

cube_paired <- cube_result$paired_data
foraging_paired <- foraging_result$paired_data
chasing_paired <- chasing_result$paired_data
soaring_paired <- soaring_result$paired_data
transiting_paired <- transiting_result$paired_data
cube_rmse <- cube_result$rmse
foraging_rmse <- foraging_result$rmse
chasing_rmse <- chasing_result$rmse
soaring_rmse <- soaring_result$rmse
transiting_rmse <- transiting_result$rmse

# Adding behavior labels
cube_paired$behavior <- "Cube"
foraging_paired$behavior <- "Foraging"
chasing_paired$behavior <- "Chasing"
soaring_paired$behavior <- "Soaring"
transiting_paired$behavior <- "Transiting"

# Combining paired data
all_behaviors <- bind_rows(
  cube_paired,
  foraging_paired,
  chasing_paired,
  soaring_paired,
  transiting_paired
)

# Remove random error points
all_behaviors <- all_behaviors %>%
  filter(
    abs(dev_x) <= 100,
    abs(dev_y) <= 100,
    abs(dev_z) <= 100
    #euclidean_distance <= 100
  )

# Calculate drone speed (m/s)
all_behaviors <- all_behaviors %>%
  arrange(unique_flight, milliseconds_elapsed) %>%
  group_by(unique_flight) %>%
  mutate(
    dt = c(NA, diff(milliseconds_elapsed)),   # seconds
    dx = c(NA, diff(X_m_drone)),
    dy = c(NA, diff(Y_m_drone)),
    dz = c(NA, diff(z__drone)),
    speed = sqrt(dx^2 + dy^2 + dz^2) / dt
  ) %>%
  ungroup()

## Test for reasonable speeds:
# summary(all_behaviors$speed)
#
# ggplot(all_behaviors, aes(speed)) +
#   geom_histogram(bins = 40)

# combining rmse data
all_rmse <- bind_rows(
  cube_rmse,
  foraging_rmse,
  chasing_rmse,
  soaring_rmse,
  transiting_rmse
)

# Speeds - bin the speeds and plotting the mean absolute error ± 95% confidence interval
speed_data <-
  all_behaviors %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  )


  .capture_new_objects(before, environment())
}
