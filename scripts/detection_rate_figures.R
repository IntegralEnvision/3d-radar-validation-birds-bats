# Detection-rate diagnostic figures and unshifted summary table.
# This script can run independently after detection_rate_analysis.R has created
# the consolidated intermediate bundle.
source(file.path("R", "analysis_config.R"))
if (!exists("analysis_config", inherits = FALSE)) analysis_config <- load_analysis_config()
source(file.path("R", "flight_metadata.R"))
source(file.path("R", "detection_rate_bundle.R"))
if (!exists("all_flight_detection_summary", inherits = FALSE)) {
  results_path <- Sys.getenv(
    "DETECTION_RATE_RESULTS_PATH",
    unset = detection_rate_bundle_path()
  )
  load_detection_rate_results(read_detection_rate_bundle(results_path))
}
library(dplyr)
library(ggplot2)

behavior_levels <- c("Cube", "Chasing", "DynoSoaring", "Foraging", "Transiting")
behavior_labels <- c("Cubes", "Chasing", "Soaring", "Foraging", "Transiting")
behavior_colors <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00")
radar_colors <- c("7360" = "#0072B2", "9090" = "#D55E00")
pct <- function(x) paste0(round(100 * x), "%")
plot_theme <- theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(), legend.title = element_text(), legend.position = "none")

# Primary flight-level results: unshifted 1-second bins.
primary_1s <- all_flight_detection_summary |>
  filter(bin_width_seconds == 1, bin_start_shift_seconds == 0) |>
  mutate(
    behavior = factor(behavior, levels = behavior_levels, labels = behavior_labels),
    radar_type = factor(radar_type, levels = c("7360", "9090"))
  )

behavior_detection_by_bin <- all_flight_detection_summary |>
  filter(bin_start_shift_seconds == 0) |>
  mutate(
    behavior = factor(behavior, levels = behavior_levels, labels = behavior_labels),
    bin_length = factor(
      bin_width_seconds,
      levels = BIN_WIDTH_OPTIONS_SECONDS,
      labels = paste0(BIN_WIDTH_OPTIONS_SECONDS, " s")
    )
  )

behavior_n <- behavior_detection_by_bin |>
  group_by(behavior) |>
  summarise(n = n_distinct(flight_id), .groups = "drop")
radar_n <- primary_1s |> count(radar_type, name = "n")

# Each behavior has one flight-level boxplot for every unshifted bin duration.
figure_detection_rate_by_behavior <- ggplot(
  behavior_detection_by_bin,
  aes(behavior, detection_rate, fill = bin_length)
) +
  geom_boxplot(
    width = .5,
    outlier.shape = 21,
    position = position_dodge2(width = .75, preserve = "single")
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, .2),
    labels = pct,
    expand = expansion(mult = c(0, .02))
  ) +
  scale_fill_viridis_d(name = "Temporal Window", drop = FALSE) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(
    x = "Behavior",
    y = "Detection rate",
    fill = "Bin length"
  ) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.position = "right"
  )
figure_detection_rate_by_radar_type <- ggplot(
  primary_1s, aes(radar_type, detection_rate, fill = radar_type)
) +
  geom_boxplot(width = .6, outlier.shape = 21) +
  geom_text(data = radar_n, aes(radar_type, 1.055, label = paste0("N = ", n)),
            inherit.aes = FALSE, vjust = 0) +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 1, .2), labels = pct) +
  coord_cartesian(ylim = c(0, 1.12), clip = "off") +
  labs(x = "Radar type", y = "Detection rate",
       title = "One-second detection rate by radar type",
       subtitle = "Unshifted bins; descriptive because behavior and radar type are not fully crossed") +
  plot_theme

# Gap-event distributions for primary 1-second bins. The logarithmic axis keeps
# short common gaps visible while retaining unusually long gaps.
primary_gaps <- all_gap_events |>
  filter(bin_width_seconds == 1, bin_start_shift_seconds == 0) |>
  mutate(
    behavior = factor(behavior, levels = behavior_levels, labels = behavior_labels),
    radar_type = factor(radar_type, levels = c("7360", "9090"))
  )
behavior_gap_n <- primary_gaps |> count(behavior, name = "n") |> mutate(label_y = 190); behavior_gaps_over_200 <- primary_gaps |> filter(gap_duration_seconds > 200) |> group_by(behavior) |> summarise(n_over_200 = n(), maximum_gap_seconds = max(gap_duration_seconds), .groups = "drop") |> mutate(marker_y = 200, label_y = 174, outlier_label = paste0(n_over_200, " gap", if_else(n_over_200 == 1, "", "s"), " >200 s (max = ", maximum_gap_seconds, " s)"))
radar_gap_n <- primary_gaps |> count(radar_type, name = "n") |>
  mutate(label_y = max(primary_gaps$gap_duration_seconds) * 1.35)

figure_gap_duration_by_behavior <- ggplot(
  primary_gaps, aes(behavior, gap_duration_seconds, fill = behavior)
) +
  geom_boxplot(width = .65, outlier.shape = 21) +
  geom_text(data = behavior_gap_n, aes(behavior, label_y, label = paste0("N = ", n)), inherit.aes = FALSE) + geom_point(data = behavior_gaps_over_200, aes(behavior, marker_y), inherit.aes = FALSE, shape = 24, size = 3, fill = "black") + geom_text(data = behavior_gaps_over_200, aes(behavior, label_y, label = outlier_label), inherit.aes = FALSE, size = 3.2) + scale_fill_viridis_d() + scale_y_continuous(breaks = seq(0, 200, 50)) + coord_cartesian(ylim = c(0, 205)) +
  labs(x = NULL, y = "Gap duration (seconds)",
       title = "One-second-bin gap duration by behavior",
       subtitle = "N is the number of gap events; triangle marks gap(s) continuing above 200 s") +
  plot_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1))

figure_gap_duration_by_radar_type <- ggplot(
  primary_gaps, aes(radar_type, gap_duration_seconds, fill = radar_type)
) +
  geom_boxplot(width = .6, outlier.shape = 21) +
  geom_text(data = radar_gap_n,
            aes(radar_type, label_y, label = paste0("N = ", n)),
            inherit.aes = FALSE) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion(mult = c(.02, .12))) +
  labs(x = "Radar type", y = "Gap duration (seconds)",
       title = "One-second-bin gap duration by radar type",
       subtitle = "N is the number of gap events; triangle marks gap(s) continuing above 200 s") +
  plot_theme

# Existing distance and sensitivity figures.
distance_detection_summary <- all_detection_bins |>
  filter(bin_width_seconds == 1, bin_start_shift_seconds == 0, eligible) |>
  mutate(distance_m = (minimum_drone_distance_m + maximum_drone_distance_m) / 2,
         distance_band_midpoint_m = floor(distance_m / 100) * 100 + 50) |>
  group_by(radar_type, distance_band_midpoint_m) |>
  summarise(n_flights = n_distinct(flight_id), n_eligible_bins = n(),
            detection_rate = mean(detection_numeric), .groups = "drop") |>
  filter(n_eligible_bins >= 20)
figure_detection_rate_by_distance <- ggplot(
  distance_detection_summary,
  aes(distance_band_midpoint_m, detection_rate, color = factor(radar_type),
      shape = factor(radar_type))
) + geom_line() + geom_point() +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(0, 1), labels = pct) +
  labs(x = "Horizontal distance from radar (m; 100-m bands)",
       y = "Pooled detection rate", color = "Radar type", shape = "Radar type",
       title = "One-second detection rate by distance and radar type",
       subtitle = "Bands contain at least 20 eligible seconds") +
  theme_bw(base_size = 12) + theme(plot.title = element_text(hjust = 0.5), legend.position = "top")

bin_shift_sensitivity_by_flight <- all_flight_detection_summary |>
  select(behavior, flight_id, radar_type, bin_width_seconds,
         bin_start_shift_seconds, detection_rate) |>
  mutate(alignment = if_else(bin_start_shift_seconds == 0, "unshifted", "shifted")) |>
  select(-bin_start_shift_seconds) |>
  tidyr::pivot_wider(names_from = alignment, values_from = detection_rate) |>
  filter(!is.na(unshifted), !is.na(shifted)) |>
  mutate(shift_difference = shifted - unshifted,
         bin_length = factor(bin_width_seconds, levels = BIN_WIDTH_OPTIONS_SECONDS,
                             labels = paste0(BIN_WIDTH_OPTIONS_SECONDS, " s")))
bin_shift_sensitivity_summary <- bin_shift_sensitivity_by_flight |>
  group_by(bin_width_seconds, bin_length) |>
  summarise(n_paired_flights = n(), median_difference = median(shift_difference),
            maximum_absolute_difference = max(abs(shift_difference)), .groups = "drop") |> mutate(label_y = max(bin_shift_sensitivity_by_flight$shift_difference) + 0.08 * diff(range(bin_shift_sensitivity_by_flight$shift_difference))); figure_bin_shift_sensitivity <- ggplot(
  bin_shift_sensitivity_by_flight, aes(bin_length, shift_difference, fill = bin_length)
) + geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.shape = 21) + geom_text(data = bin_shift_sensitivity_summary, aes(bin_length, label_y, label = paste0("N = ", n_paired_flights)), inherit.aes = FALSE, vjust = 0) + scale_fill_viridis_d() + scale_y_continuous(labels = pct, expand = expansion(mult = c(.05, .15))) +
  labs(x = "Bin length", y = "Shifted minus unshifted detection rate",
       title = "Sensitivity to a half-bin shift") + plot_theme

# Unshifted table only; no sensitivity estimates are included.
radar_type_bin_length_table <- radar_type_detection_summary |>
  filter(bin_start_shift_seconds == 0) |>
  select(radar_type, bin_width_seconds, n_flights, n_eligible_bins,
         n_detection_bins, pooled_detection_rate, median_flight_detection_rate) |>
  left_join(
    all_flight_detection_summary |> filter(bin_start_shift_seconds == 0) |>
      group_by(radar_type, bin_width_seconds) |>
      summarise(q25_flight_detection_rate = quantile(detection_rate, .25),
                q75_flight_detection_rate = quantile(detection_rate, .75),
                .groups = "drop"),
    by = c("radar_type", "bin_width_seconds")
  ) |>
  left_join(
    radar_type_gap_summary |> filter(bin_start_shift_seconds == 0) |>
      select(radar_type, bin_width_seconds, n_gaps, minimum_gap_seconds,
             median_gap_seconds, percentile_75_gap_seconds,
             percentile_95_gap_seconds, maximum_gap_seconds),
    by = c("radar_type", "bin_width_seconds")
  )

safe_ggsave <- function(...) { tryCatch(ggsave(...), error = function(e) warning("Could not overwrite an open figure file: ", conditionMessage(e), call. = FALSE)) }
figure_output_directory <- project_path(analysis_config$outputs$detection_figures)
dir.create(figure_output_directory, recursive = TRUE, showWarnings = FALSE)
safe_ggsave(
  file.path(figure_output_directory, "detection_rate_by_behavior_1s.pdf"),
  figure_detection_rate_by_behavior,
  width = 8.2,
  height = 5.7,
  dpi = 600,
  device = cairo_pdf
)
safe_ggsave(file.path(figure_output_directory, "detection_rate_by_radar_type_1s.pdf"), figure_detection_rate_by_radar_type, width = 7.2, height = 5.5, dpi = 300)
safe_ggsave(file.path(figure_output_directory, "gap_duration_by_behavior_1s.pdf"), figure_gap_duration_by_behavior, width = 8.2, height = 5.7, dpi = 300)
safe_ggsave(file.path(figure_output_directory, "gap_duration_by_radar_type_1s.pdf"), figure_gap_duration_by_radar_type, width = 7.2, height = 5.5, dpi = 300)
safe_ggsave(file.path(figure_output_directory, "detection_rate_by_distance_1s.pdf"), figure_detection_rate_by_distance, width = 8.2, height = 5.7, dpi = 300)
safe_ggsave(file.path(figure_output_directory, "bin_shift_sensitivity.pdf"), figure_bin_shift_sensitivity, width = 8.2, height = 5.7, dpi = 300)
write.csv(radar_type_bin_length_table,
          file.path(figure_output_directory, "radar_type_bin_length_table_unshifted.csv"),
          row.names = FALSE)
