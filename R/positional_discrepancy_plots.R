# Reusable positional-discrepancy paper-figure functions.

build_positional_discrepancy_plots <- function(figure_data) {
  list2env(figure_data, envir = environment())
  before <- ls(envir = environment())
########################################
# All Plots
########################################

# Speed vs. Radar Error - How does radar error change as drone speed increases?
# Bin drone speeds and calculate the mean absolute error ± 95% confidence interval
speed_summary <-
  speed_data %>%
  mutate(
    speed_bin = floor(speed)
  ) %>%
  group_by(behavior, axis, speed_bin) %>%
  summarise(
    mean_error = mean(abs(error), na.rm = TRUE),
    sd_error = sd(abs(error), na.rm = TRUE),
    n = n(),
    se = sd_error / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

speedPlot <- ggplot(
  speed_summary,
  aes(
    x = speed_bin,
    y = mean_error,
    color = behavior
  )
) +
  geom_ribbon(
    aes(
      ymin = mean_error - 1.96 * se,
      ymax = mean_error + 1.96 * se,
      fill = behavior
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  facet_wrap(~axis) +
  labs(
    title = "", # OLD Mean Absolute Error vs Drone Speed",
    x = "Drone Speed (m/s)",
    y = "Mean Absolute Deviation (m)",
    color = "Behavior",
    fill = "Behavior"
  ) +
  coord_cartesian(xlim = c(0, 13)) +
  scale_fill_viridis_d() +
  theme_flight()

# Figure 5: RMSE by behavior - Which flight behavior produces the largest radar errors?
# rmseByBehav <- ggplot(
#   all_rmse %>%
#     filter(measure %in% c("dev_x", "dev_y", "dev_z")),
#   aes(
#     x = factor(
#       behavior,
#       levels = c("Cubes", "Transiting", "Foraging", "Soaring", "Chasing")
#     ),
#     y = RMSE,
#     fill = behavior
#   )
# ) +
#   geom_boxplot() +
#   #facet_wrap(~measure) +
#   facet_wrap(
#     ~measure,
#     labeller = labeller(measure = c(
#       dev_x = "A. X Deviation",
#       dev_y = "B. Y Deviation",
#       dev_z = "C. Z Deviation"
#     ))
#   ) +
#   labs(
#     title = "", #"RMSD by Behavior and Error Component",
#     x = "Behavior",
#     y = "RMSD (m)",
#     fill = "Behavior"
#   ) +
#   scale_fill_viridis_d() +
#   theme_flight()

# Panel labels
panel_labels <- data.frame(
  measure = factor(
    c("dev_x", "dev_y", "dev_z"),
    levels = c("dev_x", "dev_y", "dev_z")
  ),
  label = c("(A)", "(B)", "(C)")
)

# behavior_levels <- c(
#   "Cubes",
#   sort(setdiff(unique(all_rmse$behavior), "Cubes"))
# )
behavior_levels <- c(
  "Cubes",
  sort(setdiff(
    unique(as.character(all_rmse$behavior)),
    "Cubes"
  ))
)

rmse_plot_data <- all_rmse %>%
  filter(measure %in% c("dev_x", "dev_y", "dev_z")) %>%
  mutate(
    measure = factor(
      measure,
      levels = c("dev_x", "dev_y", "dev_z")
    ),
    behavior = factor(
      as.character(behavior),
      levels = behavior_levels
    )
  )

rmseByBehav <- ggplot(
  rmse_plot_data,
  aes(
    x = behavior,
    y = RMSE,
    fill = behavior
  )
) +
  geom_boxplot(width = 0.5) +

  # facet_wrap(
  #   ~measure,
  #   nrow = 1
  # ) +

  facet_wrap(
    ~measure,
    ncol = 1
  ) +

  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    vjust = 1.2,
    size = 5
  ) +

  labs(
    title = "",
    x = NULL,
    y = "RMSD (m)",
    fill = "Behavior"
  ) +

  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 30, 10),
    expand = c(0, 0)
  ) +

  scale_fill_viridis_d() +

  theme_flight() +

  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    text = element_text(
      family = "sans",
      size = 12
    ),

    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),

    axis.text.y = element_text(size = 10),

    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),

    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),

    panel.spacing = unit(0.7, "lines")
  )

# TODO: JPS: In final code edits make this universally set for all plots,
# perhaps a part of the plot themes
rmseByBehav <- rmseByBehav +
  theme(
    text = element_text(family = "sans", size = 12),
    legend.position = "none"
  )

# Error vs distance -  Does radar error (x, y, and z) increase as the target moves farther from the radar?
# Bin distance measurements and compute the mean absolute error ± 95% confidence interval
# Figure 8
error_vs_distance_data <-
  all_behaviors %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  )

error_summary <-
  error_vs_distance_data %>%
  mutate(
    distance_bin = floor(distance_to_radar / 25) * 25
  ) %>%
  group_by(behavior, axis, distance_bin) %>%
  summarise(
    mean_error = mean(abs(error), na.rm = TRUE),
    sd_error = sd(abs(error), na.rm = TRUE),
    n = n(),
    se = sd_error / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

behavior_levels <- unique(as.character(error_summary$behavior))

behavior_levels <-
  c(
    "Cube",
    sort(setdiff(
      unique(as.character(error_summary$behavior)),
      "Cube"
    ))
  )


behavior_labels <- behavior_levels
behavior_labels[behavior_levels == "Cube"] <- "Cubes"


# Set behavior order: Cube first, then remaining behaviors alphabetically
error_summary <- error_summary %>%
  mutate(
    behavior = factor(
      as.character(behavior),
      levels = c(
        "Cube",
        sort(setdiff(
          unique(as.character(behavior)),
          "Cube"
        ))
      )
    )
  )

panel_labels <- data.frame(
  axis = c("dev_x", "dev_y", "dev_z"),
  label = c("(A)", "(B)", "(C)")
)

errorVsDist <- ggplot(
  error_summary,
  aes(
    x = distance_bin,
    y = mean_error,
    color = behavior
  )
) +
  geom_ribbon(
    aes(
      ymin = mean_error - 1.96 * se,
      ymax = mean_error + 1.96 * se,
      fill = behavior
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 0.75) +
  geom_hline(
    data = data.frame(
      axis = c("dev_x", "dev_y", "dev_z"),
      threshold = c(5, 5, 15)
    ),
    aes(
      yintercept = threshold
    ),
    linetype = "dotted",
    color = "black",
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  facet_wrap(~axis, ncol=1) +
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    hjust = -0.2,
    vjust = 1.3,
    size = 4,
    family = "sans",
    inherit.aes = FALSE
  ) +
  labs(
    title = "",
    x = "Distance from Radar (m)",
    y = "MAD (m)",
    color = "Behavior",
    fill = "Behavior"
  ) +
  scale_x_continuous(
    limits = c(0, 1500),
    breaks = seq(0, 1400, by = 100),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 3),
    expand = expansion(mult = c(0, 0))
  )+
  scale_color_viridis_d(
    breaks = behavior_levels,
    labels = behavior_labels
  ) +
  scale_fill_viridis_d(
    breaks = behavior_levels,
    labels = behavior_labels
  ) +
  theme_flight() +
  theme(
    text = element_text(
      family = "sans",
      size = 12
    ),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0.7,"lines"),
    axis.text.x = element_text(
      #family = "Arial",
      size = 10,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 10,
      margin = margin(r=5)
    ),
    axis.title.x = element_text(
      size = 11
    ),
    axis.title.y = element_text(
      size = 11
    ),
    legend.title = element_text(
      size = 11
    ),
    legend.text = element_text(
      size = 10
    )

  )
# Error Distribution by Behavior - Which behaviors have greater or lessor error variability?
# Compares the full distribution of signed errors for each flight behavior
# Wider distributions indicate greater variability in radar error
drone_error <- data.frame(
  axis = c("dev_x", "dev_y", "dev_z"),
  lower = c(-5, -5, -15),
  upper = c(5, 5, 15)
)

# Sorting Behaviors
# Set behavior order: Cubes first, then alphabetical
# Figure 7
# TODO - JPS for some reason in this dataset Cubes got called Cube - QC this and make sure it is corrected

# behavior_order <- c(
#   "Cube",
#   "Chasing",
#   "Diving",
#   "Foraging",
#   "Soaring"
# )
# Explicit behavior order for display
behavior_order <- c(
  "Cubes",
  "Chasing",
  "Foraging",
  "Soaring",
  "Transiting"
)

error_vs_distance_data <- error_vs_distance_data %>%
  mutate(
    behavior_display = case_when(
      behavior == "Cube" ~ "Cubes",
      TRUE ~ as.character(behavior)
    ),
    behavior_display = factor(
      behavior_display,
      levels = behavior_order
    )
  )

# Check this BEFORE plotting
levels(error_vs_distance_data$behavior_display)

panel_labels <- data.frame(
  behavior_display = factor(
    "Cubes",
    levels = behavior_order
  ),
  axis = c("dev_x", "dev_y", "dev_z"),
  label = c("(A)", "(B)", "(C)")
)

errorDistrib <- ggplot(
  error_vs_distance_data,
  aes(
    x = error,
    fill = behavior_display
  )
) +

  # Shade the acceptable drone-error range
  geom_rect(
    data = drone_error,
    aes(
      xmin = lower,
      xmax = upper,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey70",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +

  # Error distributions
  geom_density(alpha = 0.4) +

  facet_grid(
    behavior_display ~ axis,
    as.table = TRUE
  ) +

  # Panel labels only in the top row
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    hjust = -0.2,
    vjust = 1.3,
    size = 5,
    family = "sans",
    inherit.aes = FALSE
  ) +

  labs(
    title = "",
    x = "Positional Deviation (m)",
    y = "Density",
    fill = "Behavior"
  ) +

  scale_fill_viridis_d() +

  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 3)
  ) +

  theme_flight() +

  theme(
    text = element_text(
      family = "sans",
      size = 12
    ),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.y = element_text(
      family = "sans",
      size = 9
    ),
    strip.text.x = element_blank(),
    axis.text.x = element_text(
      family = "sans",
      size = 9,
      angle = 45,
      hjust = 1
    ),
    scale_y_continuous(
      limits = c(0, NA),
      breaks = scales::pretty_breaks(n = 3),
      expand = c(0, 0)
    )

  )
# Component error comparison
# Compare the distributions of signed X, Y, and Z errors across flight behaviors
component_data <-
  all_behaviors %>%
  pivot_longer(
    c(dev_x,dev_y,dev_z),
    names_to="axis",
    values_to="error"
  )

componentError <- ggplot(
  component_data,
  aes( # JPS edits to x to get proper order for behavior
    x = factor(
        behavior,
        levels = c("Cube", "Transiting", "Foraging", "Soaring", "Chasing")
      ),
    y=error,
    fill=behavior
  )
)+
  geom_boxplot()+
  #facet_wrap(~axis) +
  facet_wrap(
    ~axis,
    labeller = labeller(axis = c(
      dev_x = "A. X Deviation",
      dev_y = "B. Y Deviation",
      dev_z = "C. Z Deviation"
    ))
  ) +
  labs(
    title = "", #OLD title "Component-wise Error by Behavior",
    x = "Behavior",
    y = "Deviation", #Old Error (m)",
    fill = "Behavior"
  ) +
  scale_fill_viridis_d() +
  theme_flight()

# Mean RMSE bar chart - Figure 6
# Compare the average RMSE of each error component across flight behaviors

behavior_summary <-
  all_rmse %>%
  filter(measure %in% c("dev_x", "dev_y", "dev_z")) %>%
  group_by(
    behavior,
    measure
  ) %>%
  summarise(
    mean_RMSE = mean(RMSE),
    sd = sd(RMSE),
    .groups = "drop"
  ) %>%
  mutate(
    behavior = factor(
      behavior,
      levels = c(
        "Cubes",
        sort(setdiff(unique(behavior), "Cubes"))
      )
    )
  )


meanRmseBar <- ggplot(
  behavior_summary,
  aes(
    x = behavior,
    y = mean_RMSE,
    fill = measure
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "",
    x = "Behavior",
    y = "Mean RMSD (m)",
    fill = "Dimensions"
  ) +
  scale_fill_viridis_d(
    labels = c("x", "y", "z")
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    expand = c(0,0)
  ) +
  theme_flight() +
  theme(
    text = element_text(
      family = "sans",
      size = 12
    ),
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 10
    ),
    axis.title.x = element_text(
      size = 11
    ),
    axis.title.y = element_text(
      size = 11
    ),
    legend.text = element_text(
      size = 10
    ),
    legend.title = element_text(
      size = 11
    )
  )

### (Foraging Flights Only) ###
# Radar Error vs. Altitude - Does radar bias change with bird altitude during foraging flights?
# Bin altitudes and compute the mean signed error ± 95% confidence interval
foraging_height <-
  all_behaviors %>%
  filter(behavior == "Foraging") %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  )

height_summary <-
  foraging_height %>%
  mutate(
    height_bin = floor(z__drone / 5) * 5
  ) %>%
  group_by(axis, height_bin) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    n = n(),
    se = sd_error / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

heightPlot <- ggplot(
  height_summary,
  aes(x = height_bin, y = mean_error)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(
    aes(
      ymin = mean_error - 1.96 * se,
      ymax = mean_error + 1.96 * se
    ),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point() +
  #facet_wrap(~axis) +
  facet_wrap(
    ~axis,
    labeller = labeller(axis = c(
      dev_x = "A. X Deviation",
      dev_y = "B. Y Deviation",
      dev_z = "C. Z Deviation"
    ))
  ) +
  labs(
    x = "Altitude (m)",
    y = "Mean Signed Deviation (m)", #Old"Mean Signed Error (m)",
    title = "",#OLD "Foraging Flights: Radar Bias vs Altitude"
  ) +
  scale_fill_viridis_d(name = "Mean Deviation") +
  theme_flight()

# ------------------------------------------------------------------------------
# Radar Error vs. Altitude: every behavior plus an all-behaviors comparison
# ------------------------------------------------------------------------------

# Repeat the exact Foraging preparation above while retaining behavior as a
# grouping variable. This produces comparable 5 m altitude bins and 95%
# confidence intervals for Cube, Foraging, Chasing, Soaring, and Transiting.
height_summary_by_behavior <-
  all_behaviors %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  ) %>%
  mutate(
    height_bin = floor(z__drone / 5) * 5
  ) %>%
  group_by(behavior, axis, height_bin) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    n = sum(!is.na(error)),
    se = sd_error / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

plot_height_by_behavior <- function(data, plot_title = "") {
  ggplot(
    data,
    aes(x = height_bin, y = mean_error)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(
        ymin = mean_error - 1.96 * se,
        ymax = mean_error + 1.96 * se
      ),
      alpha = 0.2
    ) +
    geom_line() +
    geom_point() +
    facet_wrap(
      ~axis,
      labeller = labeller(axis = c(
        dev_x = "A. X Deviation",
        dev_y = "B. Y Deviation",
        dev_z = "C. Z Deviation"
      ))
    ) +
    labs(
      x = "Altitude (m)",
      y = "Mean Signed Deviation (m)",
      title = plot_title
    ) +
    theme_flight()
}

behavior_names <- sort(unique(height_summary_by_behavior$behavior))

heightPlotsByBehavior <- setNames(
  lapply(
    behavior_names,
    function(selected_behavior) {
      plot_height_by_behavior(
        height_summary_by_behavior %>%
          filter(behavior == selected_behavior),
        paste(selected_behavior, "Flights")
      )
    }
  ),
  behavior_names
)

# Explicit object names make the plots easy to inspect interactively. heightPlot
# remains the original Foraging-only object for backward compatibility.
heightPlot_cube <- heightPlotsByBehavior[["Cube"]]
heightPlot_foraging <- heightPlotsByBehavior[["Foraging"]]
heightPlot_chasing <- heightPlotsByBehavior[["Chasing"]]
heightPlot_soaring <- heightPlotsByBehavior[["Soaring"]]
heightPlot_transiting <- heightPlotsByBehavior[["Transiting"]]

# A single comparison plot overlays behavior-specific trends within each axis.
heightPlot_all_behaviors <- ggplot(
  height_summary_by_behavior,
  aes(
    x = height_bin,
    y = mean_error,
    color = behavior,
    fill = behavior
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(
    aes(
      ymin = mean_error - 1.96 * se,
      ymax = mean_error + 1.96 * se
    ),
    alpha = 0.08,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  facet_wrap(
    ~axis,
    labeller = labeller(axis = c(
      dev_x = "A. X Deviation",
      dev_y = "B. Y Deviation",
      dev_z = "C. Z Deviation"
    ))
  ) +
  scale_color_viridis_d(name = "Behavior") +
  scale_fill_viridis_d(name = "Behavior") +
  labs(
    x = "Altitude (m)",
    y = "Mean Signed Deviation (m)",
    title = "Radar Deviation vs. Altitude Across Behaviors"
  ) +
  theme_flight()

# Optional linear model used to test the interaction between distance to radar and altitude on radar error
# foraging_model_data <-
#   all_behaviors %>%
#   filter(behavior == "Foraging") %>%
#   pivot_longer(
#     cols = c(dev_x, dev_y, dev_z),
#     names_to = "axis",
#     values_to = "error"
#   )
# model <- lm(
#   error ~ distance_to_radar * z__drone,
#   data = foraging_model_data
# )

# Radar Bias and Accuracy Landscapes - How does radar performance change across both distance to radar and altitude
# Compute mean signed error (bias)
# Compute mean absolute error (accuracy)
## Figure 9

# foraging_landscape <-
#   all_behaviors %>%
#   filter(behavior == "Foraging") %>%
#   pivot_longer(
#     cols = c(dev_x, dev_y, dev_z),
#     names_to = "axis",
#     values_to = "error"
#   ) %>%
#   mutate(
#     dist_bin = floor(distance_to_radar / 25) * 25,
#     height_bin = floor(z__drone / 10) * 10
#   )
# bias_surface <-
#   foraging_landscape %>%
#   group_by(axis, dist_bin, height_bin) %>%
#   summarise(
#     value = mean(error, na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   filter(n >= 10)
# accuracy_surface <-
#   foraging_landscape %>%
#   group_by(axis, dist_bin, height_bin) %>%
#   summarise(
#     value = mean(abs(error), na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   filter(n >= 10)
# plot_surface <- function(data, title) {
#   ggplot(data, aes(x = dist_bin, y = height_bin, fill = value)) +
#     geom_tile() +
#     #facet_wrap(~axis) +
#     facet_wrap(
#       ~axis,
#       labeller = labeller(axis = c(
#         dev_x = "A. X Deviation",
#         dev_y = "B. Y Deviation",
#         dev_z = "C. Z Deviation"
#       ))
#     ) +
#     scale_fill_viridis_c(
#       option = "A",
#       name = "Mean Deviation (m)"
#     ) +
#     #scale_fill_gradient2(
#     #  low = "blue",
#     #  mid = "white",
#     #  high = "red",
#     #  midpoint = 0
#     #) +
#     labs(
#       x = "Distance to Radar (m)",
#       y = "Altitude (m)",
#       #fill = "Mean Error",
#       title = title
#     ) +
#     theme_flight()
# }
# biasPlot <- plot_surface(
#   bias_surface,
#   #"Foraging Flights: Radar Bias (Signed Error)"
#   #"Foraging Flights: Radar Bias (Signed Deviation)"
#   ""
# )
foraging_landscape <-
  all_behaviors %>%
  filter(
    behavior == "Foraging",
    distance_to_radar > 60
  ) %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  ) %>%
  mutate(
    dist_bin = floor(distance_to_radar / 25) * 25,
    height_bin = floor(z__drone / 10) * 10
  )

bias_surface <-
  foraging_landscape %>%
  group_by(axis, dist_bin, height_bin) %>%
  summarise(
    value = mean(error, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

accuracy_surface <-
  foraging_landscape %>%
  group_by(axis, dist_bin, height_bin) %>%
  summarise(
    value = mean(abs(error), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 10)
panel_labels <- data.frame(
  axis = c("dev_x", "dev_y", "dev_z"),
  label = c("(A)", "(B)", "(C)")
)

plot_surface <- function(data, title) {
  ggplot(
    data,
    aes(
      x = dist_bin,
      y = height_bin,
      fill = value
    )
  ) +
    geom_tile() +

    facet_wrap(~axis, ncol = 1) +

    geom_text(
      data = panel_labels,
      aes(
        x = -Inf,
        y = Inf,
        label = label
      ),
      hjust = -0.2,
      vjust = 1.3,
      inherit.aes = FALSE,
      size = 4.5,
        family = "sans"
    ) +

    scale_fill_viridis_c(
      option = "D",
      name = "Mean PD",
      guide = guide_colorbar(
        barheight = unit(2.5, "cm"),
        barwidth = unit(0.4, "cm")
      )
    ) +
    scale_x_continuous(
      limits = c(0,1400),
      breaks = seq(0, 1400, by=100),
      expand = c(0,0)
    ) +

    scale_y_continuous(
      limits = c(70, 130),
      expand = c(0, 0)
    ) +

    labs(
      x = "Distance from Radar (m)",
      y = "Altitude (m)",
      title = title
    ) +

    theme_flight() +

    theme(
      text = element_text(
        family = "sans",
        size = 12
      ),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      axis.text.x = element_text(
        size = 10,
        angle = 45,
        hjust = 1,
      ),
      axis.text.y = element_text(
        size = 10,
        margin = margin(r=5)
      ),
      axis.title.x = element_text(
        size = 11
      ),
      axis.title.y = element_text(
        size = 11
      ),
      legend.title = element_text(
        size = 11,
        hjust = 0.5
      ),
      legend.text = element_text(size = 10),
      legend.margin = margin(0, 0, 0, 0)

    )
}

biasPlot <- plot_surface(
  bias_surface,
  ""
)

# ------------------------------------------------------------------------------
# Distance-by-altitude bias surfaces for every behavior
# ------------------------------------------------------------------------------
######################### JPS: START QC OF THE BIAS PLOTS... NOT SURE WHY THEY FILTER FOR DISTANCE TO RADAR >60 ##########
####################### CHECK OUT THE QC STEPS IN THE SCRIPT #########
# Use the same 25 m distance bins, 10 m altitude bins, signed deviations, minimum
# distance filter, and minimum cell sample size as the original Foraging surface.
all_behavior_landscape <-
  all_behaviors %>%
  filter(distance_to_radar > 60) %>%
  pivot_longer(
    cols = c(dev_x, dev_y, dev_z),
    names_to = "axis",
    values_to = "error"
  ) %>%
  mutate(
    dist_bin = floor(distance_to_radar / 25) * 25,
    height_bin = floor(z__drone / 10) * 10
  )

bias_surface_by_behavior <-
  all_behavior_landscape %>%
  group_by(behavior, axis, dist_bin, height_bin) %>%
  summarise(
    value = mean(error, na.rm = TRUE),
    n = sum(!is.na(error)),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

# Pooling across behavior gives the requested overall surface. This is a
# descriptive summary of all observations, so behaviors with more points have
# greater influence on a cell's mean.
bias_surface_all_behaviors <-
  all_behavior_landscape %>%
  group_by(axis, dist_bin, height_bin) %>%
  summarise(
    value = mean(error, na.rm = TRUE),
    n = sum(!is.na(error)),
    .groups = "drop"
  ) %>%
  filter(n >= 10)

# The original Foraging figure used altitude limits of 70-130 m. The full data
# span approximately 35-140 m, so these generalized figures use one wider,
# consistent scale to avoid clipping Cube and Chasing observations.
plot_general_bias_surface <- function(data, plot_title = "") {
  ggplot(
    data,
    aes(
      x = dist_bin,
      y = height_bin,
      fill = value
    )
  ) +
    geom_tile() +
    facet_wrap(~axis, ncol = 1) +
    geom_text(
      data = panel_labels,
      aes(x = -Inf, y = Inf, label = label),
      hjust = -0.2,
      vjust = 1.3,
      inherit.aes = FALSE,
      size = 4.5,
      family = "sans"
    ) +
    scale_fill_viridis_c(
      option = "D",
      name = "Mean PD",
      guide = guide_colorbar(
        barheight = unit(2.5, "cm"),
        barwidth = unit(0.4, "cm")
      )
    ) +
    scale_x_continuous(
      limits = c(0, 1400),
      breaks = seq(0, 1400, by = 100),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(35, 140),
      expand = c(0, 0)
    ) +
    labs(
      x = "Distance from Radar (m)",
      y = "Altitude (m)",
      title = ""
    ) +
    theme_flight() +
    theme(
      text = element_text(family = "sans", size = 12),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.spacing = unit(0.7, "lines"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10, margin = margin(r = 5)),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      legend.title = element_text(size = 11, hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.margin = margin(0, 0, 0, 0)
    )
}

biasPlotsByBehavior <- setNames(
  lapply(
    behavior_names,
    function(selected_behavior) {
      plot_general_bias_surface(
        bias_surface_by_behavior %>%
          filter(behavior == selected_behavior),
        paste(selected_behavior, "Flights")
      )
    }
  ),
  behavior_names
)

biasPlot_cube <- biasPlotsByBehavior[["Cube"]]
biasPlot_foraging <- biasPlotsByBehavior[["Foraging"]]
biasPlot_chasing <- biasPlotsByBehavior[["Chasing"]]
biasPlot_soaring <- biasPlotsByBehavior[["Soaring"]]
biasPlot_transiting <- biasPlotsByBehavior[["Transiting"]]

biasPlot_all_behaviors <- plot_general_bias_surface(
  bias_surface_all_behaviors,
  "All Behaviors"
)

# This optional comparison grid retains behavior rather than pooling it. A fixed
# fill range across all panels makes colors directly comparable.
shared_bias_limit <- max(
  abs(bias_surface_by_behavior$value),
  na.rm = TRUE
)

comparison_behavior_levels <- c(
  "Cubes",
  sort(setdiff(
    unique(as.character(bias_surface_by_behavior$behavior)),
    "Cube"
  ))
)

bias_surface_behavior_comparison <-
  bias_surface_by_behavior %>%
  mutate(
    behavior = if_else(
      as.character(behavior) == "Cube",
      "Cubes",
      as.character(behavior)
    ),
    behavior = factor(behavior, levels = comparison_behavior_levels)
  )

comparison_panel_labels <- data.frame(
  behavior = factor("Cubes", levels = comparison_behavior_levels),
  axis = c("dev_x", "dev_y", "dev_z"),
  label = c("(A)", "(B)", "(C)")
)

biasPlot_behavior_comparison <- ggplot(
  bias_surface_behavior_comparison,
  aes(x = dist_bin, y = height_bin, fill = value)
) +
  geom_tile() +
  facet_grid(behavior ~ axis) +
  geom_text(
    data = comparison_panel_labels,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.2,
    vjust = 1.3,
    inherit.aes = FALSE,
    size = 4.5,
    family = "sans"
  ) +
  scale_fill_viridis_c(
    option = "D",
    limits = c(-shared_bias_limit, shared_bias_limit),
    name = "Mean PD"
  ) +
  scale_x_continuous(
    limits = c(0, 1400),
    breaks = seq(0, 1400, by = 200),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(35, 140),
    expand = c(0, 0)
  ) +
  labs(
    x = "Distance from Radar (m)",
    y = "Altitude (m)",
    title = ""
  ) +
  theme_flight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_blank(),
    strip.background.x = element_blank()
  )

accuracyPlot <- ggplot(accuracy_surface, aes(x = dist_bin, y = height_bin, fill = value)) +
  geom_tile() +
  #facet_wrap(~axis) +
  facet_wrap(
    ~axis,
    labeller = labeller(axis = c(
      dev_x = "A. X Deviation",
      dev_y = "B. Y Deviation",
      dev_z = "C. Z Deviation"
    ))
  ) +
  scale_fill_viridis_c(
    name = "Mean Abs Deviation (m)"
  ) +
  #scale_fill_gradient(
  #  low = "white",
  #  high = "red"
  #) +
  labs(
    x = "Distance to Radar (m)",
    y = "Altitude (m)",
    #fill = "Mean Abs Error",
    title = "Foraging Flights: Radar Accuracy (Magnitude)"
  ) +
  theme_flight()

#combinedPlot <- biasPlot / accuracyPlot
combinedPlot <- biasPlot
#JPS Edit - Grace only wants biasPlot
combinedPlot



  .capture_new_objects(before, environment())
}
