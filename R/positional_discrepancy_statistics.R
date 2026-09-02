# Reusable positional-discrepancy paper-figure functions.

calculate_positional_discrepancy_statistics <- function(figure_data) {
  list2env(figure_data, envir = environment())
########################################
# Statistical Analysis
########################################
#print(summary(model))

# Overall radar accuracy - check if radar tracking accuracy varies depending on flight behavior
# Use Kruskal-Wallis model
behavior_test <- all_rmse %>%
  filter(measure %in% c("dev_x","dev_y","dev_z"))

kruskal_overall <- kruskal.test(RMSE ~ behavior, data = behavior_test)
kruskal_x <- kruskal.test(RMSE ~ behavior, data = filter(all_rmse, measure=="dev_x"))
kruskal_y <- kruskal.test(RMSE ~ behavior, data = filter(all_rmse, measure=="dev_y"))
kruskal_z <- kruskal.test(RMSE ~ behavior, data = filter(all_rmse, measure=="dev_z"))

kruskal_results <- data.frame(
  Component = c("All Components","X","Y","Z"),
  Chi_squared = c(kruskal_overall$statistic, kruskal_x$statistic, kruskal_y$statistic, kruskal_z$statistic),
  df = c(kruskal_overall$parameter, kruskal_x$parameter, kruskal_y$parameter, kruskal_z$parameter),
  p_value = c(kruskal_overall$p.value, kruskal_x$p.value, kruskal_y$p.value, kruskal_z$p.value)
)

# Distance Effect - check if radar error increases as distance from radar increases
# use linear mixed regression model
distance_axis_test <- all_behaviors %>%
  pivot_longer(cols = c(dev_x, dev_y, dev_z), names_to = "axis", values_to = "error") %>%
  mutate(abs_error = abs(error))

distance_lmm_results <-
  distance_axis_test %>%
  group_by(axis) %>%
  summarise(
    model = list(lmer(abs_error ~ distance_to_radar + (1 | unique_flight), data = cur_data())), .groups = "drop") %>%
  rowwise() %>%
  summarise(
    axis = axis,
    slope = fixef(model)[2],
    intercept = fixef(model)[1],
    p_value = coef(summary(model))[2, "Pr(>|t|)"]
  )

# Speed Effect - check if radar error increases as drone speed increases(for each error component)
# Run linear mixed regression model
speed_data <-
  all_behaviors %>%
  pivot_longer(cols = c(dev_x, dev_y, dev_z), names_to = "axis", values_to = "error") %>%
  mutate(abs_error = abs(error))

speed_models <- speed_data %>%
  group_by(axis) %>%
  summarise(model = list(lmer(abs_error ~ speed + (1 | unique_flight), data = cur_data())), .groups = "drop")

speed_results <- speed_models %>%
  rowwise() %>%
  summarise(
    axis = axis,
    slope = fixef(model)["speed"],
    intercept = fixef(model)["(Intercept)"],
    p_value = summary(model)$coefficients["speed", "Pr(>|t|)"],
    .groups = "drop"
  )

# Foraging Height - check if radar error increases as drone height increases
# use linear mixed-effects regression model
height_test <- all_behaviors %>%
  filter(behavior == "Foraging") %>%
  pivot_longer(cols = c(dev_x, dev_y, dev_z), names_to = "axis", values_to = "error") %>%
  mutate(abs_error = abs(error))

height_models <- height_test %>%
  group_by(axis) %>%
  summarise(model = list(lmer(abs_error ~ z__drone + (1 | unique_flight), data = cur_data())), .groups = "drop")

height_results <- height_models %>%
  rowwise() %>%
  summarise(
    axis = axis,
    slope = fixef(model)[2],
    intercept = fixef(model)[1],
    p_value = coef(summary(model))[2, "Pr(>|t|)"]
  )

message("Kruskal-Wallis Test: Radar Accuracy by Flight Behavior")
print(kruskal_results)

message("Linear Mixed-Effects Regression: Distance Effect")
print(distance_lmm_results)

message("Linear Mixed-Effects Regression: Speed Effect")
print(speed_results)

message("Linear Mixed-Effects Regression: Foraging Altitude Effect")
print(height_results)


  list(
    kruskal_wallis = kruskal_results,
    distance_mixed_model = distance_lmm_results,
    speed_mixed_model = speed_results,
    foraging_altitude_mixed_model = height_results
  )
}
