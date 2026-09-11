# Reusable positional-discrepancy paper-figure functions.

save_positional_discrepancy_figures <- function(
    plot_objects,
    output_dir = project_path("output", "positional_discrepancy")) {
  list2env(plot_objects, envir = environment())
########################################
# Saving Plots
########################################

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


ggsave(
  filename = file.path(output_dir, "figure_5_rmsd_by_behavior.pdf"),
  plot = rmseByBehav +
    theme(legend.position = "none"),
  width = 5,
  height = 7,
  dpi = 600,
  device = cairo_pdf
  )


ggsave(
  filename = file.path(output_dir, "figure_6_mean_rmsd_by_behavior.pdf"),
  plot = meanRmseBar,
  width = 6.3,
  height = 4,
  units = "in",
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  filename = file.path(output_dir, "figure_7_deviation_distribution.pdf"),
  plot = deviationDistribution,
  width=6.3,
  height=4,
  units = "in",
  dpi=600,
  device = cairo_pdf)

ggsave(
  filename = file.path(output_dir, "figure_8_deviation_by_distance.pdf"),
  plot = deviationByDistance,
  width = 8,
  height = 7,
  units = "in",
  dpi=600,
  device = cairo_pdf)

# Foraging positional-deviation surface using the paper's reference range
ggsave(
  filename = file.path(output_dir, "mean_positional_deviation_foraging_reference_range.pdf"),
  plot = biasPlot,
  width = 8,
  height = 7,
  units = "in",
  dpi=600,
  device = cairo_pdf)

# Save the additional positional-deviation surfaces using the same settings as the
# original Foraging-only figure above.
positional_deviation_surface_plots <- list(
  mean_positional_deviation_cubes.pdf = biasPlot_cube,
  mean_positional_deviation_foraging_full_range.pdf = biasPlot_foraging,
  mean_positional_deviation_chasing.pdf = biasPlot_chasing,
  mean_positional_deviation_soaring.pdf = biasPlot_soaring,
  mean_positional_deviation_transiting.pdf = biasPlot_transiting,
  mean_positional_deviation_all_behaviors.pdf = biasPlot_all_behaviors,
  figure_9_mean_positional_deviation_by_behavior.pdf = biasPlot_behavior_comparison
)

invisible(
  Map(
    function(filename, plot) {
      ggsave(
        filename = file.path(output_dir, filename),
        plot = plot,
        width = 8,
        height = 7,
        units = "in",
        dpi = 600,
        device = cairo_pdf
      )
    },
    names(positional_deviation_surface_plots),
    positional_deviation_surface_plots
  )
)





  invisible(output_dir)
}
