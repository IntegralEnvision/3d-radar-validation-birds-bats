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
  filename = file.path(output_dir, "Figure5_rmseByBehav.pdf"),
  plot = rmseByBehav +
    theme(legend.position = "none"),
  width = 5,
  height = 7,
  dpi = 600,
  device = cairo_pdf
  )


ggsave(
  filename = file.path(output_dir, "Figure6_meanRmseBar.pdf"),
  plot = meanRmseBar,
  width = 6.3,
  height = 4,
  units = "in",
  dpi = 600,
  device = cairo_pdf
)

ggsave(
  filename = file.path(output_dir, "Figure7_errorDistrib.pdf"),
  plot = errorDistrib,
  width=6.3,
  height=4,
  units = "in",
  dpi=600,
  device = cairo_pdf)

ggsave(
  filename = file.path(output_dir, "Figure8_errorVsDist.pdf"),
  plot = errorVsDist,
  width = 8,
  height = 7,
  units = "in",
  dpi=600,
  device = cairo_pdf)

# Figure 9
ggsave(
  filename = file.path(output_dir, "Figure9_biasPlot.pdf"),
  plot = biasPlot,
  width = 8,
  height = 7,
  units = "in",
  dpi=600,
  device = cairo_pdf)

# Save the expanded Figure 9 plots using the same output settings as the
# original Foraging-only figure above.
figure9_expanded_plots <- list(
  Figure9_biasPlot_Cube.pdf = biasPlot_cube,
  Figure9_biasPlot_Foraging.pdf = biasPlot_foraging,
  Figure9_biasPlot_Chasing.pdf = biasPlot_chasing,
  Figure9_biasPlot_Soaring.pdf = biasPlot_soaring,
  Figure9_biasPlot_Transiting.pdf = biasPlot_transiting,
  Figure9_biasPlot_AllBehaviors.pdf = biasPlot_all_behaviors,
  Figure9_biasPlot_BehaviorComparison.pdf = biasPlot_behavior_comparison
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
    names(figure9_expanded_plots),
    figure9_expanded_plots
  )
)




# ggsave(file.path(output_dir, "componentError.png"), componentError, width=10, height=5, dpi=300)
# ggsave(file.path(output_dir, "heightPlot.png"), heightPlot, width=7, height=5, dpi=300)
# ggsave(file.path(output_dir, "combinedPlot.png"), combinedPlot, width=10, height=8, dpi=300)
# ggsave(file.path(output_dir, "speedPlot.png"), speedPlot, width = 10, height = 5, dpi = 300)

  invisible(output_dir)
}
