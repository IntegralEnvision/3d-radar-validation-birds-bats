# Generate all positional-discrepancy paper figures and statistical summaries.
# Run from the project root after scripts/positional_discrepancy_analysis.R.

rm(list = ls())

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(plotly)
  library(ggplot2)
  library(gganimate)
  library(gifski)
  library(scales)
  library(ggh4x)
  library(patchwork)
  library(FSA)
  library(lme4)
  library(lmerTest)
})

source(file.path("R", "analysis_config.R"))
analysis_config <- load_analysis_config()
source(file.path("R", "flight_metadata.R"))
source(file.path("R", "positional_discrepancy_bundle.R"))
source(file.path("R", "positional_discrepancy_figure_data.R"))
source(file.path("R", "positional_discrepancy_plots.R"))
source(file.path("R", "positional_discrepancy_statistics.R"))
source(file.path("R", "positional_discrepancy_figure_output.R"))

positional_results <- read_positional_discrepancy_bundle()
figure_data <- prepare_positional_discrepancy_figure_data(positional_results)
plot_objects <- build_positional_discrepancy_plots(figure_data)
statistical_results <- calculate_positional_discrepancy_statistics(figure_data)
save_positional_discrepancy_figures(
  plot_objects,
  output_dir = project_path(analysis_config$outputs$positional_figures)
)
