# Run the complete analysis from the project root.
# The two pipelines intentionally retain different alignment methods.

run_script <- function(script, arguments = character()) {
  rscript <- file.path(R.home("bin"), "Rscript")
  message("\nRunning ", script, "...")
  status <- system2(rscript, c(script, arguments))
  if (!identical(status, 0L)) stop("Analysis step failed: ", script, call. = FALSE)
}

run_script(file.path("scripts", "check_repository.R"), "--require-data")

# Detection opportunities: drone-based complete time windows, including misses.
run_script(file.path("scripts", "detection_rate_analysis.R"))

run_script(file.path('scripts', 'detection_rate_figures.R'))

# Positional discrepancy: approved common-grid alignment for radar detections only.
run_script(file.path("scripts", "positional_discrepancy_analysis.R"))
run_script(file.path("scripts", "positional_discrepancy_figures.R"))

message("\nAll analyses completed successfully.")
