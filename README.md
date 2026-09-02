# Drone-Radar Flight Analysis

This repository accompanies the manuscript *Systematic validation of 3D ground-based radars for tracking birds and bats* and contains the reproducible code and supporting materials used for the analyses presented in the paper. The project evaluates 3D ground-based radar performance against controlled drone flights with known positions and trajectories, using radar detections and corresponding drone positions collected during field studies at the Jack and Laura Dangermond Preserve and Morro Bay, California. The repository is intended to support transparency, reproducibility, and reuse of the analytical workflow described in the paper.

The repository contains two intentionally distinct analyses:

* **Detection rate:** Across all eligible drone-based detection opportunities, how often did the radar detect the drone?
* **Positional discrepancies:** When the radar detected the drone, how different was the radar-estimated position from the corresponding drone position?

The repository contains code for preprocessing radar detections and drone positions, target filtering, coordinate transformation, and radar-drone alignment using methods appropriate to each analysis, as well as calculation of detection rate and positional discrepancy metrics and generation of the figures and summary outputs used in the manuscript. The repository provides the code needed to reproduce the main analyses and examine how radar and drone data were processed and aligned for quantitative comparison. The workflow may also provide a useful starting point for similar validation studies involving biological radar, drone-based reference targets, or other remote sensing systems used for ecological monitoring and collision risk assessment.

## Repository structure

- `config/analysis_config.yml` - reviewer-facing analysis settings.
- `metadata/flights.csv` - authoritative flight registry, relative input paths, timing settings, and inclusion flags.
- `R/` - reusable loading, analysis, bundle, statistics, plotting, and output functions.
- `scripts/` - runnable analysis, figure, validation, and full-pipeline entry points.
- `derived_data/` - documentation and locally generated intermediate RDS bundles; RDS files are ignored by Git.
- `data/README.md` - raw-data arrangement and availability instructions; raw files are ignored by Git.

Generated files are written under `output/`, which is intentionally ignored by Git because all results can be regenerated.

## Reproducible R environment

The project uses `renv` and records package versions in `renv.lock`. From a fresh clone, install `renv` if necessary and restore the environment:

```r
install.packages("renv")
renv::restore()
```

The lockfile was generated with R 4.5.1. System libraries required by packages such as `sf` may also be needed on non-Windows systems.

## Data setup

Raw data are not stored in GitHub. Arrange the required files under `data/` using the relative paths in `metadata/flights.csv`. See `DATA_AVAILABILITY.md` and `data/README.md`.

Check the repository and all local input paths before a full run:

```powershell
Rscript "scripts\check_repository.R" --require-data
```

Without local raw data, the structural checks used by GitHub Actions can be run with:

```powershell
Rscript "scripts\check_repository.R"
```

## Configuration

Edit `config/analysis_config.yml` before a full run. The checked-in defaults include:

- 9090 cone of silence: 300 m
- 7360 cone of silence: 300 m
- 9090 positional Y offset: 20 m
- 7360 positional Y offset: 0 m
- Detection rate temporal windows: 1-5 seconds
- Temporal window shift fraction: 0.5
- Drone evaluation interval: 0.1 seconds

## Run the complete analysis

From the repository root:

```powershell
Rscript "scripts\run_all_analyses.R"
```

This validates the repository, runs the detection-rate analysis, runs the positional-discrepancy analysis, and creates the paper figures. The runner keeps the two radar-drone data alignment methods separate.

Individual entry points are also available:

```powershell
Rscript "scripts\detection_rate_analysis.R"
Rscript "scripts\detection_rate_figures.R"
Rscript "scripts\positional_discrepancy_analysis.R"
Rscript "scripts\positional_discrepancy_figures.R"
```

Selected result paths and radar parameters can also be overridden with the command-line options documented in the analysis scripts.

## Generated outputs

- `output/detection_rate/` - detection-rate figures and summary tables.
- `output/positional_discrepancy/` - positional-discrepancy figures.
- `output/detection_rate_qc/` - independent QC outputs.
- `derived_data/detection_rate_results.rds` - consolidated detection-rate intermediate results.
- `derived_data/positional_discrepancy_results.rds` - consolidated positional-discrepancy intermediate results.

These products remain local and are ignored by Git. The preserved baseline PDFs under `validation/` are intentionally tracked.

## TODO

Two author decisions remain:

1. Select and add the code license. MIT is a common permissive option, but the copyright holder must approve it.
2. Add the final paper title, author list, DOI or preprint link, and approved data repository/access statement to a `CITATION.cff` file and `DATA_AVAILABILITY.md`.

Before final submission tentatively update these two things.
