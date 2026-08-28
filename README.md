# Drone-Radar Flight Analysis

This repository contains the analysis code for evaluating radar detection and positional accuracy using paired drone and radar flight tracks.

The repository contains two intentionally distinct analyses:

- **Detection rate:** Across all eligible drone-based detection opportunities, how often did the radar detect the drone?
- **Positional deviation:** When the radar detected the drone, how close was the radar-estimated position to the drone position?

The temporal alignment methods differ by design and must not be substituted for one another.

## Repository structure

- `config/analysis_config.yml` - reviewer-facing analysis settings.
- `metadata/flights.csv` - authoritative flight registry, relative input paths, timing settings, and inclusion flags.
- `R/` - reusable loading, analysis, bundle, statistics, plotting, and output functions.
- `scripts/` - runnable analysis, figure, validation, and full-pipeline entry points.
- `derived_data/` - documentation and locally generated intermediate RDS bundles; RDS files are ignored by Git.
- `validation/baseline_figures/` - 12 preserved pre-restructure positional PDFs for final comparison.
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

Edit `config/analysis_config.yml` before a full run. The checked-in defaults reproduce the validated canonical analyses, including:

- 9090 cone of silence: 300 m
- 7360 cone of silence: 150 m
- 9090 positional Y offset: 20 m
- 7360 positional Y offset: 0 m
- Detection bin widths: 1-5 seconds
- Shifted-bin fraction: 0.5
- Drone evaluation interval: 0.1 seconds

The approved positional alignment and drone-based detection-opportunity method are fixed in code and recorded in result bundles; they are not user-adjustable settings.

## Run the complete analysis

From the repository root:

```powershell
Rscript "scripts\run_all_analyses.R"
```

This validates the repository, runs the detection-rate analysis, runs the positional-deviation analysis, and creates the paper figures. The runner keeps the two alignment methods separate.

Individual entry points are also available:

```powershell
Rscript "scripts\detection_rate_analysis.R"
Rscript "scripts\detection_rate_figures.R"
Rscript "scripts\positional_deviation_analysis.R"
Rscript "scripts\positional_deviation_figures.R"
```

Selected result paths and radar parameters can also be overridden with the command-line options documented in the analysis scripts.

## Generated outputs

- `output/detection_rate/` - detection-rate figures and summary tables.
- `output/positional_deviation/` - positional-deviation figures.
- `output/detection_rate_qc/` - independent QC outputs.
- `derived_data/detection_rate_results.rds` - consolidated detection-rate intermediate results.
- `derived_data/positional_deviation_results.rds` - consolidated positional-deviation intermediate results.

These products remain local and are ignored by Git. The preserved baseline PDFs under `validation/` are intentionally tracked.

## Validation status

The restructured analyses were checked against `flight_analysis_SOURCE_OF_TRUTH_2026_08_28` and the validated canonical bundles. Positional paired data, RMSE tables, and flight metadata matched exactly. Detection-rate analytical objects matched exactly apart from expected run timestamps. The comparable uniform-300 m detection summary also matched the source-of-truth CSV hash.

GitHub Actions runs the structural repository checks on every push and pull request.

## Before making the repository public

Two author decisions remain:

1. Select and add the code license. MIT is a common permissive option, but the copyright holder must approve it.
2. Add the final paper title, author list, DOI or preprint link, and approved data repository/access statement to a `CITATION.cff` file and `DATA_AVAILABILITY.md`.

Do not make the repository public until data-sharing permissions and location-information disclosure have been reviewed by the authors and PI.
