# Raw data

This directory contains the unmodified radar exports, drone flight logs, and
radar-location files used by the analysis. There are 85 paired flights: 6 cube
flights collected in 2024 and 79 behavioral flights collected in 2026.

The raw files are not tracked in Git because the local data directory is
approximately 10.6 GB and contains files larger than GitHub's 100 MB per-file
limit. Source-dataset citations and repository identifiers are temporarily
omitted for anonymous peer review and will be restored before public release.

## Authoritative manifest

`metadata/flights.csv` is the authoritative manifest. Each row represents one
analytical flight and records its paired `radar_file`, `drone_file`, and
`radar_location_file`, along with the analysis time window, drone row indices,
inclusion flags, and any exclusion notes. Analysis scripts should obtain file
paths from this manifest rather than discover files by directory listing.

All manifest paths are relative to the repository root and use forward slashes,
so the workflow is portable across operating systems.

## Directory structure

```text
data/
|-- 2024/
|   |-- radar/cube/
|   |-- drone/mavic_enterprise_3/cube/
|   `-- Dangermond_RadarLocation_govtpt_2024_08_27.txt
`-- 2026/
    |-- radar/
    |   |-- chasing/
    |   |-- dynamic_soaring/
    |   |-- foraging/
    |   `-- transiting/
    |-- drone/
    |   |-- air2/{chasing,foraging}/
    |   `-- air3/{chasing,dynamic_soaring,transiting}/
    `-- Dangermond_RadarLocation_school_2026_01_05_v2.txt
```

The hierarchy is `data/<year>/<sensor>/...`. Radar files are grouped directly
by flight behavior. Drone files include an additional aircraft directory
because more than one aircraft was used in 2026. The labels `air2` and `air3`
are retained source-project aircraft identifiers; `mavic_enterprise_3` is the
identifier used for the 2024 aircraft.

## Naming conventions

- Directory names use lowercase snake case, including behavior names such as
  `dynamic_soaring`.
- The controlled behaviors are `cube`, `chasing`, `dynamic_soaring`,
  `foraging`, and `transiting`.
- Raw filenames retain their source/export names to preserve provenance. Their
  capitalization is therefore not a directory-naming convention.
- `Final_noOmittedPoints` identifies the supplied radar export in which no
  points were omitted; it is intentionally retained as part of the source
  filename. The manifest determines which file is used.
- Radar-location files are stored once at the corresponding year level because
  all listed flights at that site/year reference the same location file.

The `behavior` and `flight_id` values in the manifest retain established
analysis labels (for example, `DynoSoaring_01`) for compatibility with analysis
objects and outputs. These identifiers need not match directory capitalization.
