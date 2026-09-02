# Raw Data

Raw radar and drone files are not tracked in Git because the local data directory is approximately 10.6 GB and contains files larger than GitHub's 100 MB per-file limit.

The source datasets used in these analyses are archived in the Knowledge Network for Biocomplexity (KNB):

* **Chang, Grace. 2024.** *Integrated, multiscale bird and bat monitoring system technology testing, Government Point, 26–30 August 2024*. Knowledge Network for Biocomplexity. URN: `urn:uuid:c745abb8-f51f-4c6b-8184-5caedb9cb918`.
* **Chang, Grace. 2026.** *Application of Radar Technologies for Classifying Bird Flight Behavior at Dangermond Preserve, January 5–10, 2026*. Knowledge Network for Biocomplexity. URN: `urn:uuid:f5c52336-850f-436d-b917-8f33716b7422`.

To reproduce the analyses, obtain the required data from the archived datasets and place the required files in the `data/` directory using the relative paths recorded in `metadata/flights.csv`. The metadata registry is the authoritative manifest for the required radar, drone, and radar-location files.

The repository does not redistribute the raw data; the KNB records above provide the authoritative data citations and source locations.
