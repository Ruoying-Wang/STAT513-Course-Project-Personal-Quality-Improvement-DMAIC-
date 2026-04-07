# STAT 513 Course Project: Personal Quality Improvement (DMAIC)

This repository contains the R script used to generate figures (Fig. A1–A7) for the STAT 513 course project report.

## Files
- `analysis.R`: One-click script that reads the Y/N dataset and exports Fig_A1–Fig_A7 as PNG files.

## How to run
1. Open `analysis.R` in R/RStudio.
2. Update `file_path` and `out_dir` at the top of the script.
3. Run:
   ```r
   source("analysis.R")

## Data note
The raw dataset is not included in this public repository for privacy.

## Output
Running `source("analysis.R")` will export:
Fig_A1_Bar.png ... Fig_A7_EWMA.png
