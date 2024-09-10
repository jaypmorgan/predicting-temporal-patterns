# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse", "ggplot2", "here", "dtw", "dendextend", "patchwork", "zoo", "kableExtra",
    "gt", "gtExtras", "clock", "lubridate", "scales", "caret", "yardstick", "ggpubr",
    "gtsummary")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("src/")

# Replace the target list below with your own:
list(
    tar_target(catalogue, create_catalogue("data/raw")),
    tar_target(data, prepare_data(load_from_catalogue(catalogue))),
    tar_target(mean_curves, create_mean_curves(data)),
    tar_target(dendogram, hierarchical_clustering(mean_curves)),
    tar_target(mot_curve, visualise_mean_curve(mean_curves, "MOT")),
    tar_target(classes, label_data(mean_curves)),
    tar_target(final_data, left_join(data, classes, by = join_by(keyword == keyword))),
    tar_target(keywords, list_keywords(final_data))
)
