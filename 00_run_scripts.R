## Reproducibly downloading and processing the Bromeliad working group data
## Andrew MacDonald
## Oct 2025

## this is the MAIN script that runs the other scripts.
## if you want to create a new release of the dataset, this is where you should work!

# the workflow is divided into two parts (which are called "projects" in targets).
# 01_download_data -- downloads the data
# 02_process_data -- processes, corrects and combines data

## The reason for this division is that data downloads should happen only when
## the BWG database has changed. Data processing, on the other hand, probably
## needs regular maintenance and updating of the code and the process

library(targets)

download_now <- FALSE

if(download_now){
  Sys.setenv(TAR_PROJECT = "project_download_data")
  tar_make()
}

Sys.setenv(TAR_PROJECT = "project_process_data")
tar_make()

## while working it is helpful to visualize the process.

tar_visnetwork(targets_only = TRUE, script = "01_download_data.R")


tar_visnetwork(targets_only = TRUE, script = "02_process_data.R")
