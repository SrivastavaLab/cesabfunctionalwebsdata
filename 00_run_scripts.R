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
# library(osfr)
# library(tidyverse)

download_now <- FALSE

# might need to delete a dataset, e.g. bromeliads, to force a download.
# this is necessary because Targets has no way of knowing if the database has changed!

# tar_delete(dats, store = "store_download_data")
# tar_delete(visits, store = "store_download_data")
# tar_delete(broms, store = "store_download_data")
# tar_delete(abs, store = "store_download_data")
# tar_delete(trts_all, store = "store_download_data")

Sys.setenv(BWG_BASEURL = "https://www.zoology.ubc.ca/~dashcc/bwgdb/v3/api/")
# Sys.unsetenv("BWG_BASEURL")

if(download_now){
  Sys.setenv(TAR_PROJECT = "project_download_data")
  tar_make()
}

Sys.setenv(TAR_PROJECT = "project_process_data")
tar_make("all_data", callr_function = NULL)


Sys.setenv(TAR_PROJECT = "project_observed_data")
tar_visnetwork(targets_only = TRUE)
tar_make()

Sys.setenv(TAR_PROJECT = "project_imputed_data_new")
tar_make()



# data release ------------------------------------------------------------

# pak::pkg_install("traitecoevo/datastorr")
# pak::pkg_install("SrivastavaLab/fwdata")
Sys.setenv(TAR_PROJECT = "project_imputed_data_new")
tar_load(imputed_data)
readr::write_rds(imputed_data, "all_data.rds")


library(fwdata)
fw_auth()  # sets GITHUB_TOKEN

datastorr::github_release_create(
  fw_info(),
  filename = "all_data.rds",
  description = "rerun with targets and updated workflow"
)

library(fwdata)
fw_auth()  # sets GITHUB_TOKEN

datastorr::github_release_create(
  fw_info(),
  filename = "path/to/all_data.rds",
  description = "brief description of what changed"
)
