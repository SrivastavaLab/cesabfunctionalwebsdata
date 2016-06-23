## Master file for recreating the data

## first you have to log in. If you don't have a BWG database password then you can't have the data!

library(bwgdata)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

bwg_auth()

## this is very slow! run at your own risk
# source("R/01_accessing_data.R")

source("R/02_summarizing_data.R")

### ...

source("R/99_merging.R")

## preparing a release

library(datastorr)

github_release_info("SrivastavaLab/cesabfunctionalwebsdata", readRDS)
