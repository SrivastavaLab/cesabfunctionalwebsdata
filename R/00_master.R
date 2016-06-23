## Master file for recreating the data

## first you have to log in. If you don't have a BWG database password then you can't have the data!

library(bwgdata)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

bwg_auth()

source("R/01_accessing_data.R")
