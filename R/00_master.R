## Master file for recreating the data

## first you have to log in. If you don't have a BWG database password then you can't have the data!

library(bwgdata)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

# bwg_auth()

## this is very slow! run at your own risk

# 01. Downloads the data from the BWG database
#returns:data-raw/01_broms.csv, data-raw/01_abundance.csv, data-raw/01_datasets.csv
# data-raw/01_traits.csv, data-raw/01_visits.csv

# source("R/01_accessing_data.R")

# 02. This script summarizes the data
#input: data-raw/01_broms.csv, data-raw/01_traits.csv
#returns: data-raw/02_broms.csv,  data-raw/02_abundance.csv

source("R/02_summarizing_data.R")

# 04. The script will fill in missing values for max volume and total detritus in BWGdb data
#input: data-raw/01_broms.csv, data-raw/01_datasets.csv, data-raw/01_visits.csv
#returns: data-raw/vol_table.csv

source("R/04_allometric_eq.R")

# 07. This script combines trait data -- tachet vs other traitsl
#input: data-raw/01_traits.csv
#returns: data-raw/07_traits.csv

source("R/07_traits.R")

# 11. This script attaches the morphological defense traits
#input: data-raw/07_traits.csv
#returns: data-intermediate/11MD_trait.csv

source("R/11_MDtrait.R")

# 12. This script attaches the production traits
#input: data-intermediate/11MD_trait.csv, data-intermediate/CPfamily-updated.csv, data-intermediate/CPgenus-updated.csv
#returns: data-intermediate/12CP_trait.csv

source("R/12_CPtrait.R")

# 13. This script attaches the body form traits, returns the T matrix
#input: data-intermediate/12CP_trait.csv, data-intermediate/BFfamily-updated.csv, data-intermediate/BFgenus-updated.csv
#returns: data-raw/13_T_matrix.csv

source("R/13_BFtrait.R")

# 22. Summarizes detritus data
#input: data-raw/02_broms.csv
#returns: data-raw/22_detritus_summary.csv

source("R/22_detritus_summary.R")

# 23. Sets openness to different bromeliads
#input: data-raw/02_bromeliad_wide.csv
#returns: data-raw/23_open.csv

source("R/23_openness.R")

# 24. This script returns the max volume of bromeliads
#input: data-raw/02_broms.csv
#returns: data-raw/24_volume.csv

source("R/24_volumen.R")

# 29. This script returns all the new bromeliad characteristis including
#openness, detritus, water volume and elevation
#input: data-raw/02_broms.csv, data-raw/22_detritus_summary.csv
#data-raw/24_volume.csv, data-raw/23_open.csv
#output:data-raw/29_full_bromeliad_wide.csv

source("R/29_bromeliad_wide.R")

# 41. Joins detritus and volume to create E matrix
#input: data-raw/24_volume.csv, data-raw/22_detritus_summary.csv, data-raw/23_open.csv
#returns: data-raw/41_E_matrix.csv

source("R/41_E_matrix.R")

# 51. Summarizes the abundance table to create the W matrix
#input: data-raw/01_abundance.csv
#returns: data-raw/51_W_matrix.csv

source("R/51_abundance_W_matrix.R")

# 81. Filtering
# This script edits out missing values.
#input: data-raw/51_W_matrix.csv, data-raw/13_T_matrix.csv,
#data-raw/41_E_matrix.csv, R/output_cleaning_functions.R
#returns:data-raw/81_WBE.RDS

source("R/81_filtering.R")


# 98. ? This scripts merges the final data and prepares a new release of the dataset
#input: data-raw/01_datasets.csv, read_csv("data-raw/01_visits.csv, read_csv("data-raw/01_traits.csv
#read_csv("data-raw/02_broms.csv, read_csv("data-raw/02_abundance.csv

source("R/98_merging.R")

## preparing a release

# 99. Pushes a release
#input: "releases/all_data.rds"

#source("R/99_RELEASE.R")


