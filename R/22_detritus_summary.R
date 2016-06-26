### summarizing detritus data.
### Andrew MacDonald June 2016


# pakages -----------------------------------------------------------------

require(dplyr)
require(readr)
require(tidyr)
library(stringr)
library(assertr)
library(purrr)

# read in data ------------------------------------------------------------

broms_detritus <- read_csv("data-raw/03_broms.csv",
                           col_types = cols(dead_leaves = "n"))

glimpse(broms_detritus)


# data confirmation -------------------------------------------------------


broms_detritus <- broms_detritus %>%
  ## there should be no duplicate bromelaid names.
  verify(length(bromeliad_id) == length(unique(bromeliad_id)))



# combining detritus categories -------------------------------------------

## Within visit, find all the detritus categories which exist
detritus_list <- broms_detritus %>%
  split(., .$visit_id) %>%
  map(~ .[ ,colSums(!is.na(.x)) > 0])

detritus_list %>%
  map(~names(.x)[grepl("detritus", names(.x))])
