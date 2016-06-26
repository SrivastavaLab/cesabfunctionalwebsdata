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



## If a column is fpom or cpom, this means that these attributes were added as
## "extra" attritbures to the dataset -- not, as should have been done, directly
## as a specific range of detritus. They can be ignored.

## The result of the previous script should be (check this somehow??!) to place
## all useful information into a column that starts with "detritus". Keep only
## this, and the bromeliad_id

detritus_only <- detritus_list %>%
  map(~ .x[grepl("bromeliad_id|detritus", names(.x))])


### ALSO, if there is a vlaue in detritus0_NA, this is the complete value
### already. IN some cases this will be estimated directly from the diameter, in
### other cases added from observed data. If it is present, our task is done.

has_0_NA <- detritus_only %>%
  map_lgl(~ any(grepl("detritus0_NA", names(.x))))

## such datasets need only their final column
detritus_complete <- detritus_only %>%
  .[has_0_NA] %>%
  map(~ select(.x, bromeliad_id, detritus0_NA))

## other datasets ---------------

### Some datasets do not have *any* detritus yet. Let us find these

is_missing <- detritus_only %>%
  map_lgl(~ ncol(.x) == 1)

assertthat::assert_that(length(is_missing) == length(has_0_NA))

detritus_missing <- detritus_only %>%
  .[is_missing]

## other datasets will require combination. The first step is to identify their
## columns, and confirm that they are in sequential ranges

## the first step is to get the names in question
category_names <- detritus_only %>%
  .[!has_0_NA & !is_missing] %>%
  map(names) %>%
  map(~ .x[!grepl("bromeliad_id", .x)]) %>%
  map(~ gsub("detritus", "", .x))

## remove na from min because NA is maximum by definition

## this function will print "TRUE" if the categories defined by the vector are
## continuous, e.g. c("1500_20000","20000_NA","0_1500" ) `
is_continuous_categories <- function(cat_vector){
  cat_range <- cat_vector %>%
    str_split("_") %>%
    transpose %>%
    map(unlist) %>%
    {c(invoke(setdiff, .), invoke(setdiff, rev(.)))}

  identical(cat_range, c("0", "NA"))
}

c("1500_20000","20000_NA","0_1500" ) %>% str_split("_") %>% map(as.numeric) %>% map(min, na.rm = TRUE)

c("1500_20000","20000_NA","0_1500" ) %>% str_split("_") %>% map(as.numeric) %>% map(max)

