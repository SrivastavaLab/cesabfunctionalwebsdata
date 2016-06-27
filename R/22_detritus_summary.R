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

broms_detritus <- read.csv("data-raw/02_broms.csv",stringsAsFactors = FALSE)

glimpse(broms_detritus)

## does it have columns called "detritus"

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

## logical vectors are based on the full dataset always, and should always be the same
assertthat::assert_that(length(is_missing) == length(has_0_NA))

detritus_missing <- detritus_only %>%
  .[is_missing]

## other datasets will require combination. The first step is to identify their
## columns, and confirm that they are in sequential ranges

## the first step is to get the names in question

## i want a logical vector that identifies where the detritus amounts require
## combination. These are defined as bromeliads which do NOT have a single
## column AND ALSO those which do not have 0_NA already

is_possible_to_combine <- !has_0_NA & !is_missing

## there are two kinds of datasets in this category: the first has continuous
## categories, the second does not. let us find them each in turn.

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


get_range <- function(cat_vector) {
  cat_range <- cat_vector %>%
    str_split("_") %>%
    transpose %>%
    map(unlist) %>%
    {c(invoke(setdiff, .), invoke(setdiff, rev(.)))}
  return(cat_range)
}

detect_continuous <- function(dfnames){

  if (length(dfnames) > 0){
    is_continuous_categories(dfnames)

  } else {

      FALSE
  }
}

detect_discontinuous <- function(dfnames){

  if (length(dfnames) > 0){
    !is_continuous_categories(dfnames)

  } else {

    FALSE
  }
}



is_discont <- detritus_only %>%
  map(names) %>%
  map(~ .x[!grepl("bromeliad_id", .x)]) %>%
  map(~ gsub("detritus", "", .x)) %>%
  map_lgl(detect_discontinuous)


is_contin <- detritus_only %>%
  map(names) %>%
  map(~ .x[!grepl("bromeliad_id", .x)]) %>%
  map(~ gsub("detritus", "", .x)) %>%
  map_lgl(detect_continuous)


cbind(is_missing, is_discont, is_contin) %>% as.data.frame() %>%
  colSums() %>% sum()
  ## should be equal to the lenght of what went in

is_discont_ids <- is_discont %>% which(TRUE) %>% names(.)

endpoints <- detritus_only %>%
  map_if(!is_missing, names) %>%
  map_if(!is_missing, ~ .x[!grepl("bromeliad_id", .x)]) %>%
  map_if(!is_missing, ~ gsub("detritus", "", .x)) %>%
  map_if(!is_missing, get_range) %>%
  map_if(is_missing, ~ c("NA", "NA")) %>%
  map(paste, collapse = "_")

final_detritus <- detritus_only %>%
  map(~ data.frame(row_sum = rowSums(dplyr::select(.x, -bromeliad_id)),
                   bromeliad_id = dplyr::select(.x, bromeliad_id))) %>%
  map2(endpoints, cbind) %>%
  bind_rows(.id = 'visit_id')



# remove negative values --------------------------------------------------

final_detritus$row_sum[final_detritus$row_sum < 0] <- NA

final_detritus %>%
  select(visit_id,
         bromeliad_id,
         detritus_sum = row_sum,
         detritus_range = `.y[[i]]`) %>%
  write_csv("data-raw/22_detritus_summary.csv")




