# accessing -----------------------------------------------------

## Getting the abundance matrix for each site requires querying its precise dataset ID
## this convenience function creates a named vector of all the datasets and downloads them all
## can be slow

# get_all_abundances <- function(.dats){
#
#   ## might need to replace this with a suitable structure
#   bwg_get_safe <- possibly(bwg_get, NA)
#
#   abds_ <- .dats %>%
#     .[["dataset_id"]] %>%
#     as.numeric %>%
#     set_names %>%
#     map(~ list(dataset_id = .x)) %>%
#     map(~ bwg_get_safe("matrix", .))
#
#   return(abds_)
# }



# Filtering ---------------------------------------------------------------

## NOTE eventualy these will be deleted and then this test will be deleted
## because it will be useless
filter_test <- function(x){
  x %>%
    filter(!grepl("^Test", name))
}


## pesky test datasets, and other garbage potentially, goes on a blacklist.

make_blacklist <- function(.visits, .dats){
  criteria <- .visits %>%
    left_join(.dats, by = "dataset_id") %>%
    ## count the sins here
    filter(str_detect(name, "Test"))

  criteria %>%
    select(visit_id, dataset_id)
}



# Restructuring -------------------------------------------------------------


no_attrib_unnest_det <- function(.broms){

  ## Drop the attributes part of the variable names
  names(.broms) <- str_replace(names(.broms), "attributes\\.", "")
  ## yes this makes the function a bit not modular. If you're reading this you
  ## can feel quite free to judge :)

  ### the detritus should be unnested (for raw. summarize later, across columns)
  .broms %>%
    ## supply a default data.frame, or otherwise unnesting chokes
    mutate(detritus = map_if(detritus,
                             is_null,
                             ~ tibble(min = NA,
                                          max = NA,
                                          mass = NA))) %>%
    unnest(detritus)
}

combine_multi_names <- function(.trts_all){
  ### names are a nested list. Convert them to a vector of length one.
  ## bespoke predicate function to detect multiple names
  is_long <- function(x) length(x) > 1

  .trts_all %>%
    mutate(names = names %>%
             ## again, supply a sensible default (an empty word, not a NULL)
             map_if(is_null, ~"") %>%
             map_if(is_long, paste, collapse = ";")) %>%
    ## unnesst safely, since now it is a wholesome list of length 1 dataframes
    unnest(names)

}



# import corrected names --------------------------------------------------

import_BromeliadSpecies <- function(path){
  read_delim(path,
             ";", escape_double = FALSE, trim_ws = TRUE)
}

