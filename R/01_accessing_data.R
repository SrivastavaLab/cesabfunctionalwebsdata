## accessing data from the bwg database

require("bwgdata")
require(readr)
require(stringr)
require(tidyr)
require(purrr)

dats <- bwg_get("datasets")
visits <- bwg_get("visits")
broms <- bwg_get("bromeliads")
spp <- bwg_get("species")
trts <- bwg_get("species", list(traits = "true"))
## ABUNDANCE downloaded in abundance section below

## the goal now is to simply write these as proper CSVs

# Datasets ----------------------------------------------------------------

## we need to filter out the test datasets
dats <- dats %>%
  filter(!grepl("^Test", name))

dats %>%
  ## write out datasets
  write_csv("data-raw/datasets.csv")

# Visits ------------------------------------------------------------------

## write visits
write_csv(visits, "data-raw/visits.csv")

# Bromelaids --------------------------------------------------------------

## bromeliads
### bromeliads are fine to print as-is, except for two qualities:
### the word "attributes" should be dropped from some columns



names(broms) <- str_replace(names(broms), "attributes\\.", "")

### the detritus should be unnested (for raw. summarize later)


broms %>%
  ## supply a default data.frame
  mutate(detritus = map_if(detritus,
                           is_null,
                           ~ data_frame(min = NA,
                                        max = NA,
                                        mass = NA))) %>%
  unnest(detritus) %>%
  write_csv("data-raw/broms.csv")


# Traits ------------------------------------------------------------------


### similarly, these traits need only to have their "traits" column dropped

names(trts) <- str_replace(names(trts), "traits\\.", "")

### AND of course they also need their formal names. What if these were just slapped together for our purposes here?
is_long <- function(x) length(x) > 1

trts %>%
  mutate(names = names %>%
           map_if(is_null, ~"") %>%
           map_if(is_long, paste, collapse = ";")) %>%
  unnest(names) %>%
  write_csv("data-raw/traits.csv")


# Abundances (matrix) -----------------------------------------------------

make_brom_a_df <- function(abd_data){
  # browser()
  abd_data %>%
    at_depth(2,
             ~ map_at(.x, "measurements",
                      ~ map(.x,
                            ~ map_at(.x, "bromeliads",
                                     ~ data_frame(brm = names(.x),
                                                  abd = as.numeric(.x))))))
}

### should run to see where errors are

## A function to convert the information about the abundance of a particular
## insect of particular size into one dataframe.
one_measurement <- function(measure_list){
  measure_list_flat <- map_at(measure_list, "measurement", flatten_chr)

  assertthat::assert_that(length(measure_list_flat$measurement) == 1)

  measure_list_flat %>%
    flatten %>%
    invoke(data_frame, .)
}


make_full_df <- function(abd_data_flat){
  abd_data_flat %>%
    at_depth(2,
             ~ map_at(.x, "measurements",
                      ~ map(.x, one_measurement) %>%
                        bind_rows)) %>%
    at_depth(2, flatten) %>%
    at_depth(2, ~ invoke(data_frame, .x)) %>%
    at_depth(1, bind_rows, .id = "species_id") %>%
    .[["species"]]
}

abds <- dats %>%
  .[["dataset_id"]] %>%
  as.numeric %>%
  set_names %>%
  map(~ list(dataset_id = .x)) %>%
  map(~ bwg_get("matrix", .))


all_abd <- abds %>%
  map(make_brom_a_df) %>%
  map(make_full_df)
