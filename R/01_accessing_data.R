## accessing data from the bwg database

require("bwgdata")
require(readr)
require(stringr)
require(tidyr)
require(purrr)
require(dplyr)

dats <- bwg_get("datasets")
visits <- bwg_get("visits")
broms <- bwg_get("bromeliads")
spp <- bwg_get("species")
trts <- bwg_get("species", list(traits = "true"))
trts_tach <- bwg_get("species", list(tachet = "true"))
## ABUNDANCE downloaded in abundance section below

## the goal now is to simply write these as proper CSVs

# Datasets ----------------------------------------------------------------

## we need to filter out the test datasets
dats <- dats %>%
  filter(!grepl("^Test", name))

dats %>%
  ## write out datasets
  write_csv("data-raw/01_datasets.csv")

# Visits ------------------------------------------------------------------

## write visits
write_csv(visits, "data-raw/01_visits.csv")

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
  write_csv("data-raw/01_broms.csv")


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
  write_csv("data-raw/01_traits.csv")


# Abundances (matrix) -----------------------------------------------------
abds <- dats %>%
  .[["dataset_id"]] %>%
  as.numeric %>%
  set_names %>%
  map(~ list(dataset_id = .x)) %>%
  map(~ bwg_get("matrix", .))

abds %>%
  bwgdata::tidy_dataset_list() %>%
  write_csv("data-raw/01_abundance.csv")
