library(bwgdata) # works with version 0.2.1 ONLY
library(tidyverse)

dats <- bwg_get("datasets")

dats %>%
  filter(location == "Cardoso")

# we want dataset_id 6

cardoso_raw <- bwg_get("matrix", opts = list(dataset_id = 6))

str(cardoso_raw, max.level = 2)

## need to extract first element of list, then pull out the first and second
## elements of the list associated with each species. This corresponds to the
## bwgname and the measurement of that species
cardoso_meas <- cardoso_raw[[1]] %>%
  enframe(name = "species.key") %>%
  mutate(bwgname = map_chr(value, 1),
         measure = map(value,2)) %>%
  select(-value)


cardoso_almost_flat <- cardoso_meas %>%
  # measure is a nested list; extract and convert to a dataframe
  mutate(meas_list = map(measure, 1),
         meas_df = map(meas_list, enframe, name = "listcontents")) %>%
  select(-measure, -meas_list) %>%
  # unpack then pivot the contents to columns
  unnest(meas_df) %>%
  pivot_wider(names_from = "listcontents", values_from = "value")

## flatten and unpack the columns
cardoso_almost_flat %>%
  mutate(category_range = flatten_chr(category_range),
         measurement = map_chr(measurement, "value"),
         bromeliads = map(bromeliads, enframe)) %>%
  unnest(bromeliads) %>%
  mutate(value = flatten_chr(value) %>% readr::parse_double(.)) %>%
  write_csv("cardoso_abundances_and_measurements.csv")


