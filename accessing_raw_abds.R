dats <- bwg_get("datasets")
abds <- get_all_abundances(dats)

library(tidyverse)

dats %>%
  filter(location == "Cardoso")

# we want dataset_id 6

cardoso_raw <- bwg_get("matrix", opts = list(dataset_id = 6))

str(cardoso_raw, max.level = 2)

map_df(cardoso_raw[[1]], 1, .id = "species_id")

cardoso_raw[[1]][[1]]$measurements

# Diptera.190

cardoso_meas <- cardoso_raw[[1]] %>%
  enframe %>%
  mutate(bwgname = map_chr(value, 1),
         measure = map(value,2))

cardoso_meas %>%
  filter(bwgname == "Diptera.190") %>% .[["measure"]] %>% .[[1]] %>% enframe %>%
  mutate(cat = map_chr(value, "category_range"),
         mes = map(value, "measurement"))

cardoso_almost_flat <- cardoso_meas %>%
  filter(bwgname == "Diptera.193") %>%
  .[["measure"]] %>% .[[1]] %>% enframe %>%
  mutate(cat = map_chr(value, "category_range"),
         mes = map_chr(value, c("measurement", "value")),
         abd = map(value, 3))
#
# cardoso_almost_flat$abd[[1]] %>%
#   enframe(name = "bromeliad_id", value = "abd") %>%
#   unnest(cols = c(abd))

make_brom_df <- . %>%
  enframe(name = "bromeliad_id", value = "abd") %>%
  unnest(cols = c(abd))

cardoso_almost_flat %>%
  mutate(abd_brom = map(abd, make_brom_df)) %>%
  select(-value, -abd) %>% unnest(abd_brom)


cardoso_almost_flat <- cardoso_meas %>%
  mutate(meas_list = map(measure, 1),
         meas_df = map(meas_list, enframe, name = "listcontents")) %>%
  select(-value, -measure, -meas_list) %>%
  unnest(meas_df) %>%
  pivot_wider(names_from = "listcontents", values_from = "value")


cardoso_almost_flat %>%
  mutate(category_range = flatten_chr(category_range),
         measurement = map_chr(measurement, "value"),
         bromeliads = map(bromeliads, enframe)) %>%
  rename(species.key = name) %>%
  unnest(bromeliads) %>%
  mutate(value = flatten_chr(value))
