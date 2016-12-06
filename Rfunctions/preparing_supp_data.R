# reading in the data and letting it have correct names
library(dplyr)
library(readr)
library(purrr)

size_data <- dir("data-intermediate", pattern = "size_.*csv", full.names = TRUE) %>%
  set_names(basename(.)) %>%
  map(read_csv2)

all_size_data <- size_data %>%
  # the first column is called many things
  map(~ set_names(.x, c("plant_id", names(.x)[-1]))) %>%
  map(~ mutate(.x, plant_id = as.character(plant_id))) %>%
  bind_rows(.id = "filename") %>%
  mutate(filename = filename %>% str_replace("size_", "") %>% str_replace("\\.csv", ""))

all_size_data$filename %>% unique

# this could add more info at the level of the file: who collected it, etc.
more_information <- frame_data(
  ~ filename,       ~species,
  "aquilega_Biog",  "Aechmaea_aquilega",
  "aquilegaKT",     "Aechmaea_aquilega",
  "Guzmania_PR",    "Guzmania_sp",
  "mertensii",      "Aechmaea_mertensii",
  "vriesea_prod",   "Vrisea_splendens",
  "vriesea",        "Vrisea_speledens"
)

# write data out
all_size_data %>%
  left_join(more_information) %>%
  write_csv("data-intermediate/size_all_data.csv")
