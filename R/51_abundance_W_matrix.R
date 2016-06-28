### summarizing abundances
## Andrew MacDonald


library(dplyr)
library(tidyr)
library(readr)

abunds <- read.csv("data-raw/01_abundance.csv")


# summarize  --------------------------------------------------------------
summed_abundance_spp <- abunds %>%
  group_by(dataset_id, species_id, bwg_name, brm) %>%
  summarise(abd = sum(abd, na.rm = TRUE)) %>%
  ungroup

write_csv(summed_abundance_spp, "data-raw/51_abundance.csv")

wide_spp <- summed_abundance_spp %>%
  filter(abd != 0, !is.na(abd)) %>%
  select(-bwg_name) %>%
  unite("dataset_species", dataset_id, species_id) %>%
  spread(dataset_species, abd, fill = 0)

dim(wide_spp)

write_csv(wide_spp, "data-raw/51_W_matrix.csv")
