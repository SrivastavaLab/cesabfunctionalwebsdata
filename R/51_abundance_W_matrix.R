### summarizing abundances
## Andrew MacDonald


library(dplyr)
library(tidyr)
library(readr)

abunds <- read.csv("data-raw/01_abundance.csv")


# summarize  --------------------------------------------------------------
### nope not yet!!
wide_spp <- abunds %>%
  group_by(dataset_id, species_id, bwg_name, brm) %>%
  summarise(abd = sum(abd, na.rm = TRUE)) %>%
  select(-bwg_name) %>%
  distinct %>%
  spread(species_id, abd)

write_csv(wide_spp, "data-raw/51_W_matrix.csv")
