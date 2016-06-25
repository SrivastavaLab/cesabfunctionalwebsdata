# Loading the data

CPtrait <- read.csv("data-raw/05_CPtrait.csv")

Alltraits <- read.csv("data-raw/01_traits.csv")

library(dplyr)

# Merge by bwgnames

bwg_trait <- CPtrait %>%
  select(bwg_name, starts_with("CP"))

bwg_join <- left_join(Alltraits, bwg_trait, by = "bwg_name")

# Merge by genus

CP_genus <- CPtrait %>%
  select(genus, starts_with("CP")) %>%
  distinct()

genus_join <- left_join(bwg_join, CP_genus, by = "genus")



