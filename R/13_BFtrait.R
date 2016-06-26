
library(dplyr)
library(assertr)
library(readr)
library(tidyr)
library(daff)

Alltraits <- read_csv("data-intermediate/12CP_trait.csv")

## read data

genus_new <- read_csv2("data-intermediate/BFgenus_updated.csv", col_type = "_ciiii")

family_new <- read_csv2("data-intermediate/BFfamily_updated.csv", col_type = "_ciiii")

# add traits by genus

by_genus <- left_join(Alltraits, genus_new) %>%
  verify(nrow(.) == nrow(Alltraits))

# count number of NAs by

by_genus$BF1 %>%
  is.na() %>% sum()

# add trait by family
by_family <- left_join(by_genus, family_new, by = "family") %>%
  verify(nrow(.) == nrow(Alltraits))


by_family_merged <- by_family %>%
  mutate(BF1 = ifelse(is.na(BF1.x), BF1.y, BF1.x)) %>%
  mutate(BF2 = ifelse(is.na(BF2.x), BF2.y, BF2.x)) %>%
  mutate(BF3 = ifelse(is.na(BF3.x), BF3.y, BF3.x)) %>%
  mutate(BF4 = ifelse(is.na(BF4.x), BF4.y, BF4.x)) %>%
  select(-ends_with(".x"), -ends_with(".y"))


# count number of NAs by family

by_family_merged$BF1 %>%
  is.na() %>% sum()

stopifnot(nrow(by_family_merged) == nrow(Alltraits))

write.csv(by_family_merged, "data-raw/13_final_trait.csv", row.names = FALSE)
