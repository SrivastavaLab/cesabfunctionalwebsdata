# Loading the data

library(dplyr)
library(assertr)
library(readr)
library(tidyr)
library(daff)

## read_csv2 uses ; as a field separator
#CPtrait <- read_csv2("data-raw/05_CPtrait.csv") %>%
  #select(-`#`)

Alltraits <- read_csv("data-intermediate/11MD_trait.csv")

## read corrected

genus_updated <- read_csv2("data-intermediate/CPgenus_updated.csv", col_types = "_cnnn")

family_updated <- read_csv2("data-intermediate/CPfamily_updated.csv", col_types = "_cnnn")

# add traits by genus

by_genus <- left_join(Alltraits, genus_updated) %>%
  verify(nrow(.) == nrow(Alltraits))

# count number of NAs by

by_genus$CPI1 %>%
  is.na() %>% sum()

# add trait by family
by_family <- left_join(by_genus, family_updated, by = "family") %>%
  verify(nrow(.) == nrow(Alltraits))

by_family_merged <- by_family %>%
  mutate(CPF1 = ifelse(is.na(CPI1.x), CPI1.y, CPI1.x)) %>%
  mutate(CPF2 = ifelse(is.na(CPI2.x), CPI2.y, CPI2.x)) %>%
  mutate(CPF3 = ifelse(is.na(CPI3.x), CPI3.y, CPI3.x)) %>%
  select(-starts_with("CPI")) %>%
  rename(CP1 = CPF1, CP2 = CPF2, CP3 = CPF3)

by_family_merged$CP1 %>%
  is.na() %>% sum()

by_family_merged %>% glimpse()

stopifnot(nrow(by_family_merged) == nrow(Alltraits))

write.csv(by_family_merged, "data-intermediate/12CP_trait.csv", row.names = FALSE)

