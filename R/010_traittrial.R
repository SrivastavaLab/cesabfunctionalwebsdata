# Loading the data

library(dplyr)
library(assertr)
library(readr)
library(tidyr)
library(daff)

## read_csv2 uses ; as a field separator
CPtrait <- read_csv2("data-raw/05_CPtrait.csv") %>%
  select(-`#`)

Alltraits <- read_csv("data-raw/07_traits.csv")


# Create genera table

genus <- CPtrait %>%
  select(genus, CPI1, CPI2, CPI3) %>%
  filter(!is.na(genus)) %>%
  distinct() %>%
  arrange(genus) %>%  as.data.frame()

familyt <- CPtrait %>%
  select(family, CPI1, CPI2, CPI3) %>%
  distinct() %>%
  arrange(family)

new_database_families <- Alltraits %>%
  select(family) %>%
  distinct() %>%
  filter(!is.na(family), family != "NA") %>%
  arrange() %>%
  dplyr::setdiff(familyt %>% select(family))


write.csv(genus, "data-produced/genus.csv")

familyt %>%
  bind_rows(new_database_families) %>%
  write_csv("data-produced/original_family.csv")
#write.csv(familyt, "data-produced/family.csv")


## read corrected

familynew <- read_csv2("data-produced/family.csv", col_types = "_cnnn")


# add traits by genus

by_genus <- left_join(Alltraits, genus) %>%
  verify(nrow(.) == nrow(Alltraits))

# count number of NAs by
by_genus %>%
  filter(is.na(CPI1)) %>% nrow()

# add trait by family
by_family <- left_join(by_genus, familynew, by = "family") %>%
  verify(nrow(.) == nrow(Alltraits))

by_family_merged <- by_family %>%
  mutate(CPF1 = ifelse(is.na(CPI1.x), CPI1.y, CPI1.x)) %>%
  mutate(CPF2 = ifelse(is.na(CPI2.x), CPI2.y, CPI2.x)) %>%
  mutate(CPF3 = ifelse(is.na(CPI3.x), CPI3.y, CPI3.x)) %>%
  select(-starts_with("CPI")) %>%
  rename(CPI1 = CPF1, CPI2 = CPF2, CPI3 = CPF3)

render_diff(diff_data(by_genus, by_family_merged))

# count number of NAs by family

by_family_merged %>%
  filter(is.na(CPI1)) %>% nrow()

# leave out terrestrials and everything that is NA up to order

by_family_merged %>%
  filter(is.na(CPI1), realm != "terrestrial", !is.na(genus), !is.na(subfamily),
         !is.na(family)) %>% View()



