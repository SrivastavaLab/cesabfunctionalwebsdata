# Loading the data

CPtrait <- read.csv("data-raw/05_CPtrait.csv", sep =';') %>%
  select(-X.)

Alltraits <- read.csv("data-raw/01_traits.csv")

library(dplyr)

try_join <- left_join(Alltraits, CPtrait) %>%
  filter(!is.na(CPI1)) %>%
  select(-species_id, -bwg_name, -starts_with('t'), -domain, -kingdom, -phylum, -subphylum,
         -key, -functional_group, -predation, -realm, -micro_macro,
         -barcode, -names) %>%
  distinct()

glimpse(try_join)

setdiff(names(Alltraits), )
setdiff(names(try_join), names(Alltraits))

alltry_join <- left_join(Alltraits, try_join) %>%
  filter(!is.na(CPI1)) %>%
  glimpse()

write.csv(alltry_join, "data-produced/alltry_join.csv")


CP_join_bwgnames <- left_join(Alltraits, CPtrait, by = "bwg_name")


# Merge by bwgnames

bwg_trait <- CPtrait %>%
  select(bwg_name, starts_with("CP"))

bwg_join <- left_join(Alltraits, bwg_trait, by = "bwg_name")

# Merge by genus

CP_genus <- CPtrait %>%
  select(genus, starts_with("CP")) %>%
  distinct()

genus_join <- left_join(bwg_join, CP_genus, by = "genus")

genus_merged <- genus_join %>%
  mutate(CPG1 = ifelse(!is.na(CPI1.y), CPI1.y, CPI1.x)) %>%
  mutate(CPG2 = ifelse(!is.na(CPI2.y), CPI2.y, CPI2.x)) %>%
  mutate(CPG3 = ifelse(!is.na(CPI3.y), CPI3.y, CPI3.x)) %>%
  select(-CPI1.x, -CPI2.x, -CPI3.x, -CPI1.y, -CPI2.y, -CPI3.y)

# Merge by Family

CP_family <- CPtrait %>%
  select(family, genus, starts_with("CP")) %>%
  distinct()

family_join <- left_join(genus_merged, CP_family, by = c("family"))

family_merged <- family_join %>%
  mutate(CPF1 = ifelse(!is.na(CPG1), CPG1, CPI1)) %>%
  mutate(CPF2 = ifelse(!is.na(CPG2), CPG2, CPI2)) %>%
  mutate(CPF3 = ifelse(!is.na(CPG3), CPG3, CPI3)) %>%
  select(-CPI1, -CPI2, -CPI3, -CPG1, -CPG2, -CPG3)

# Merge by subrder

CP_suborder <- CPtrait %>%
  select(subord, family, subfamily, genus, starts_with("CP")) %>%
  distinct()

order_join <- left_join(family_merged, CP_suborder, by = c("subord"))

head(order_join)
order_join %>%
  mutate(eq = CPF1 == CPI1)
  select(eq) %>% sum()

sum(order_join$CPF1 == order_join$CPI1, na.rm = TRUE)

order_merged <- order_join %>%
  mutate(CPO1 = ifelse(!is.na(CPF1), CPF1, CPI1)) %>%
  mutate(CPO2 = ifelse(!is.na(CPF2), CPF2, CPI2)) %>%
  mutate(CPO3 = ifelse(!is.na(CPF3), CPF3, CPI3)) %>%
  select(-CPI1, -CPI2, -CPI3, -CPF1, -CPF2, -CPF3)


bwg_join %>%
  select(CPI1) %>%
  is.na() %>% sum()

genus_merged %>%
  select(CPG1) %>%
  is.na() %>% sum()

family_merged %>%
  select(CPF1) %>%
  is.na() %>% sum()

order_merged %>%
  select(CPO1) %>%
  is.na() %>% sum()



## checks


family_merged %>%
  select(bwg_name, subclass, ord, subord, family, subfamily, CPF1) %>%
  filter(is.na(CPF1), subclass == "Oligochaeta")

CP_join_bwgnames %>%
  filter(subclass == "Oligochaeta") %>%
  select(bwg_name, subclass, ord.x, subord, family.x, subfamily, CPI1)

CPtrait %>% select(bwg_name, CPI1) %>%
  filter(is.na(CPI1)) %>% glimpse()

glimpse(family_merged)
