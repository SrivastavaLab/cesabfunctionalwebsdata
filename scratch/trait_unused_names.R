

# identify taxa which did not match ---------------------------------------


missers <- find_taxo_missing(trait_spreadsheet, lowest_taxonomic)
missers

# checking names for correspondance with taxize ---------------------------

## name check those traits

the_animal_names <- trait_spreadsheet$taxon_name %>% unique

library(taxize)
## try gnr_resolve for all our animals

all_animal_names_resolved <- map(the_animal_names, gnr_resolve)
all_matched_names <- all_animal_names_resolved %>%
  map("matched_name") %>%
  map(unique)

unfound_names <- all_matched_names %>% set_names(the_animal_names) %>% keep(is.null)

found_names <- all_matched_names %>% set_names(the_animal_names) %>% discard(is.null)

found_names_df <- map_df(found_names, ~ .x %>% set_names %>% map_dbl(nchar) %>% tibble::enframe(.), .id = "spp")

mismatched_names <- found_names_df %>%
  group_by(spp) %>%
  filter(value == min(value)) %>%
  ungroup %>%
  mutate(spp = str_replace_all(spp, "_", " ")) %>%
  filter(spp != name)


# visulaize the missing data ----------------------------------------------

library(naniar)

gg_missing_var(traits)

visdat::vis_miss(traits)

traits %>%
  left_join(canonical, by = "species_id") %>%
  filter(., !complete.cases(.)) %>%
  visdat::vis_miss_ly(.)

traits %>%
  left_join(canonical, by = "species_id") %>%
  visdat::vis_miss(.)

traits %>%
  filter(., !complete.cases(.)) %>%
  group_by(taxon_level, taxon_name, num) %>%
  nest %>%
  mutate(nmiss = map_dbl(data, ~ sum(is.na(.x)))) %>%
  arrange(desc(nmiss)) %>% View
