remake::dump_environment()

# we do not really have information on the ~~species richness~~ of bromeliads --
# how many morphospecies do we even have? To know this we would have to match
# taxonomic information (species, Hermetia, those hydrophilids) with site
# information (ie. higher taxonomic groups are treated separately if they are in
# separate sites)

# Another idea would be to list off the species names which are known to be synonymous:


# to website --------------------------------------------------------------

# Take a look and see -- what species are duplicated?
traits %>%
  select(-matches("[A-Z]{2}[0-9]")) %>%
  semi_join(synonymous_names) %>%
  arrange(taxon_name) %>%
  select(-(domain:species), -(functional_group:micro_macro), -barcode) %>% View

all_unique_names <- summed_abundance_lasgamas_dyst_correct %>%
  filter(abd > 0) %>%
  left_join(traits %>% select(species_id, bwg_name, taxon_name), by = c("species_id", "bwg_name")) %>%
  left_join(synonymous_names) %>%
  # create a "same name species" column
  mutate(can_merge = isTRUE(can_merge),
         same_name_spp = if_else(can_merge, taxon_name, false = paste0(taxon_name, "_", bwg_name)))

all_unique_names$same_name_spp %>% unique

 all_unique_names %>%
   select(dataset_id, same_name_spp, bromeliad_id, abd) %>%
   group_by(dataset_id, same_name_spp) %>%
   summarize(abd = median(abd)) %>%
   mutate(rnk = dense_rank(desc(abd))) %>%
   ggplot(aes(x = rnk, y = abd)) + geom_point() + facet_wrap(~dataset_id)

