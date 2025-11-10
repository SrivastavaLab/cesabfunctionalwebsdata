
output_spp_dictionary <- function(.abundance_filtered, .broms_date, .visits_date, .trts_all_values){

  spp_visits <- .abundance_filtered %>%
    select(bromeliad_id, species_id) %>%
    distinct() %>%
    left_join(.broms_date %>% select(bromeliad_id, visit_id) %>% distinct) %>%
    select(species_id, visit_id) %>%
    distinct %>%
    left_join(.visits_date %>% select(visit_id, date, habitat, dataset_name))

  spp_names <- .trts_all_values %>%
    select(bwg_name, species_id, Order=ord, Family=family,
           Genus = genus, species) %>%
    replace_na(list(Order = "", Family = "", Genus = "", species = "")) %>%
    # unite(Species, Genus, species, sep = " " ) %>%
    distinct

  spp_dict <- spp_visits %>% left_join(spp_names) %>%
    select(bwg_name, Genus, species, Family, Order, habitat, date, dataset_name) %>%
    distinct

  return(spp_dict)

}


