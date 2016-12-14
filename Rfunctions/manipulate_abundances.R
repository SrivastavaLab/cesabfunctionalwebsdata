
sum_species_abundances <- function(.abundance_filtered) {
  .abundance_filtered %>%
    rename(bromeliad_id = brm) %>%
    group_by(dataset_id, species_id, bwg_name, bromeliad_id) %>%
    summarise(abd = sum(abd, na.rm = TRUE)) %>%
    ungroup
}

# correct LasGamas dytiscid
correct_lasgamas_dytiscid <- function(.summed_abundance_spp){
  .summed_abundance_spp %>%
    mutate(species_id = if_else(dataset_id %in% c(166, 171, 181) & species_id == "4516",
                                true = "5496",
                                false = species_id),
           bwg_name =   if_else(dataset_id %in% c(166, 171, 181) & bwg_name == "Coleoptera.52",
                                true = "Coleoptera.64",
                                false = bwg_name)
    )
}


spread_present_species <- function(.summed_abundance_spp){
  .summed_abundance_spp %>%
    filter(abd != 0, !is.na(abd)) %>%
    select(-bwg_name) %>%
    unite("dataset_species", dataset_id, species_id) %>%
    spread(dataset_species, abd, fill = 0)
}
