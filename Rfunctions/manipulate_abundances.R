
sum_species_abundances <- function(.abundance_filtered) {
  .abundance_filtered %>%
    group_by(dataset_id, species_id, bwg_name, brm) %>%
    summarise(abd = sum(abd, na.rm = TRUE)) %>%
    ungroup
}


spread_present_species <- function(.summed_abundance_spp){
  .summed_abundance_spp %>%
    filter(abd != 0, !is.na(abd)) %>%
    select(-bwg_name) %>%
    unite("dataset_species", dataset_id, species_id) %>%
    spread(dataset_species, abd, fill = 0)
}
