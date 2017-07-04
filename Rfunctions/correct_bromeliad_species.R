# this script divides the bromeliad species name ("species") into genus and
# species. It corrects a few really egregious mispellings, in preparation for

extract_bromeliad_species_names <- function(.bromeliad_elevation){

  # browser()

  genus_spp_broms <- .bromeliad_elevation %>%
    select(species) %>%
    distinct %>%
    ## standardize the name of an unknown Vrisea/Guzmania combo
    mutate(species = str_replace(species, "Guzmania/Vriesea sp.|Vriesea_or_Guzmania", "VrieseaGuzmania_sp")) %>%
    separate(species, c("bromeliad_genus", "bromeliad_trivial"), extra = "merge", remove = FALSE) %>%
    arrange(bromeliad_genus) %>%
    mutate(bromeliad_genus_correct = case_when(bromeliad_genus == "G"                 ~ "Guzmania",
                                               str_detect(species, "/|_or_")          ~ "Unknown",
                                               is.character(bromeliad_genus)          ~ bromeliad_genus),
           ## Correct the trivial part of the species name.
           bromeliad_trivial_correct = case_when(bromeliad_trivial == "gladifolia"    ~ "gladioliflora",
                                                 bromeliad_trivial == "gl"            ~ "gladioliflora",
                                                 bromeliad_trivial == "desantsii"     ~ "desautelsii",
                                                 is.na(bromeliad_trivial)             ~ "sp",
                                                 str_detect(bromeliad_trivial, "sp")  ~ "sp",
                                                 is.character(bromeliad_trivial)      ~ bromeliad_trivial,
                                                 bromeliad_genus_correct == "Unknown" ~ "unknown"))


  genus_spp_corrected <- genus_spp_broms %>%
    unite("species_correct", bromeliad_genus_correct, bromeliad_trivial_correct) %>%
    select(species, species_correct)
  return(genus_spp_corrected)
}

join_old_new_bromeliad_names <- function(.genus_spp_corrected, .bromeliad_names) {
  correct_name_pairing <- genus_spp_corrected %>%
    stringdist_left_join(bromeliad_names, by = c(species_correct = "Bromeliad_species"), max_dis = 2) %>%
    arrange(Bromeliad_species)

  return(correct_name_pairing)
}


correct_bromelaid_species_names <- function(.bromeliad_elevation, .correct_name_pairing) {
  correct_name_by_old <- .correct_name_pairing$Bromeliad_species %>%
    set_names(.correct_name_pairing$species)

  .bromeliad_elevation %>%
    mutate(species = correct_name_by_old[species])
}

