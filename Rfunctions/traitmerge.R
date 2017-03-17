#' andrew macdonald
#' notes on megin traits in to the dat

make_taxonomy_cols <- function(.trts_all_filtered) {
  trts_just_taxonomy <- .trts_all_filtered %>%
    select_("species_id","names", "bwg_name", "domain", "kingdom", "phylum", "subphylum",
            "class", "subclass", "ord", "subord", "family", "subfamily",
            "tribe", "genus", "species", "subspecies", "functional_group",
            "predation", "realm", "micro_macro", "barcode")

  trts_parsed_cols <- parse_column_types_reader(trts_just_taxonomy)

  # just the taxonomy
  trts_taxonomy_cols <- trts_parsed_cols %>%
    select_("species_id", "domain", "kingdom", "phylum", "subphylum",
            "class", "subclass", "ord", "subord", "family", "subfamily",
            "tribe", "genus", "species", "subspecies")

  # could create something where i get the columsn which are ignored here, and preserve them.

  taxa_species_names <- trts_taxonomy_cols %>%
    ## delete "extra genus" parts of species names
    mutate(newspecies = stringr::str_replace(species, "\\(.*\\)\\s", "")) %>%
    # mutate(species_name = case_when(
    #   !is.na(genus) & !is.na(newspecies) & !is.na(subspecies) ~ paste(genus, newspecies, subspecies, sep = "_"),
    #   !is.na(genus) & !is.na(newspecies)                      ~ paste(genus, newspecies,             sep = "_")
    # )) %>%
    mutate(newspecies = if_else(!is.na(newspecies) & !is.na(subspecies),
                                true = paste(newspecies, subspecies, sep = "_"),
                                # false = if_else(
                                #   !is.na(genus) & !is.na(newspecies),
                                #   true  = paste(genus, newspecies, sep = "_"),
                                #   false = NA_character_)
                                false = newspecies)
    ) %>%
    select(-species, -subspecies) %>%
    mutate(species_name = if_else(!is.na(genus) & !is.na(newspecies),
                                  true = paste(genus, newspecies, sep = "_"),
                                  false = NA_character_)) %>%
    select(-genus, -newspecies)

  return(taxa_species_names)
}

# taxa_species_names %>% glimpse
#

get_lowest_taxonomic <- function(.taxonomy_cols) {
  # for each morphospecies, which is the lowest level to which it has been identified?
  taxa_lowest <- .taxonomy_cols %>%
    gather(taxon_level, taxon_name, -species_id) %>%
    group_by(species_id) %>%
    # assuming that gather keeps column headers in the same sequence! this could be hard coded.
    mutate(tax_num = seq_along(taxon_level)) %>%
    filter(!is.na(taxon_name)) %>%
    filter(tax_num == max(tax_num)) %>%
    ungroup %>%
    mutate(species_id = as.character(species_id))
  #
}

## prepare lowest-taxon table for merging
taxa_low_for_merge <- taxa_lowest %>%
#
# # must get the taxonomic traits -------------------------------------------
#
# sheettitle <- "traits_for_editing_20-01-2017"
#
# library(googlesheets)
# post_editing <- gs_title(sheettitle) %>%
#   gs_read_csv()
#
#
# post_edit_no_dup <- post_editing %>%
#   select(-starts_with("reference"))
#
# ## check that no rows are duplicate
# if( nrow(post_edit_no_dup %>% filter(duplicated(.))) > 0) stop("duplicates present")
#
#
# ## prepare post_editing for merging
# taxonomic_traits <- post_edit_no_dup %>%
#   select(-tax_num, -taxon_level)
#
#
# new_trait_table <- taxa_low_for_merge %>%
#   left_join(taxonomic_traits, by = "taxon_name")
