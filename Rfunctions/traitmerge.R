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
    # mutate(newspecies = stringr::str_replace(species, "\\(.*\\)\\s", "")) %>%
    # mutate(species_name = case_when(
    #   !is.na(genus) & !is.na(newspecies) & !is.na(subspecies) ~ paste(genus, newspecies, subspecies, sep = "_"),
    #   !is.na(genus) & !is.na(newspecies)                      ~ paste(genus, newspecies,             sep = "_")
    # )) %>%
    mutate(species_name = paste(genus, species, subspecies, sep = "_"),
           ## remove the "_NA" bit that comes when absent names are combined
           species_name = str_replace_all(species_name, pattern = "_NA", ""),
           # parse remaining "NA" to true NA with readr
           species_name = readr::parse_character(species_name)) %>%
    select(- genus, -species, -subspecies)

  return(taxa_species_names)
}

# taxa_species_names %>% glimpse
#

get_lowest_taxonomic <- function(.taxonomy_cols) {
  # for each morphospecies, which is the lowest level to which it has been identified?
  taxa_long <- .taxonomy_cols %>%
    gather(taxon_level, taxon_name, -species_id) %>%
    filter(!is.na(taxon_name))

  taxon_numbers <- frame_data(
    ~num,     ~taxon_level,
    1,        "domain",
    2,        "kingdom",
    3,        "phylum",
    4,        "subphylum",
    5,        "class",
    6,        "subclass",
    7,        "ord",
    8,        "subord",
    9,        "family",
    10,        "subfamily",
    11,        "tribe",
    12,        "species_name"
  )

  taxonomic_levels <- taxa_long %>%
    left_join(taxon_numbers, by = "taxon_level")


  maximum_num <- taxonomic_levels %>%
    group_by(species_id) %>%
    filter(num == max(num)) %>%
    ungroup

  return(maximum_num)

}

# must get the taxonomic traits -------------------------------------------

get_trait_spreadsheet <- function() {
  post_editing <- gs_title("traits_for_editing_20-01-2017") %>%
    gs_read_csv()

  post_edit_no_dup <- post_editing %>%
    select(-starts_with("reference"))

  ## check that no rows are duplicate
  if( nrow(post_edit_no_dup %>% filter(duplicated(.))) > 0) stop("duplicates present")

  return(post_edit_no_dup)
}

find_taxo_missing <- function(.trait_spreadsheet, .lowest_taxonomic) {

  spreadsheet_names <- .trait_spreadsheet %>% select(taxon_name) %>% distinct

  lowtaxonomy_names <- .lowest_taxonomic %>% select(taxon_name) %>% distinct

  dplyr::setdiff(spreadsheet_names, lowtaxonomy_names)

}


merge_trait_by_taxonomy <- function(.trait_spreadsheet, .lowest_taxonomic){
  ## prepare spreadsheet traits for merging
  taxonomic_traits <- .trait_spreadsheet %>%
    select(-tax_num, -taxon_level)

  # merge traits by taxonomy
  new_trait_table <- .lowest_taxonomic %>%
    left_join(taxonomic_traits, by = "taxon_name")
}

