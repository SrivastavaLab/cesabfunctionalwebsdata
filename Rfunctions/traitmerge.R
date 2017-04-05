#' andrew macdonald
#' notes on megin traits in to the dat

make_taxonomy_cols <- function(.trts_all_filtered) {
  trts_taxonomy_cols <- trts_parsed_cols %>%
    select_("species_id", "domain", "kingdom", "phylum", "subphylum",
            "class", "subclass", "ord", "subord", "family", "subfamily",
            "tribe", "genus", "species", "subspecies")

  #no starting NA
  no_start_NA <- function(x) !grepl("NA[A-Za-z]{3,}", x)

  taxo_cols_with_spp <- trts_taxonomy_cols %>%
    mutate(species = stringr::str_replace(species, "\\(.*\\)\\s", "")) %>%
    # if species exists, fuse with genus -- this assumes that species is only present when genus is also present
    assert_rows(num_row_NAs, within_bounds(0, 2), genus, species) %>%
    assert_rows(col_concat, no_start_NA, genus, species) %>%
    mutate(species_name = if_else(!is.na(species),
                                  paste0(genus, "_", species),
                                  NA_character_)) %>%
    select(-species)

  return(taxo_cols_with_spp)
}


get_lowest_taxonomic <- function(.taxonomy_cols) {
  # for each morphospecies, which is the lowest level to which it has been identified?
  taxa_long <- .taxonomy_cols %>%
    gather(taxon_level, taxon_name, -species_id) %>%
    filter(!is.na(taxon_name))

  taxon_numbers <- frame_data(
    ~taxon_number,     ~taxon_level,
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
    12,        "genus",
    13,        "species_name"
  )

  taxonomic_levels <- taxa_long %>%
    left_join(taxon_numbers, by = "taxon_level")


  maximum_num <- taxonomic_levels %>%
    group_by(species_id) %>%
    filter(taxon_number == max(taxon_number, na.rm = TRUE)) %>%
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

  in_spreadsheet_not_bwgdb <- dplyr::setdiff(spreadsheet_names, lowtaxonomy_names)

  in_bwgdb_not_spreadsheet <-  dplyr::setdiff(lowtaxonomy_names, spreadsheet_names)

  list(in_spreadsheet_not_bwgdb = in_spreadsheet_not_bwgdb,
       in_bwgdb_not_spreadsheet = in_bwgdb_not_spreadsheet)
}


merge_trait_by_taxonomy <- function(.trait_spreadsheet, .lowest_taxonomic){
  ## prepare spreadsheet traits for merging
  taxonomic_traits <- .trait_spreadsheet %>%
    rename(taxon_number = tax_num)

  # merge traits by taxonomy
  new_trait_table <- .lowest_taxonomic %>%
    left_join(taxonomic_traits, by = c("taxon_name", "taxon_level", "taxon_number"))

  return(new_trait_table)
}

get_canonical_traits <- function(.trts_all_filtered) {
  .trts_all_filtered %>%
    select(species_id, functional_group, predation, realm, micro_macro)
}


