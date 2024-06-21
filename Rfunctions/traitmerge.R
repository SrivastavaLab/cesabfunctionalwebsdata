#' andrew macdonald
#' notes on megin traits in to the dat

make_taxonomy_cols <- function(.trts_all_filtered) {
  trts_taxonomy_cols <- .trts_all_filtered %>%
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

# split off subspecies and put it back

lowest_name_and_subspecies <- function(.taxonomy_cols, .lowest_names) {

  # here is all the subspecies columns
  subspp <- .taxonomy_cols %>%
    # cut off subspecies
    select(species_id, subspecies)

  # now we define the lowest taxonomic category, without subspecies. Because
  # even something that is only identified to the level of subfamily can have a
  # subspecies!! what a time to be alive
  lowest_names <- .taxonomy_cols %>%
    select(-subspecies) %>%
    get_lowest_taxonomic

  # anybody missing?
  .taxonomy_cols %>%
    anti_join(.lowest_names)
  # just these unknown animals!
  # check that the names are acceptable and message: "6516", "6511", "6506"

  mysteries <- .taxonomy_cols %>%
    anti_join(.lowest_names) %>%
    assert(in_set(c("6516", "6511", "6506")), species_id)

  # if that worked, let us know
  message("There are only three taxa with no trait information at all. Three Unknown animals")

  # we add on subspecies, no matter _what_ is the lowest level of the species --
  # ie even if it is not species. That is because sometimes "subspecies" is
  # actually just life history stage
  with_taxon_names <- .lowest_names %>%
    # add back in those subspecies!
    left_join(subspp) %>%
    mutate(taxon_name_2 = if_else(!is.na(subspecies),
                                  paste0(taxon_name, "_", subspecies),
                                  taxon_name)) %>%
  select(species_id, taxon_level, taxon_name = taxon_name_2, taxon_number)

  return(with_taxon_names)
}

# must get the taxonomic traits -------------------------------------------

get_trait_spreadsheet <- function() {
  # post_editing <- gs_title("traits_for_editing_20-01-2017") %>%
  #   gs_read_csv()
  post_editing <- read_sheet("traits_for_editing_20-01-2017")

  post_edit_no_dup <- post_editing %>%
    select(-starts_with("reference"))

  ## check that no rows are duplicate
  if( nrow(post_edit_no_dup %>% filter(duplicated(.))) > 0) stop("duplicates present")

  return(post_edit_no_dup)
}


merge_trait_by_taxonomy <- function(.trait_spreadsheet, .lowest_taxonomic){

  # there should be ABSOLUTELY NO taxa in the database that cannot find a match in
  # the trait table. Stop the party if that is not true
  message("checking that the taxonomy spreadsheet contains all the correct species")
  .lowest_taxonomic %>% anti_join(.trait_spreadsheet) %>% verify(nrow(.) == 0)

  ## prepare spreadsheet traits for merging
  taxonomic_traits <- .trait_spreadsheet %>%
    rename(taxon_number = tax_num)

  # merge traits by taxonomy
  new_trait_table <- .lowest_taxonomic %>%
    left_join(taxonomic_traits, by = c("taxon_name", "taxon_level", "taxon_number"))

  # absolutely no increase in row numbers
  stopifnot(nrow(new_trait_table) == nrow(.lowest_taxonomic))
  message("traits combined with no duplication of species")

  return(new_trait_table)
}

get_canonical_traits <- function(.trts_all_filtered) {
  .trts_all_filtered %>%
    select_("species_id","names", "bwg_name", "domain", "kingdom", "phylum", "subphylum",
            "class", "subclass", "ord", "subord", "family", "subfamily",
            "tribe", "genus", "species", "subspecies", "functional_group",
            "predation", "realm", "micro_macro", "barcode")

}


