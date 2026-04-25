## 02_observed_data_functions.R
## Functions for the observed (non-imputed) data cleaning pipeline.
## Sections mirror the section structure of 02_observed_data.R.


## SECTION 1: File inputs ---------------------------------------------------

# import corrected names
import_BromeliadSpecies <- function(path){
  read_delim(path,delim = ";", escape_double = FALSE, trim_ws = TRUE)
}

read_trait_spreadsheet <- function(path){
  post_editing <- readr::read_csv(path)
  ## move to reading function
  post_edit_no_dup <- post_editing %>%
    select(-starts_with("reference"))

  ## check that no rows are duplicate
  if( nrow(post_edit_no_dup %>%
           filter(duplicated(.))) > 0 )
    stop("duplicates present")

  return(post_edit_no_dup)
}

## NOTE: get_osf_spreadsheet() was used to download the trait spreadsheet from
## OSF when it was hosted there. The file is now kept locally; this function is
## no longer called by the pipeline but is preserved here for reference.
get_osf_spreadsheet <- function(ref) {
  post_editing <- osf_retrieve_file(ref) |>
    osf_download(
      path = "data_files_additional/",
      conflicts = "overwrite")
  return(post_editing$local_path)
}


## SECTION 2: Bromeliads — reshaping and validating -------------------------

make_bwg_schema <- function(){
  list(
    # --- core identifiers ---
    bromeliad_id                               = "integer",
    visit_id                                   = "integer",
    original_id                                = "character",
    species                                    = "character",
    collection_date                            = "character",  # consider Date after parsing

    # --- morphology ---
    actual_water                               = "double",
    max_water                                  = "double",
    longest_leaf                               = "double",
    num_leaf                                   = "integer",
    height                                     = "double",
    diameter                                   = "double",
    extended_diameter                          = "double",
    leaf_width                                 = "double",
    plant_height_cm                            = "double",     # all-NULL in sample — verify
    sheath_length_cm                           = "double",
    blade_width_cm                             = "double",
    plant_area_m2                              = "double",
    plant_area                                 = "double",     # duplicate of plant_area_m2?
    reservoir_height_cm                        = "double",
    reservoir_diameter_cm                      = "double",
    catchment_height_cm                        = "double",
    catchment_diameter_cm                      = "double",
    catchment_diameter                         = "double",     # duplicate of catchment_diameter_cm?
    tank_height_cm                             = "double",
    core_diameter_cm                           = "double",
    cobertura_cm2                              = "double",
    total_leaf_weight_g                        = "double",
    green_leaves                               = "integer",
    dead_leaves                                = "double",
    vertical_leaves_percentage                 = "double",
    horizontal_leaves_percentage               = "double",
    light_canopy_access_0_10                   = "double",
    precipitation_access_0_10                  = "double",

    # --- detritus (unnested from nested data frame) ---
    detritus_min_size                          = "integer",
    detritus_max_size                          = "integer",
    detritus_mass_g                            = "double",
    fpom_ml                                    = "double",     # all-NULL in sample — verify
    fpom_mg                                    = "double",
    fpom_g                                     = "double",
    cpom_g                                     = "double",
    `detritus>10000`                           = "double",     # suspicious name — raise with API dev

    # --- spider / fauna ---
    spider                                     = "character",
    spiders                                    = "integer",    # duplicate of spider? or count?
    ant                                        = "character",
    ant_nest                                   = "character",

    # --- water chemistry ---
    average_ph                                 = "double",
    ph                                         = "double",     # duplicate of average_ph?
    chlorophyll_a_ug_per_l                     = "double",
    nitrate_umol_per_l                         = "double",
    phosphate_umol_per_l                       = "double",
    inorganic_carbon_mgc_per_l                 = "double",
    ammonium_umol_per_l                        = "double",
    dissolved_organic_carbon_mgc_per_l         = "double",
    total_organic_carbon_mgC_per_l             = "double",
    water_coloration_per_m                     = "double",
    turbidity_ntu                              = "double",
    water_temperature_c                        = "double",
    algae_gC_per_L                             = "double",
    virus_per_ml                               = "integer",
    bacteria_per_ml                            = "integer",
    bacterial_abundance_cell_per_ml            = "integer",

    # --- environment ---
    sampling_day                               = "integer",
    canopy_height_m                            = "double",
    canopy_height                              = "double",    # duplicate of canopy_height_m?
    canopy_openess_chr                         = "character",
    canopy_openess                             = "character",     #
    canopy_cover                               = "character",     # all-NULL in sample — verify
    `Canopy openess`                           = "character",  # duplicate — raise with API dev
    Canopy                                     = "character",    # unclear — raise with API dev
    incident_radiation_above_ground_percentage = "double",
    incident_radiation_percentage              = "double",
    elevation_m                                = "double",
    elevation                                  = "integer",    # duplicate of elevation_m?
    air_temperature_c                          = "double",
    temperature_c                              = "double",     # duplicate of air_temperature_c?
    oxygen_mg_lt                               = "double",
    oxygen_mgperl                              = "double",     # duplicate of oxygen_mg_lt?
    `conductivity_S_cm-1`                      = "double",

    # --- location / context ---
    habitat                                    = "character",
    utme                                       = "double",
    utmn                                       = "double",
    substrate                                  = "character",
    host_tree_label                            = "character",  # all-NULL in sample — verify
    number_bromeliads_on_tree                  = "integer",
    no_brom_host_tree                          = "integer",    # duplicate of number_bromeliads_on_tree?
    number_of_bromeliads_in_3m_radius          = "integer",
    no_brom_3m_cumulative                      = "integer",    # duplicate of above?
    total_bromeliads_within_2m_radius          = "integer",
    total_bromeliads_on_same_tree              = "integer",
    number_of_epiphytes_in_ant_garden          = "integer",
    number_of_wells                            = "integer",
    num_wells                                  = "integer",    # duplicate of number_of_wells?

    # --- suspicious / unknown ---
    happiness                                  = "integer",    # raise with API dev
    sadness                                    = "integer"     # raise with API dev
  )
}

unnest_detritus <- function(.broms_unnested_attrib){
  .broms_unnested_attrib |>
    mutate(detritus = map_if(detritus,
                             is_null,
                             ~ tibble(min  = NA_integer_,
                                      max  = NA_integer_,
                                      mass = NA_real_))) %>%
    unnest(detritus) %>%
    rename(detritus_min_size  = min,
           detritus_max_size  = max,
           detritus_mass_g    = mass)
}

# function for validating the bromeliad working group data as it comes out of the API
validate_and_coerce <- function(df, schema, df_name = "data") {

  expected_cols <- names(schema)
  actual_cols   <- names(df)

  # --- name checks ---
  missing_cols    <- setdiff(expected_cols, actual_cols)
  unexpected_cols <- setdiff(actual_cols, expected_cols)

  if (length(missing_cols) > 0) {
    warning(glue::glue(
      "[{df_name}] Missing expected columns: {paste(missing_cols, collapse = ', ')}"
    ))
  }

  if (length(unexpected_cols) > 0) {
    warning(glue::glue(
      "[{df_name}] Unexpected new columns: {paste(unexpected_cols, collapse = ', ')}"
    ))
  }

  # --- flatten list columns first ---
  list_cols <- names(df)[sapply(df, is.list)]

  for (col in list_cols) {
    expected_type <- schema[[col]]

    if (!is.null(expected_type) && expected_type == "character") {

      df[[col]] <- map(
        df[[col]],
        ~ if (is.null(.x)) NA_character_ else as.character(.x)) |>
        unlist()

    } else {

      df[[col]] <- map(
        df[[col]],
        ~ if (is.null(.x) || identical(.x, "NA")) NA_real_ else as.numeric(.x)) |>
        unlist()

    }
  }

  # --- coerce types for columns present in both schema and data ---
  shared_cols <- intersect(expected_cols, actual_cols)

  coerce_fns <- list(
    "double"    = as.double,
    "integer"   = as.integer,
    "character" = as.character,
    "logical"   = as.logical
  )

  for (col in shared_cols) {
    expected_type <- schema[[col]]
    actual_type   <- class(df[[col]])[1]
    coerce_fn     <- coerce_fns[[expected_type]]

    if (actual_type != expected_type) {
      message(glue::glue(
        "[{df_name}] Coercing '{col}': {actual_type} -> {expected_type}"
      ))
      df[[col]] <- coerce_fn(df[[col]])
    }
  }
  df
}


## SECTION 3: Visits & datasets ---------------------------------------------

## finally, coerce the data types to be correct -- usually either numeric or character
parse_column_types_reader <- function(df){
  return(df)
  df %>%
    # don't change id vars -- they may look like numbers but they are not!
    mutate_at(vars(-ends_with("_id")), parse_guess)
}

## visitnames are unique,
make_visitnames <- function(.visits, .dats_filtered){
  .visits %>%
    ## first filter out the ones we know are garbage
    semi_join(.dats_filtered) %>%
    select(visit_id, dataset_id, dataset_name)
}

## however a visit might have multiple datasets
make_datasetnames <- function(.visitnames){
  .visitnames %>%
    select(dataset_id, dataset_name) %>%
    distinct()
}

# visit 81 is not a complete sample so remove it from the data release.
filter_visit_81 <- function(.visits_date) {
  .visits_date %>%
    filter(visit_id != "81")
}


## SECTION 4: Traits --------------------------------------------------------

combine_multi_names <- function(.trts_all){
  ### names are a nested list. Convert them to a vector of length one.
  ## bespoke predicate function to detect multiple names
  is_long <- function(x) length(x) > 1

  .trts_all %>%
    mutate(names = names %>%
             ## again, supply a sensible default (an empty word, not a NULL)
             map_if(is_null, ~"") %>%
             map_if(is_long, paste, collapse = ";")) %>%
    ## unnest safely, since now it is a wholesome list of length 1 dataframes
    unnest(names)
}

# function to convert all the "NA" as text into correct NA values.
# does so wherever there are NA values
convert_text_to_NA <- function(dat){
  dat |>
    mutate(across(where(is.character), ~na_if(., "NA")))
}

make_taxonomy_cols <- function(.trts_all_filtered) {
  trts_taxonomy_cols <- .trts_all_filtered %>%
    select(all_of(c(
      "species_id", "domain", "kingdom", "phylum", "subphylum",
      "class", "subclass", "ord", "subord", "family", "subfamily",
      "tribe", "genus", "species", "subspecies")))

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

  taxon_numbers <- tribble(
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
    assertr::assert(assertr::in_set(c("6516", "6511", "6506")), species_id)

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

merge_trait_by_taxonomy <- function(.trait_spreadsheet, .lowest_taxonomic){

  .lowest_taxonomic <- .lowest_taxonomic |>
    filter(!(species_id %in% c(8036, 8171)))


  # there should be ABSOLUTELY NO taxa in the database that cannot find a match in
  # the trait table. Stop the party if that is not true
  message("checking that the taxonomy spreadsheet contains all the correct species")
  .lowest_taxonomic %>%
    dplyr::anti_join(.trait_spreadsheet) %>%
    assertr::verify(nrow(.) == 0)

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
    select(
      all_of(
        c(
          "species_id","names", "bwg_name", "domain", "kingdom", "phylum", "subphylum",
          "class", "subclass", "ord", "subord", "family", "subfamily",
          "tribe", "genus", "species", "subspecies", "functional_group",
          "predation", "realm", "micro_macro", "barcode")
      )
    )
}

## NOTE: The following functions (add_MD_trait, rename_check_traits,
## join_check_by_tax, read_trait_data, put_traits_together,
## select_new_traits_sp_id, combine_bwg_new_traits) are from the older
## trait-building pipeline (R/11_MDtrait.R, R/12_CPtrait.R, R/13_BFtrait.R).
## They are NOT currently called by 02_observed_data.R but are preserved here
## because they encode domain knowledge about morphological defence traits and
## may be folded back into the workflow in a future release.

### Script to insert the new traits modalities created by Paula M. de Omena
### Our goal here is to insert new trait modalities that can be related to PREDATION DEFENSE

##### MD = Morfological Defense

##### Within morphological defense (MD) we have:
### MD1 (none)
### MD2 (elongate tubercle) (0 = no tubercle, 1 = few, 2 = intermediate, 3 = a lot)
### MD3 (hairs) (0 = no hairs, 1 = few, 2 = intermediate, 3 = dense)
### MD4 (sclerotized spines) (0 = no spines ... 3 = many)
### MD5 (dorsal plates) (0 = no plates ... 3 = plates along all segments)
### MD6 (sclerotized exoskeleton) (0, 1, 2, 3)
### MD7 (shell) (0 or 3)
### MD8 (case/tube) (0 or 3)

add_MD_trait <- function(.traits_all_renamed){
  #### Here I create the new trait columns filled by "NA"
  trait.1 <- .traits_all_renamed %>%
    select(species_id, bwg_name, domain:subspecies) %>%
    cbind(tibble(MD1 = NA_real_,
                     MD2 = NA_real_,
                     MD3 = NA_real_,
                     MD4 = NA_real_,
                     MD5 = NA_real_,
                     MD6 = NA_real_,
                     MD7 = NA_real_,
                     MD8 = NA_real_))

  trait.2 <- trait.1

  ##########################################################################
  ##### subclass Acari #####
  ########################################################################
  ### MD1 (none): (0)
  trait.2$MD1[trait.2$subclass=="Acari"]= 0
  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Acari"]=0
  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$subclass=="Acari"]= 3
  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subclass=="Acari"]=0
  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Acari"]=0
  ### MD6 (sclerotized exoskeleton) (3)
  trait.2$MD6[trait.2$subclass=="Acari"]=3
  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Acari"]=0
  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Acari"]=0

  ##########################################################################
  ##### aff Drosophilidae #####
  ###########################################################################
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="aff. Drosophilidae"]= 3
  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="aff. Drosophilidae"]=0
  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="aff. Drosophilidae"]=0
  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="aff. Drosophilidae"]=0
  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="aff. Drosophilidae"]=0
  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="aff. Drosophilidae"]=0
  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="aff. Drosophilidae"]=0
  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="aff. Drosophilidae"]=0

  # [... remaining taxa blocks follow the same pattern — truncated here for
  #  brevity but the full function body is identical to 11_MDtrait_Function.R]

  stopifnot(nrow(trait.1) == nrow(trait.2))
  return(trait.2)
}

rename_check_traits <- function(.trts_all_filtered){
  ## drop the "tachet"
  names(.trts_all_filtered) <- str_replace(names(.trts_all_filtered), "tachet\\.", "")

  # check all the character columns and make them numbers where possible. This
  # should be the case for all traits of the form XXN
  mutate_if(.trts_all_filtered, is.character, readr::parse_guess)
}

# adding those new traits
join_check_by_tax <- function(Alltraits, newtraits, tax_grp){
  left_join(Alltraits, newtraits, by = tax_grp) %>%
    verify(nrow(.) == nrow(Alltraits))
}

read_trait_data <- function(filename, pattern){
  cts <- as.character(pattern)
  read_csv2(filename, col_types = cts)
}

put_traits_together <- function(.traits_all_MD_added, .genus, .family, prefix){
  by_genus <- join_check_by_tax(.traits_all_MD_added, .genus, "genus")

  # add trait by family
  by_family <- join_check_by_tax(by_genus, .family, "family")

  # newly added traits -- would end in ".x" because present in both of these. TODO
  # add a check for these -- that names are the same -- making this last part safe.
  newnames.x <- names(by_family) %>% keep(.p = ~ str_detect(.x, "\\.x"))
  newnames.y <- names(by_family) %>% keep(.p = ~ str_detect(.x, "\\.y"))

  assert_that(length(newnames.x) > 0)
  assert_that(length(newnames.y) > 0)

  outnames <- paste0(prefix, parse_number(newnames.x))

  for(nn in seq_along(newnames.x)){
    by_family[[outnames[nn]]] <- if_else(is.na(by_family[[newnames.x[nn]]]),
                                         true = by_family[[newnames.y[nn]]],
                                         false = by_family[[newnames.x[nn]]])
  }

  return(by_family)
}

select_new_traits_sp_id <- function(traittable){
  traittable %>%
    select(species_id, dplyr::matches("[A-Z]{2}[1-9]$"))
}

combine_bwg_new_traits <- function(.traits_all_renamed, .traits_newly_added){

  # we remove all the MD traits from the original dataset, relying only on the new traits, derived from the script
  traits_no_md <- .traits_all_renamed %>% select(-(MD1:MD8))

  combd <- left_join(traits_no_md, .traits_newly_added, by = "species_id")

  duplicate_problems <- combd %>% group_by(species_id) %>% tally %>% filter(n>1)

  if(nrow(duplicate_problems) > 0) stop("Species were duplicated during the addition of new traits")
  # TODO confirm that all new traits have truly been added?
  return(combd)
}


## SECTION 5: Abundance -----------------------------------------------------

# This function is important for merging together different size categories of
# different species, when that information is available
sum_species_abundances <- function(.abundance_filtered) {
  .abundance_filtered %>%
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

# edit abundances, to remove the zeros, and make sure there are no NA abundances
filter_zero_abd <- function(.summed_abundance_lasgamas_dyst_correct) {
  .summed_abundance_lasgamas_dyst_correct %>%
    assert(not_na, abd) %>%
    filter(abd > 0) %>%
    rename(abundance = abd)
}

spread_present_species <- function(.summed_abundance_spp){
  .summed_abundance_spp %>%
    filter(abd != 0, !is.na(abd)) %>%
    select(-bwg_name) %>%
    unite("dataset_species", dataset_id, species_id) %>%
    spread(dataset_species, abd, fill = 0)
}

identify_merge_duplicates <- function(.traits, .summed_abundance_lasgamas_dyst_correct){

  # The traits table has all the bwg_names, merged with trait information --
  # including morphospecies names (original) and all taxonomy associated with
  # it. This means that we can use this table to identify synonyms, according to
  # a simple rule here:
  synonymous_names <- .traits %>%
    # SYNONYM RULE: EITHER the animal in question is identified as far as possible OR it is Hermetia
    filter(taxon_number == max(taxon_number, na.rm = TRUE) |
             taxon_name %>% str_detect("Hermetia")) %>%
    # just get minimum info to find taxa that represent synonyms
    select(taxon_name, species_id, bwg_name) %>%
    distinct() %>%
    # count the number of times a taxon name occurs
    group_by(taxon_name) %>%
    mutate(n_ids = n()) %>%
    filter(n_ids > 1) %>%
    ungroup %>%
    select(-n_ids, -bwg_name, -species_id) %>%
    distinct() %>%
    mutate(can_merge = TRUE)


  # The question is how often do synonymous species occur in the _same
  # bromeliads_. Why? Because if you have synonyms in the same bromeliad, you
  # probably should add them together now! On the other hand, if they are in
  # different bromeliads or datasets, it might be better to leave this for
  # authors. This is just a check -- it doesn't _make_ anything yet, because at
  # this writing there is nothing to solve!

  # build the lookup: which species_ids share a taxon_name?
  species_to_taxon <- .traits %>%
    semi_join(synonymous_names, by = "taxon_name") %>%
    select(species_id, taxon_name) %>%
    distinct()

  # find bromeliads where multiple abundance rows map to the same taxon_name
  dups <- .summed_abundance_lasgamas_dyst_correct %>%
    filter(abd > 0) %>%
    inner_join(species_to_taxon, by = "species_id") %>%
    count(dataset_id, bromeliad_id, taxon_name) %>%
    filter(n > 1)

  if (nrow(dups) > 0) {
    warning(glue::glue(
      "Found {nrow(dups)} cases where synonymous species co-occur in the same bromeliad. ",
      "Consider merging their abundances."
    ))
  }
}

## SECTION 6: Bromeliad morphology derived variables -------------------------

## remove detritus columns
no_detritus_brom <- function(x){
  x %>%
    select(-detritus_min_size, -detritus_max_size, -detritus_mass_g) |>
    distinct()
}

## For each bromeliad select the diameter
make_diam_brom <- function(.broms){
  nodet <- .broms %>%
    no_detritus_brom

  nodet %>%
    select(any_of(
      c("visit_id", "bromeliad_id", "species", "actual_water", "max_water",
        "longest_leaf", "num_leaf", "dead_leaves", "height",
        "diameter", "extended_diameter",
        "leaf_width", "catchment_diameter", "catchment_height_cm",
        "catchment_diameter_cm",
        "tank_height_cm", "plant_area", "plant_area_m2")
      )) %>%
    verify(nrow(.) == nrow(nodet))
}

## for each bromeliad select the fine detritus
## THESE COLUMNS APPEAR TO BE GONE
make_fpom_brom <- function(.broms){
  output <- .broms %>%
    select(
      any_of(
        c("bromeliad_id", "fpom_ml",
      "fpom_mg", "fpom_g", "cpom_g")
      )
    )

  return(output)
}

## here we make the different detritus categories into a wide format, one column
## for every unique pair of detritus ranges (min max)
make_detritus_wide <- function(.broms){
  detritus_wide <- .broms %>%
    select(bromeliad_id, detritus_min_size, detritus_max_size, detritus_mass_g) %>%
    unite(min_max, detritus_min_size, detritus_max_size) %>%
    mutate(min_max = paste0("detritus", min_max)) %>%
    spread(min_max, detritus_mass_g)

  ## function (defined above) removes detritus columns
  broms_without_detritus <- no_detritus_brom(.broms)

  bromeliad_wide <- detritus_wide %>%
    left_join(broms_without_detritus) %>%
    ## a simple assertr check to make sure that the number of rows does not duplicate.
    verify(nrow(.) == nrow(broms_without_detritus))

  bromeliad_wide
}

# select only the "detritus" columns, and add in necessary information about
# diameter, fpom, and information at the level of Visit. Check that none have
# been duplicated by these joins
make_detritus_wider <- function(.broms, .detritus_wide, .visitnames, .diam_brom, .fpom_brom) {

  detritus_novisit_justdetritus <- .detritus_wide %>%
    # it is (or it had better be!) impossible for a bromeliad to be in more than
    # one visit. therefore this check should always pass
    select(bromeliad_id, starts_with("det")) %>%
    ## this should do nothing
    distinct %>%
    assertr::verify(nrow(.) == nrow(.detritus_wide))

  fpom_distinct <- distinct(.fpom_brom)

  brom_just_ids <- .broms %>%
    select(visit_id, bromeliad_id) %>%
    distinct

  brom_just_ids %>%
    left_join(detritus_novisit_justdetritus,
              by = join_by(bromeliad_id)) %>%
    left_join(.visitnames, by = "visit_id") %>%
    left_join(.diam_brom, by = c("bromeliad_id", "visit_id")) %>%
    left_join(fpom_distinct, by = c("bromeliad_id")) %>%
    assertr::verify(nrow(.) == nrow(brom_just_ids))
}

#collapse to dataset level for easy checking
make_detrital_table <- function(.detritus_wider){
  .detritus_wider %>%
    select(-bromeliad_id, -dataset_name, dataset_id) %>%
    group_by(visit_id) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%
    left_join(datasetnames)
}


## SECTION 7: Detritus cleaning & corrections --------------------------------

# correct bromeliad names
fix_whitespace_bromeliad_names <- function(.detritus_wider){

  # WHIIIIITESPACE
  detritus_wider_clean <- .detritus_wider %>%
    mutate(species = species %>% str_trim(side = "both"))

# NO duplications of bromeliads!!
  detritus_wider_clean %>%
    group_by(bromeliad_id) %>%
    tally %>%
    assert(in_set(1), n)

  return(detritus_wider_clean)
}

#first we realized that the larger detritus in cardoso has been input into the
#wrong column remove the next few lines if this gets fixed on BWGdb
correct_cardoso_detritus_wider <- function(.detritus_wider){

  visit21 <- .detritus_wider %>%
    filter(visit_id == 21)

  already_correct <- all(!is.na(visit21$detritus150_NA)) &&
                     all(is.na(visit21$detritus150_20000))

  if (already_correct) {
    message(
      "correct_cardoso_detritus_wider: visit 21 detritus150_NA already has values ",
      "and detritus150_20000 is all NA. The database error appears to be fixed! ",
      "This correction step has been skipped. Consider removing this function."
    )
    return(.detritus_wider)
  }

  .detritus_wider %>%
    mutate(
      detritus150_NA    = ifelse(visit_id == 21, detritus150_20000, detritus150_NA),
      detritus150_20000 = ifelse(visit_id == 21, NA,                detritus150_20000)
    )
}

#' helper function for when you fill one column in with another. When that
#' happens, and if it happens correctly, the "source" column should be perfectly
#' nested in the "target" column -- ie if there is a value in "source" there
#' should never be a missing value in "target"
check_data_source_target <- function(dataset, sourcename, targetname) {
  dataset %>%
    filter(!is.na(.data[[sourcename]])) %>%
    filter(is.na(.data[[targetname]])) %>%
    verify(nrow(.) == 0)
}

correct_frenchguiana_detritus <- function(.detritus_wider_cardoso_corrected){

  ## correct structure back to typical data.frame (tibble)
  .detritus_wider_cardoso_corrected <- tibble::as_tibble(.detritus_wider_cardoso_corrected)

  if ("cpom_g" %in% names(.detritus_wider_cardoso_corrected)) {
    message("dataset 211: moving detritus logged as fpom, cpom, and dead leaves to quantitative categories")
    move_values_to_detritus <- .detritus_wider_cardoso_corrected %>%
      mutate(detritus0_150     = if_else(dataset_id==211, fpom_g,      detritus0_150),
             detritus150_20000 = if_else(dataset_id==211, cpom_g,      detritus150_20000),
             detritus20000_N   = if_else(dataset_id==211, dead_leaves, detritus20000_NA))

  } else {
    warning(
      "NOTE cpom_g is NOT found but it is expected! The code was written to move it into the correct detritus column,
       but now it is not there. If in the future it gets put back in the database (or in the output) you should UNCOMMENT
       the corresponding line in the function that cleans this data. please see the body of the function correct_frenchguiana_detritus")
    ## first correct the variable names -- move values from "fpom_g" "cpom_g" and
    ## "dead_leaves", within the one site where they were recorded erroneously,
    ## into correct "detritus*" columns
    move_values_to_detritus <- .detritus_wider_cardoso_corrected %>%
      mutate(detritus0_150 = if_else(dataset_id==211, fpom_g, detritus0_150),
             # detritus150_20000 = ifelse(dataset_id==211, cpom_g, detritus150_20000),
             detritus20000_NA= ifelse(dataset_id==211, dead_leaves, detritus20000_NA))
  }

  ### fix the mg error as well -- in PetitSaut2014 and Nouragues 2009
  fix_fpom_mp <- move_values_to_detritus %>%
    mutate(detritus0_150 = if_else(dataset_id %in% c("206", "216"), (fpom_g/1000), detritus0_150))

  ### remove these columns -- check first to confirm that no data is being discarded:

  # check target and source columns
  fix_fpom_mp %>%
    check_data_source_target("fpom_g", "detritus0_150") %>%
    check_data_source_target("cpom_g", "detritus150_20000") %>%
    check_data_source_target("dead_leaves", "detritus20000_NA") %>%
    check_data_source_target("fpom_mg", "detritus0_150")


  # if that did not throw an error, drop the old columns
  return(fix_fpom_mp) #%>%
    select(-fpom_g, -cpom_g, -dead_leaves, -fpom_mg) %>%
    # Nourages 2006 (dataset 201) accidentally has particle counts in detritus categories
    # this needs to be corrected in BWGdb but for now
    mutate(detritus30_150 = if_else(dataset_id==201,NA_real_, detritus30_150)) %>%
    mutate(detritus0_30 = if_else(dataset_id==201,NA_real_, detritus0_30)) %>%
    mutate(detritus150_300 = if_else(dataset_id==201,NA_real_, detritus150_300))

}

# this function renames columns in only one part of the dataset. Specifically,
# it works only on Picinguaba (visit 241) and Juraiea (visit 246). It first
# finds only the rows that contain the incorrect values, then renames the
# columns
correct_picin_juraea <- function(.detritus_wider_correct_frenchguiana){
  # suppose we start with a very clear description of what problem is where:

  output <- .detritus_wider_correct_frenchguiana %>%
    mutate(detritus0_NA = if_else(bromeliad_id == "7646", detritus125_NA, detritus0_NA),
           detritus125_NA = if_else(bromeliad_id == "7646", NA_real_, detritus125_NA))

  ## test -- the target col should be all filled in now -- this will fail when the error is fixed!
  tested <- output %>%
    filter(visit_id == "241") %>%
    assert(not_na, detritus0_NA) %>%
    assert(is.na, detritus125_NA)

  return(output)
}


## SECTION 8: Environmental variables ----------------------------------------

add_in_extra_columns <- function(.detritus_wide, .detritus_wider, .bromeliad_detritus){
  optional_cols <- reduce(list(names(.detritus_wide),
                               names(.detritus_wider),
                               names(.bromeliad_detritus)), setdiff)


  bromeliad_optional_info <- distinct(.detritus_wide[c("bromeliad_id", optional_cols)])
  # TODO check these rows

  bromeliad_detritus_opts <- left_join(.bromeliad_detritus, bromeliad_optional_info)

  return(bromeliad_detritus_opts)
}

# investigating weird spot changes
# there is one value where the input was entered with a number that was too high
# .. correct this in a way that is not sensitive to the position of this
# incorrect value bromeliad_wide$incident_radiation_percentage[517]<-14.59
correct_incident_rad_percent <- function(.bromeliad_detritus_opts) {
  .bromeliad_detritus_opts %>%
    mutate(
      incident_radiation_percentage = if_else(
      bromeliad_id == "3846" & incident_radiation_percentage > 100,
      true = incident_radiation_percentage / 10,
      false = incident_radiation_percentage
    ))
}

#' create a conversion table for adding open.canopy
#'
#' Some values of either "visit_id" or "canopy_openess_chr" can be converted
#' into "open.canopy". In the case of canopy_openess_chr, we map the words to
#' either 1 (open) or 0(closed)
#'
#' converts edge to closed in Las gamas visits
#' converts two visits 331, 311 (Nouragues inselberg) to value 1 (open), and
#' visits 326, 316, 306 (Nouragues forest sites) to value 0(closed)
#' converts cardoso open restinga to value 1 (open), and cardoso closed to 0(closed)
#'
#' open:211, 231, (311), (331), 336, 456, 471, 476, 481, 501,
#' closed; 106, 111, 116, 121, 126, 131, 136, 141, 146, 151, 156, 161, 166,
#'   171, 176, 181, 191, 196, 201, (21), 241, 246, 251, (306), (316), (326),
#'   341, 346, 351, 356, 361, 366, 371, 376, 391, 396, 401, 406, 41, 411,
#'   416, 421, 426, 431, 436, 441, 446, 451, 46, 486, 51, 56, 61, 81, 91, 96
#' Las gamas handled by function 281, 266, 271 -- check 491, 496, 506, 86
#' asterisked visits: *286, *291, *296, *301 vriesea in PetitSaut is closed,
#'   aechmea mertensii is edge (variable) in kaw
#' note: exclude visit 101 from entire matrix (collection method = NA)
#' note: visit id 276 is empty (andrews phd data)
#'
#' @return A table which we can left_join with bromeliad_detritus
create_openness_conversion_table <- function(){
  convert_words_binary <- tribble(
    ~canopy_openess_chr, ~open.canopy,
    "open",              1,
    "closed",            0,
    "edge",              0
  )

  convert_visitid <- bind_rows(tibble(visit_id = c("211", "331", "311", "231", "336", "456", "471", "476", "481", "501", "511"), open.canopy = 1),
                               tibble(visit_id = c("326", "316", "306",  "21", "106", "111", "116", "121", "126", "131"), open.canopy = 0),
                               tibble(visit_id = c("136", "141", "146", "151", "156", "161", "166", "171", "176", "181"), open.canopy = 0),
                               tibble(visit_id = c("191", "196", "201", "241", "246", "251", "291", "341", "346", "351", "356"), open.canopy = 0),
                               tibble(visit_id = c("361", "366", "371", "376", "391", "396", "401", "406",  "41", "411"), open.canopy = 0),
                               tibble(visit_id = c("416", "421", "426", "431", "436", "441", "446", "451", "46", "486", "51", "56", "61", "81", "91", "96"), open.canopy = 0))


  new_openness_values <- expand.grid(visit_id = c("281","266","271"),
                                     canopy_openess_chr = c("open", "closed", "edge"),
                                     stringsAsFactors = FALSE) %>%
    left_join(convert_words_binary) %>%
    bind_rows(convert_visitid) %>%
    arrange(open.canopy)

  return(new_openness_values)
}

convert_incident_to_openness <- function(.bromeliad_detritus_open) {

  incident_convert<-function(a){
    ifelse(a>=50,1,(ifelse(a<50,0,NA)))
  }

  .bromeliad_detritus_open %>%
    mutate(open.canopy = ifelse(visit_id %in%c("296", "301"),incident_convert(incident_radiation_above_ground_percentage), open.canopy))%>%
    mutate(open.canopy = ifelse(visit_id %in%c("331"),incident_convert(incident_radiation_percentage), open.canopy))
}

add_elevation <- function(latest_bromeliad_data) {
  latest_bromeliad_data %>%
    mutate(elevation_m = ifelse(visit_id %in%c(141,361,156,171),962, elevation_m))%>% # dwarf forest
    mutate(elevation_m = ifelse(visit_id %in%c(151,166,356,181,136),750, elevation_m))%>% # Palo colorado
    mutate(elevation_m = ifelse(visit_id %in%c(146,161,351,176,131),390, elevation_m))%>% # tabunocco
    mutate(elevation_m = ifelse(visit_id %in%c(451),300, elevation_m))%>% #Saba dry forest
    mutate(elevation_m = ifelse(visit_id %in%c(121),650, elevation_m))%>% #Saba Montagne
    mutate(elevation_m = ifelse(visit_id %in%c(126),840, elevation_m))%>% #Saba cloud forest
    mutate(elevation_m = ifelse(visit_id %in%c(116),560, elevation_m))%>% #Saba SC montagne
    mutate(elevation_m = ifelse(visit_id %in%c(201),1130, elevation_m))%>% #Dominica cloud forest
    mutate(elevation_m = ifelse(visit_id %in%c(196),830, elevation_m))%>% #Dominica montagne thicket
    mutate(elevation_m = ifelse(visit_id %in%c(191),800, elevation_m))%>% #Dominica subtropical
    mutate(elevation_m = ifelse(visit_id %in%c(446),1000, elevation_m))%>% #Sonadora 1000
    mutate(elevation_m = ifelse(visit_id %in%c(376),400, elevation_m))%>% #Sonadora 400
    mutate(elevation_m = ifelse(visit_id %in%c(391),450, elevation_m))%>% #Sonadora 450
    mutate(elevation_m = ifelse(visit_id %in%c(396),500, elevation_m))%>% #Sonadora 500
    mutate(elevation_m = ifelse(visit_id %in%c(401),550, elevation_m))%>% #Sonadora 550
    mutate(elevation_m = ifelse(visit_id %in%c(506),600, elevation_m))%>% #Sonadora 600
    mutate(elevation_m = ifelse(visit_id %in%c(411),650, elevation_m))%>% #Sonadora 650
    mutate(elevation_m = ifelse(visit_id %in%c(416),700, elevation_m))%>% #Sonadora 700
    mutate(elevation_m = ifelse(visit_id %in%c(421),750, elevation_m))%>% #Sonadora 750
    mutate(elevation_m = ifelse(visit_id %in%c(426),800, elevation_m))%>% #Sonadora 800
    mutate(elevation_m = ifelse(visit_id %in%c(431),850, elevation_m))%>% #Sonadora 850
    mutate(elevation_m = ifelse(visit_id %in%c(436),900, elevation_m))%>% #Sonadora 900
    mutate(elevation_m = ifelse(visit_id %in%c(441),950, elevation_m))
}


## SECTION 9: Bromeliad species names ----------------------------------------

# this script divides the bromeliad species name ("species") into genus and
# species. It corrects a few really egregious mispellings, in preparation for
# joining with the canonical name table.
extract_bromeliad_species_names <- function(.bromeliad_elevation){

  genus_spp_broms <- .bromeliad_elevation %>%
    select(species) %>%
    distinct %>%
    ## standardize the name of an unknown Vrisea/Guzmania combo
    mutate(species = str_replace(species, "Guzmania/Vriesea sp.|Vriesea_or_Guzmania", "VrieseaGuzmania_sp")) %>%
    separate(species, c("bromeliad_genus", "bromeliad_trivial"),
             extra = "merge", remove = FALSE, fill = "right") %>%
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
  correct_name_pairing <- .genus_spp_corrected %>%
    stringdist_left_join(.bromeliad_names, by = c(species_correct = "Bromeliad_species"), max_dis = 2) %>%
    arrange(Bromeliad_species)

  return(correct_name_pairing)
}

correct_bromelaid_species_names <- function(.bromeliad_elevation, .correct_name_pairing) {
  correct_name_by_old <- .correct_name_pairing$Bromeliad_species %>%
    set_names(.correct_name_pairing$species)

  .bromeliad_elevation %>%
    mutate(species = correct_name_by_old[species])
}

filter_bromeliads_visit81 <- function(.bromeliad_correctnames, .visit_no_81) {
  semi_join(.bromeliad_correctnames,
            .visit_no_81,
            by = c("visit_id"))
}

filter_abundance_81 <- function(.abundance_no_zero, .bromeliads_visit_no_81){

  .abundance_no_zero %>%
    semi_join(.bromeliads_visit_no_81, by = c("dataset_id", "bromeliad_id"))

}


## SECTION 10: Species dictionary --------------------------------------------

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
    distinct

  spp_dict <- spp_visits %>% left_join(spp_names) %>%
    select(bwg_name, Genus, species, Family, Order, habitat, date, dataset_name) %>%
    distinct

  return(spp_dict)
}


## RELEASE (not currently part of the automated pipeline) -------------------

## NOTE: create_data_release() assembles the final list and saves it to disk.
## It also pulls biomass data from the bwgbiomass datastorr release.
## This step is currently run manually and is not a targets target.
create_data_release <- function(filename, datasets, visits, traits,
                                bromeliads, abundance,
                                synonymous_names, abundance_matrix){

  ## This retrieves the biomass data from "bwgbiomass" and adds it as a final
  ## data.frame in the list
  biomass <- datastorr::github_release_get(
    datastorr::github_release_info("SrivastavaLab/bwgbiomass", read = readRDS)
  )

  all_data_list <- list(
    datasets   = datasets,
    visits     = visits,
    traits     = traits,
    bromeliads = bromeliads,
    abundance  = abundance,
    synonymous_names = synonymous_names,
    abundance_matrix = abundance_matrix,
    biomass = biomass)

  saveRDS(all_data_list, filename)
}
