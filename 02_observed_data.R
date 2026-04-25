## 02_observed_data.R
## Cleaning, correcting, and reshaping the BWG data
##
## This script takes raw downloaded data and produces a fully cleaned,
## corrected dataset with NO imputation. The output is a list object
## ("observed_data") that can be consumed by 03_imputed_data.R.
##
## Andrew MacDonald

library(targets)
library(tarchetypes)
tar_option_set(
  packages = c(
    "bwgdata",
    "dplyr",
    "bwgdata",
    "purrr",
    "readr",
    "tidyr",
    "stringr",
    "assertr", # for checking data
    "assertthat", # for checking arguments to functions
    "lazyeval",
    "broom",
    "fuzzyjoin")
  )

# read in functions
tar_source(files = "Rfunctions/02_observed_data_functions.R")

## read in data downloaded in 01_download_data.R
dats <- tar_read(dats, store = "store_download_data")
visits <- tar_read(visits, store = "store_download_data")
broms <- tar_read(broms, store = "store_download_data")
abds <- tar_read(abds, store = "store_download_data")
trts_all <- tar_read(trts_all, store = "store_download_data")


list(

  ## SECTION 1: File inputs (local) ------------------------------------------
  ## Only files needed for cleaning/correction

  tar_target(
    name = fuzzy_traits_csv,
    command = "data_files_additional/fuzzy_traits.csv",
    format = "file"
  ),
  tar_target(
    name = trait_spreadsheet,
    command = read_trait_spreadsheet(fuzzy_traits_csv)
  ),
  tar_target(
    name = bromeliad_names,
    command = import_BromeliadSpecies("data-intermediate/bromeliad_names.csv")
  ),

  ## SECTION 2: Bromeliads — reshaping and validating -------------------------

  # create schema for validating dataset
  tar_target(
    name = schema,
    command = make_bwg_schema()
  ),

  ## unnest the attributes column
  tar_target(
    name = broms_unnested_attrib,
    command = tidyr::unnest_wider(broms, attributes)
  ),

  # unnest the detritus column, adding columns
  #   - detritus_min_size
  #   - detritus_max_size
  #   - detritus_mass_g
  tar_target(
    name = brom_unnested_detritus,
    command = unnest_detritus(broms_unnested_attrib)
  ),

  # convert all columns to correct units and validate against the schema
  tar_target(
    name = brom_validated,
    command = validate_and_coerce(brom_unnested_detritus, schema),
  ),

  ## SECTION 3: Visits & datasets --------------------------------------------

  tar_target(
    name = visitnames,
    command = make_visitnames(visits, dats),
  ),

  tar_target(
    name = datasetnames,
    command = make_datasetnames(visitnames),
  ),

  tar_target(
    name = visit_no_81,
    command = filter_visit_81(visits),
  ),

  ## SECTION 4: Traits --------------------------------------------------------

  tar_target(
    name = trts_all_filtered,
    command = combine_multi_names(trts_all),
  ),

  tar_target(
    name = trts_parsed_cols,
    command = convert_text_to_NA(trts_all_filtered),
  ),

  tar_target(
    name = taxonomy_cols,
    command = make_taxonomy_cols(trts_parsed_cols),
  ),

  tar_target(
    name = lowest_taxonomic,
    command = get_lowest_taxonomic(taxonomy_cols),
  ),

  tar_target(
    name = canonical_traits,
    command = get_canonical_traits(trts_parsed_cols),
  ),

  tar_target(
    name = taxon_lowest_names,
    command = lowest_name_and_subspecies(taxonomy_cols, lowest_taxonomic),
  ),

  tar_target(
    name = traits_from_tax,
    command = merge_trait_by_taxonomy(trait_spreadsheet, taxon_lowest_names),
  ),

  tar_target(
    name = traits,
    command = left_join(canonical_traits |>
                          mutate(species_id = as.character(species_id)),
                        traits_from_tax |>
                          mutate(species_id = as.character(species_id)),
                        by = join_by("species_id")),
  ),

  ## SECTION 5: Abundance -----------------------------------------------------

  tar_target(
    name = pres_abds,
    command = keep(abds, is.list) |>
      keep(~ length(.x$species)>1)
  ),

  tar_target(
    name = abundance,
    command = tidy_dataset_list(pres_abds),
  ),

  tar_target(
    name = summed_abundance_spp,
    command = sum_species_abundances(
      tidyr::unnest(abundance, measurements) |>
        dplyr::mutate(
          abd = purrr::flatten_chr(abd),
          abd = readr::parse_double(abd)
        )
    ),
  ),

  tar_target(
    name = summed_abundance_lasgamas_dyst_correct,
    command = correct_lasgamas_dytiscid(summed_abundance_spp),
  ),

  tar_target(
    name = abundance_no_zero,
    command = filter_zero_abd(summed_abundance_lasgamas_dyst_correct),
  ),

  tar_target(
    name = spp_abundances_wide,
    command = spread_present_species(summed_abundance_lasgamas_dyst_correct),
  ),

  tar_target(
    name = synonymous_names,
    command = identify_merge_duplicates(traits, summed_abundance_lasgamas_dyst_correct),
  ),

  # note: depends on brom_validated
  tar_target(
    name = abundance_no_81,
    command = abundance_no_zero |>
      mutate(across(ends_with("_id"), readr::parse_integer)) |>
      semi_join(brom_validated |>
                  filter(visit_id!=81))),

  ## SECTION 6: Bromeliad morphology derived variables ------------------------

  tar_target(
    name = diam_brom,
    command = make_diam_brom(brom_validated),
  ),

  tar_target(
    name = fpom_brom,
    command = make_fpom_brom(brom_validated),
  ),

  tar_target(
    name = detritus_wide,
    command = make_detritus_wide(brom_validated),
  ),

  tar_target(
    name = detritus_wider,
    command = make_detritus_wider(
      brom_validated, detritus_wide,
      visitnames, diam_brom, fpom_brom),
  ),

  ## SECTION 7: Detritus cleaning & corrections -------------------------------

  tar_target(
    name = detritus_wider_bromeliad_names,
    command = fix_whitespace_bromeliad_names(detritus_wider),
  ),

  tar_target(
    name = detritus_wider_cardoso_corrected,
    command = correct_cardoso_detritus_wider(detritus_wider_bromeliad_names),
  ),

  tar_target(
    name = detritus_wider_correct_frenchguiana,
    command = correct_frenchguiana_detritus(detritus_wider_cardoso_corrected),
  ),

  tar_target(
    name = detritus_wider_correct_brazil,
    command = correct_picin_juraea(detritus_wider_correct_frenchguiana),
  ),

  ## SECTION 8: Bromeliad environmental variables ----------------------------
  ## (corrections and lookups, no imputation)

  tar_target(
    name = bromeliad_detritus_extra_cols,
    command = add_in_extra_columns(detritus_wide, detritus_wider, detritus_wider_correct_brazil),
  ),

  tar_target(
    name = bromeliad_detritus_incidentrad,
    command = correct_incident_rad_percent(bromeliad_detritus_extra_cols),
  ),

  tar_target(
    name = openness_conversion_table,
    command = create_openness_conversion_table(),
  ),

  tar_target(
    name = bromeliad_detritus_open,
    command = left_join(bromeliad_detritus_incidentrad,
                        openness_conversion_table |>
                          mutate(visit_id = readr::parse_integer(visit_id))),
  ),

  tar_target(
    name = bromeliad_detritus_open_converted,
    command = convert_incident_to_openness(bromeliad_detritus_open),
  ),

  tar_target(
    name = bromeliad_elevation,
    command = add_elevation(bromeliad_detritus_open_converted),
  ),

  ## SECTION 9: Bromeliad species names --------------------------------------

  tar_target(
    name = genus_spp_corrected,
    command = extract_bromeliad_species_names(bromeliad_elevation),
  ),

  tar_target(
    name = correct_name_pairing,
    command = join_old_new_bromeliad_names(
      genus_spp_corrected,
      bromeliad_names),
  ),

  tar_target(
    name = bromeliad_correctnames,
    command = correct_bromelaid_species_names(
      bromeliad_elevation,
      correct_name_pairing),
  ),

  tar_target(
    name = bromeliads_visit_no_81,
    command = filter_bromeliads_visit81(bromeliad_correctnames, visit_no_81),
  ),

  ## SECTION 10: Species dictionary ------------------------------------------

  tar_target(
    name = spp_dictonary,
    command = output_spp_dictionary(
      abundance |>
        unnest(measurements) |>
        mutate(bromeliad_id = as.numeric(bromeliad_id),
               species_id = as.numeric(species_id) ),
      brom_validated, visits, trts_all_filtered),
  ),

  ## =========================================================================
  ## Final output: observed (non-imputed) data
  ## =========================================================================

  tar_target(
    name = observed_data,
    command = list(
      datasets          = dats,
      visits            = visit_no_81,
      traits            = traits,
      bromeliads        = bromeliads_visit_no_81,
      abundance         = abundance_no_81,
      synonymous_names  = synonymous_names,
      abundance_matrix  = spp_abundances_wide,
      ## elements needed downstream by the imputation pipeline
      brom_validated    = brom_validated,
      detritus_corrected = detritus_wider_correct_brazil
    )
  )

)
