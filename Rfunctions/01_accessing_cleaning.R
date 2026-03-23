# accessing -----------------------------------------------------

## Getting the abundance matrix for each site requires querying its precise dataset ID
## this convenience function creates a named vector of all the datasets and downloads them all
## can be slow

# get_all_abundances <- function(.dats){
#
#   ## might need to replace this with a suitable structure
#   bwg_get_safe <- possibly(bwg_get, NA)
#
#   abds_ <- .dats %>%
#     .[["dataset_id"]] %>%
#     as.numeric %>%
#     set_names %>%
#     map(~ list(dataset_id = .x)) %>%
#     map(~ bwg_get_safe("matrix", .))
#
#   return(abds_)
# }



# Filtering ---------------------------------------------------------------

## NOTE eventualy these will be deleted and then this test will be deleted
## because it will be useless
filter_test <- function(x){
  x %>%
    filter(!grepl("^Test", name))
}


## pesky test datasets, and other garbage potentially, goes on a blacklist.

make_blacklist <- function(.visits, .dats){
  criteria <- .visits %>%
    left_join(.dats, by = "dataset_id") %>%
    ## count the sins here
    filter(str_detect(name, "Test"))

  criteria %>%
    select(visit_id, dataset_id)
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


make_bwg_schema <- function(){
  bwg_schema_bromeliads <- list(
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
  canopy_height_m                            = "integer",
  canopy_height                              = "integer",    # duplicate of canopy_height_m?
  canopy_openess_chr                         = "character",
  canopy_openess                             = "double",     # all-NULL in sample — verify
  canopy_cover                               = "double",     # all-NULL in sample — verify
  `Canopy openess`                           = "character",  # duplicate — raise with API dev
  Canopy                                     = "integer",    # unclear — raise with API dev
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



  # Restructuring -------------------------------------------------------------


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
  if (length(list_cols) > 0) {
    message(glue::glue(
      "[{df_name}] Flattening list columns: {paste(list_cols, collapse = ', ')}"
    ))
    df <- df %>%
      mutate(across(all_of(list_cols),
                ~ map(., ~ if (is.null(.x)) NA_real_ else as.numeric(.x)) %>%
                  unlist()))
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

combine_multi_names <- function(.trts_all){
  ### names are a nested list. Convert them to a vector of length one.
  ## bespoke predicate function to detect multiple names
  is_long <- function(x) length(x) > 1

  .trts_all %>%
    mutate(names = names %>%
             ## again, supply a sensible default (an empty word, not a NULL)
             map_if(is_null, ~"") %>%
             map_if(is_long, paste, collapse = ";")) %>%
    ## unnesst safely, since now it is a wholesome list of length 1 dataframes
    unnest(names)

}



# import corrected names --------------------------------------------------

import_BromeliadSpecies <- function(path){
  read_delim(path,
             ";", escape_double = FALSE, trim_ws = TRUE)
}

