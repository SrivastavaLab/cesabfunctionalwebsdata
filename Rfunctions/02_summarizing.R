## summarizing repeated datasets

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


## remove detritus columns
no_detritus_brom <- function(x){
  x %>%
    select(-min, -max, -mass) %>%
    distinct
}

## For each bromeliad select the diameter
make_diam_brom <- function(.broms){
  .broms %>%
    no_detritus_brom %>%
    select(bromeliad_id, bromeliad_id, visit_id, actual_water, max_water,
           longest_leaf, num_leaf, height, diameter, extended_diameter,
           leaf_width, catchment_diameter, catchment_height_cm, catchment_diameter_cm,
           tank_height_cm, plant_area, plant_area_m2)
}

## for each bromeliad select the fine detrius
make_fpom_brom <- function(.broms){
  .broms %>%
    # no_detritus_brom %>%
    select(bromeliad_id, fpom_ml, fpom_mg, fpom_g, cpom_g, dead_leaves, num_leaf)
}



## here we make the different detritus categories into a wide format, one column
## for every unique pair of detritus ranges (min max)
make_detritus_wide <- function(.broms){
  detritus_wide <- .broms %>%
    select(bromeliad_id, min, max, mass) %>%
    unite(min_max, min, max) %>%
    mutate(min_max = paste0("detritus", min_max)) %>%
    spread(min_max, mass)


  ## function (defined above) removes detritus columns
  broms_without_detritus <- no_detritus_brom(.broms)

  bromeliad_wide <- detritus_wide %>%
    left_join(broms_without_detritus) %>%
    ## a simple assertr check to make sure that the number of rows does not duplicate.
    verify(nrow(.) == nrow(no_detritus_brom))

  bromeliad_wide
}


make_detritus_wider <- function(.broms, .detritus_wide, .visitnames, .diam_brom, .fpom_brom) {

  detritus_novisit <- .detritus_wide %>%
    # it is (or it had better be!) impossible for a bromeliad to be in more than
    # one visit. therefore this check should always pass
    select(-visit_id) %>%
    ## this should do nothing
    distinct %>%
    verify(nrow(.) == nrow(.detritus_wide))

  fpom_distinct <- distinct(.fpom_brom)

  brom_just_ids <- .broms %>%
    select(visit_id, bromeliad_id) %>%
    distinct

  brom_just_ids %>%
    # group_by(bromeliad_id) %>%
    # summarize(visit_id = first(visit_id)) %>%
    left_join(.visitnames, by = "visit_id") %>%
    left_join(.diam_brom, by = c("bromeliad_id")) %>%
    left_join(fpom_distinct, by = c("bromeliad_id")) %>%
    verify(nrow(.) == nrow(brom_just_ids))
}



#collapse to dataset level for easy checking
make_detrital_table <- function(.detritus_wider){
  .detritus_wider %>%
    select(-bromeliad_id, -dataset_name, dataset_id) %>%
    group_by(visit_id) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%
    left_join(datasetnames)
}


#first we realized that the larger detritus in cardoso has been input into the
#wrong column remove the next few lines if this gets fixed on BWGdb
correct_cardoso_detritus_wider <- function(.detritus_wider){
  .detritus_wider %>%
    mutate(detritus150_NA = ifelse(visit_id == 21,
                                   detritus150_20000,
                                   detritus150_NA),
           detritus150_20000 = ifelse(visit_id == 21,
                                      NA,
                                      detritus150_20000))

}


## finally, coerce the data types to be correct -- usually either numeric or character

# names(abundance)
# glimpse(abundance)
# OK




coerce_values <- function(.data, names_character, names_numeric){

  ## first confirm that there are no missing names.
  names_not_on_list <- setdiff(names(.data), c(names_character, names_numeric))

  msg <- sprintf("missing the names %s", paste(names_not_on_list, collapse = ", "))

  if(length(names_not_on_list) > 0) stop(msg)

  .data %>%
    mutate_each_(funs(as.numeric), names_numeric) %>%
    mutate_each_(funs(as.character), names_character)
}


coerce_values_abundance <- function(.abundance){


  coerce_values(.abundance,
                names_character = c("dataset_id", "species_id", "bwg_name",
                                    "category_range", "measurement", "bromeliad_id"),
                names_numeric = "abd")


}

coerce_values_trts_all_filtered <- function(.trts_all_filtered){
  coerce_values(.trts_all_filtered,
                names_character = c("species_id", "bwg_name", "domain", "kingdom", "phylum", "subphylum",
                                    "class", "subclass", "ord", "subord", "family", "subfamily",
                                    "tribe", "genus", "species", "subspecies", "functional_group",
                                    "predation", "realm", "micro_macro", "barcode", "names"),

                names_numeric = c("tachet.AS1", "tachet.AS2", "tachet.AS3", "tachet.AS4", "tachet.BS1",
                                  "tachet.BS2", "tachet.BS3", "tachet.BS4", "tachet.BS5", "tachet.DM1",
                                  "tachet.DM2", "tachet.FD1", "tachet.FD2", "tachet.FD3", "tachet.FD4",
                                  "tachet.FD5", "tachet.FD6", "tachet.FD7", "tachet.FD8", "tachet.FG1",
                                  "tachet.FG2", "tachet.FG3", "tachet.FG4", "tachet.FG5", "tachet.FG6",
                                  "tachet.RE1", "tachet.RE2", "tachet.RE3", "tachet.RE4", "tachet.RE5",
                                  "tachet.RE6", "tachet.RE7", "tachet.RE8", "tachet.RF1", "tachet.RF2",
                                  "tachet.RF3", "tachet.RF4", "tachet.LO1", "tachet.LO2", "tachet.LO3",
                                  "tachet.LO4", "tachet.LO5", "tachet.LO6", "tachet.LO7", "tachet.RM1",
                                  "tachet.RM2", "tachet.RM3", "tachet.RM4", "tachet.RM5", "tachet.MD1",
                                  "tachet.MD2", "tachet.MD3", "tachet.MD4", "tachet.MD5", "tachet.MD6",
                                  "tachet.MD7", "tachet.MD8", "traits.AS1", "traits.AS2", "traits.AS3",
                                  "traits.AS4", "traits.BS1", "traits.BS2", "traits.BS3", "traits.BS4",
                                  "traits.BS5", "traits.DM1", "traits.DM2", "traits.FD1", "traits.FD2",
                                  "traits.FD3", "traits.FD4", "traits.FD5", "traits.FD6", "traits.FD7",
                                  "traits.FD8", "traits.FG1", "traits.FG2", "traits.FG3", "traits.FG4",
                                  "traits.FG5", "traits.FG6", "traits.LO1", "traits.LO2", "traits.LO3",
                                  "traits.LO4", "traits.LO5", "traits.LO6", "traits.LO7", "traits.RE1",
                                  "traits.RE2", "traits.RE3", "traits.RE4", "traits.RE5", "traits.RE6",
                                  "traits.RE7", "traits.RE8", "traits.RF1", "traits.RF2", "traits.RF3",
                                  "traits.RF4", "traits.RM1", "traits.RM2", "traits.RM3", "traits.RM4",
                                  "traits.RM5"))
}

coerce_values_visits <- function(.visits){
  coerce_values(.visits,
                names_character = c("visit_id", "dataset_id", "habitat", "date", "collection_method",
                                    "meta", "dataset_name"),
                names_numeric = c("min_rainfall", "max_rainfall", "min_temperature", "max_temperature", "min_elevation",
                                  "max_elevation", "latitude", "longitude")
  )
}

coerce_visits_date <- function(.visits_values){
  .visits_values %>%
    mutate(date = ymd(date))
}

coerce_values_dats <- function(.dats){
  coerce_values(.dats,
                names_character =  c("dataset_id", "name", "country", "location","group_id", "user_id",
                                     "owner_name", "owner_email", "owner2_name",
                                     "owner2_email", "bwg_release", "public_release", "faculty_name",
                                     "faculty_email"),
                names_numeric = c("lat", "lng", "year",  "num_species", "num_visits", "num_bromeliads")
  )
}

coerce_dats_date <- function(.dats_values){
  .dats_values %>%
    mutate(bwg_release = ymd(bwg_release),
           public_release = ymd(public_release))
}


## WEIRD PART must drop a strange value with a bad name
drop_bad_name <- function(.broms_rename_unnest){
  please_leave <- which(names(.broms_rename_unnest) %in% c("horizontal _leaves_percentage", "conductivity_?S_cm-1", "algae_?gC_per_L"))
  .broms_rename_unnest <- .broms_rename_unnest[-please_leave]

  names(.broms_rename_unnest) <- str_replace_all(names(.broms_rename_unnest), "canopy openess", "canopy_openess_chr")
  names(.broms_rename_unnest) <-str_replace_all(names(.broms_rename_unnest), "cpom_g ", "cpom_g")
  .broms_rename_unnest
}


coerce_values_brom_r_u <- function(.brom_clean_name){
  names_chr <- c("bromeliad_id", "visit_id", "original_id", "species", "collection_date", "utme",
                 "utmn", "canopy_openess_chr",
                 "ant","host_tree_label", "substrate", "min", "max") ## note that min / max is here because these represent the extreme sizes of the sieve



  names_num <- c("actual_water", "spider",
                 "max_water", "longest_leaf", "num_leaf", "height", "diameter",
                 "extended_diameter", "leaf_width", "plant_height_cm", "average_ph",
                 "canopy_height_m", "elevation_m", "number_bromeliads_on_tree",
                 "number_of_bromeliads_in_3m_radius", "sampling_day", "dead_leaves", "green_leaves", "ammonium_umol_per_l",
                 "chlorophyll_a_ug_per_l", "dissolved_organic_carbon_mgc_per_l",
                 "inorganic_carbon_mgc_per_l", "nitrate_umol_per_l", "ph", "phosphate_umol_per_l",
                 "total_organic_carbon_mgC_per_l", "turbidity_ntu", "water_coloration_per_m",
                 "water_temperature_c", "air_temperature_c","horizontal_leaves_percentage", "incident_radiation_above_ground_percentage",
                 "number_of_epiphytes_in_ant_garden", "number_of_wells", "reservoir_diameter_cm",
                 "reservoir_height_cm", "vertical_leaves_percentage", "bacteria_per_ml",
                 "fpom_ml", "virus_per_ml", "bacterial_abundance_cell_per_ml",
                 "fpom_mg", "incident_radiation_percentage", "plant_area_m2",
                 "catchment_diameter_cm", "catchment_height_cm", "core_diameter_cm",
                 "light_canopy_access_0_10", "precipitation_access_0_10", "total_bromeliads_within_2m_radius",
                 "total_leaf_weight_g", "plant_area", "blade_width_cm", "cobertura_cm2",
                 "oxygen_mg_lt", "sheath_length_cm", "temperature_c",
                 "canopy_openess","no_brom_3m_cumulative",
                 "no_brom_host_tree","oxygen_mgperl", "canopy_height",
                 "elevation", "catchment_diameter", "total_bromeliads_on_same_tree",
                 "tank_height_cm","cpom_g", "fpom_g", "mass")


  coerce_values(.brom_clean_name,
                names_character = names_chr,
                names_numeric = names_num)

}



coerce_broms_date <- function(.broms_values){
  .broms_values %>%
    mutate(collection_date = ymd(collection_date))
}



