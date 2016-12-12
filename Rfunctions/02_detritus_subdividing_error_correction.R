## correcting errors in detritus variables and subdividing data into useful columns


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
  nodet <- .broms %>%
    no_detritus_brom

  nodet %>%
    select(bromeliad_id, species, actual_water, max_water,
           longest_leaf, num_leaf, dead_leaves, height, diameter, extended_diameter,
           leaf_width, catchment_diameter, catchment_height_cm, catchment_diameter_cm,
           tank_height_cm, plant_area, plant_area_m2) %>%
    verify(nrow(.) == nrow(nodet))
}

## for each bromeliad select the fine detrius
make_fpom_brom <- function(.broms){
  .broms %>%
    # no_detritus_brom %>%
    select(bromeliad_id, fpom_ml, fpom_mg, fpom_g, cpom_g)
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
    verify(nrow(.) == nrow(.detritus_wide))

  fpom_distinct <- distinct(.fpom_brom)

  brom_just_ids <- .broms %>%
    select(visit_id, bromeliad_id) %>%
    distinct

  brom_just_ids %>%
    left_join(detritus_novisit_justdetritus) %>%
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

#' helper function for when you fill one column in with another. When that
#' happens, and if it happens correctly, the "source" column should be perfectly
#' nested in the "target" column -- ie if there is a value in "source" there
#' should never be a missing value in "target"
check_data_source_target <- function(dataset, sourcename, targetname) {
  testdata <- dataset %>%
    # where the 'source' data was..
    filter_(!is.na(sourcename)) %>%
    # there should be no empty 'target' data!
    filter_(is.na(targetname)) %>%
    verify(., nrow(.) == 0)
}


### could also do this not to specific sites BUT to whatever sites have fpom_g
### but not the other -- not detritus0_150
correct_frenchguiana_detritus <- function(.detritus_wider_cardoso_corrected){

  ## first correct the variable names -- move values from "fpom_g" "cpom_g" and
  ## "dead_leaves", within the one site where they were recorded erroneously,
  ## into correct "detritus*" columns
  move_values_to_detritus <- .detritus_wider_cardoso_corrected %>%
    mutate(detritus0_150 = ifelse(dataset_id=="211", fpom_g, detritus0_150))%>%
    mutate(detritus150_20000 = ifelse(dataset_id=="211", cpom_g, detritus150_20000))%>%
    mutate(detritus20000_NA= ifelse(dataset_id=="211", dead_leaves, detritus20000_NA))

  ### fix the mg error as well -- in PetitSaut2014 and Nouragues 2009
  fix_fpom_mp <- move_values_to_detritus %>%
    mutate(detritus0_150 = ifelse(dataset_id %in% c("206", "216"), (fpom_mg/1000), detritus0_150))

  ### remove these columns -- check first to confirm that no data is being discarded:


  # check target and source columns
  fix_fpom_mp %>%
    check_data_source_target("fpom_g", "detritus0_150") %>%
    check_data_source_target("cpom_g", "detritus150_20000") %>%
    check_data_source_target("dead_leaves", "detritus20000_NA") %>%
    check_data_source_target("fpom_mg", "detritus0_150")


  # if that did not throw and error, drop the old columns
  fix_fpom_mp %>%
    select(-fpom_g, -cpom_g, -dead_leaves, -fpom_mg) %>%
    ## Nourages 2006 (dataset 201) accidentally has particle counts in detritus categories
    #this needs to be corrected in BWGdb but for now
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

## finally, coerce the data types to be correct -- usually either numeric or character

# names(abundance)
# glimpse(abundance)
# OK

parse_column_types_reader <- function(df){
  df %>%
    # don't change id vars -- they may look like numbers but they are not!
    mutate_at(vars(-ends_with("_id")), parse_guess)
}


## IN WHICH we clean up the names of variables. some variables with rather
## unhelpful names (character encoding issues perhaps) are simply dropped --
## these are rare variables, which are recorded too infrequently to be missed.
## Some other, more usful variables are renamed to fix their bad names --
## spaces, confusing/duplicate spelling, etc
drop_bad_name <- function(.broms_rename_unnest){
  please_leave <- which(names(.broms_rename_unnest) %in% c("horizontal _leaves_percentage", "conductivity_?S_cm-1", "algae_?gC_per_L"))
  .broms_rename_unnest <- .broms_rename_unnest[-please_leave]

  names(.broms_rename_unnest) <- str_replace_all(names(.broms_rename_unnest), "canopy openess", "canopy_openess_chr")
  names(.broms_rename_unnest) <-str_replace_all(names(.broms_rename_unnest), "cpom_g ", "cpom_g")
  names(.broms_rename_unnest) <-str_replace_all(names(.broms_rename_unnest), "canopy cover", "canopy_cover")
  .broms_rename_unnest
}



# correct bromeliad names -------------------------------------------------

correct_bromeliad_names <- function(.detritus_wider, .bromeliad_spp){

  # WHIIIIITESPACE
  detritus_wider_clean <- .detritus_wider %>%
    mutate(species = species %>% str_trim(side = "both"))

  out <- detritus_wider_clean #%>%
    # left_join(.bromeliad_spp %>%
    #             select(species = `Former name`,
    #                    species_name = `Proposed name`))

  # out_nrow <- filter(out, is.na(species_name)) %>% nrow()
  #
  # # browser()
  # assert_that(out_nrow == 0)

# NO duplications of bromeliads!!
  out %>%
    group_by(bromeliad_id) %>%
    tally %>%
    assert(in_set(1), n)

  ## replace original "species" with new name
#
#   out <- out %>%
#     select(-species) %>%
#     rename(species = species_name)
#
  return(out)
}

