
### output -- a matrix with traits named in the proper way (XXN, where X is a
### letter and N a number) and in which all traits are unique

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
  # add a check for these -- that names are the same -- making this last part
  # safe.

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
