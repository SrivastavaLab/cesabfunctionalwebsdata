
### output -- a matrix with traits named in the proper way (XXN, where X is a
### letter and N a number) and in which all traits are unique

rename_check_traits <- function(.trts_all_filtered){
  ## in order to find out where the traits between the different columsn are
  ## similar, we spread the two categories ("traits" and "tachet").We then stick
  ## them together.
  traits_all_combined <- .trts_all_filtered %>%
    gather(trtname, value, starts_with("tachet"), starts_with("traits")) %>%
    separate(trtname, into = c("tach_trait", "traitmode")) %>%
    spread(tach_trait, value) %>%
    replace_na(list(tachet = "", traits = "")) %>%
    mutate(combotrait = paste0(tachet, traits))

  ## check for problems -- if there is more than one character in the traits (all
  ## shold be single digits) then this indicates that the two tables had different
  ## values
  traits_all_combined %>%
    filter(nchar(combotrait) > 1) %>%
    assertr::verify(nrow(.) == 0)

  ## if there are no problems -- convert the combined trait column to numeric,
  ## delete all, make traits columns.
  traits_all_combined %>%
    mutate(combotrait2 = as.numeric(combotrait)) %>%
    select(-tachet, -traits, -combotrait) %>%
    spread(traitmode, combotrait2) %>%
    assertr::verify(nrow(.) == nrow(rename_check_traits)) %>%
    # refining output by making "NA" into a true NA
    mutate_if(is.character, readr::parse_character)
}


# adding those new traits

join_check_by_tax <- function(Alltraits, newtraits, tax_grp){
  left_join(Alltraits, newtraits, by = tax_grp) %>%
    verify(nrow(.) == nrow(Alltraits))
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

put_traits_together(traits_all_MD_added, CP_genus, CP_family, "CP")
