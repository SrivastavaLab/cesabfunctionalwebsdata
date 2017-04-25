# 25 April 2017
# Andrew MacDonald
#  Merging duplicates


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
  .summed_abundance_lasgamas_dyst_correct %>%
    filter(abd > 0) %>%
    # choose from traits those rows that represent synonyms, and take only the
    # useful columns to join with abundance. Use inner_join because this trait
    # table could contain other species not in abundance (e.g. drought experiment
    # animals)
    inner_join(.traits %>%
                 semi_join(synonymous_names) %>%
                 select(species_id, names, bwg_name, taxon_name)) %>%
    group_by(dataset_id, taxon_name, bromeliad_id) %>% nest %>%
    mutate(ndups = map_dbl(data, nrow)) %>% arrange(desc(ndups)) %>%
    verify(ndups == 1)
  # never happens

  return(synonymous_names)
}
