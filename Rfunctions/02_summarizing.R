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
  select(bromeliad_id, diameter)
}

## for each bromeliad select the fine detrius
make_fpom_brom <- function(.broms){
  .broms %>%
    no_detritus_brom %>%
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

  .broms %>%
    select(visit_id, bromeliad_id) %>%
    distinct %>%
    # group_by(bromeliad_id) %>%
    # summarize(visit_id = first(visit_id)) %>%
    left_join(.detritus_wide) %>%
    left_join(.visitnames) %>%
    left_join(.diam_brom) %>%
    left_join(.fpom_brom) %>%
    verify(nrow(.) == nrow(.broms))
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
