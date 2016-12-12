create_data_release <- function(filename, datasets, visits, traits, bromeliads, abundance){

  all_data_list <- list(
    datasets   = datasets,
    visits     = visits,
    traits     = traits,
    bromeliads = bromeliads,
    abundance  = abundance)

  saveRDS(all_data_list, filename)
}
