create_data_release <- function(filename, datasets, visits, traits,
                                bromeliads, abundance,
                                synonymous_names, abundance_matrix){

  all_data_list <- list(
    datasets   = datasets,
    visits     = visits,
    traits     = traits,
    bromeliads = bromeliads,
    abundance  = abundance,
    synonymous_names = synonymous_names,
    abundance_matrix = abundance_matrix)

  saveRDS(all_data_list, filename)
}
