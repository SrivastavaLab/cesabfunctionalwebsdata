create_data_release <- function(filename, datasets, visits, traits,
                                bromeliads, abundance,
                                synonymous_names, abundance_matrix){

  ## This retrieves the biomass data from "bwgbiomass" and adds it as a final
  ## data.frame in the list
  biomass <- datastorr::github_release_get(
    datastorr::github_release_info("SrivastavaLab/bwgbiomass", read = readRDS)
  )

  all_data_list <- list(
    datasets   = datasets,
    visits     = visits,
    traits     = traits,
    bromeliads = bromeliads,
    abundance  = abundance,
    synonymous_names = synonymous_names,
    abundance_matrix = abundance_matrix,
    biomass = biomass)

  saveRDS(all_data_list, filename)
}
