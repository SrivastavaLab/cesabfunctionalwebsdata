## updating the workflow to targets
library(targets)
library(tarchetypes)
library(bwgdata)
tar_option_set(
  packages = c(
    "bwgdata",
    "dplyr",
    "bwgdata",
    "purrr",
    "osfr")
  )

# read in functions -- only needed for `get_all_abundance`s
tar_source(files = "Rfunctions/")

list(
  ## Database downloads
  tar_target(
    name = dats,
    command = bwg_get("datasets")
  ),
  tar_target(
    name = visits,
    command = bwg_get("visits")
  ),
    # Once when this function stopped working because of a bug in the API,
    # I switched to this approach. Good to keep as a backup:
  # tar_target(
  #   name = broms_list,
  #   command = get_all_bromeliads(visits)
  # ),
  # tar_target(
  #   name = broms,
  #   command = all_bromeliads_list_to_df(broms_list)
  # ),
  tar_target(
    name = broms,
    command = bwg_get("bromeliads")
  ),
  tar_target(
    name = abds,
    command = get_all_abundances(dats)
  ),
  tar_target(
    name = trts_all,
    command = bwg_get("species",
                      list(tachet = "true",
                           traits = "true"))
  ),
  ## download additional files
  tar_target(
    name = trait_spreadsheet,
    command = get_osf_spreadsheet(ref = "uws6t"),
    format = "file"
  ),
  tar_target(
    name = FPOMdecanted_dryweight,
    command = get_osf_spreadsheet(ref = "pu4eh"),
    format = "file"
  ),
  tar_target(
    name = volume_estimated,
    command = get_osf_spreadsheet(ref = "v32yk"),
    format = "file"
  )
)
