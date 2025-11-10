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
    name = fuzzy_traits,
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
