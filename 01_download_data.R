## updating the workflow to targets
library(targets)
library(tarchetypes)
library(bwgdata)
tar_option_set(
  packages = c(
    "bwgdata",
    "dplyr",
    "bwgdata",
    "purrr")
  )

# read in functions -- only needed for `get_all_abundance`s
tar_source(files = "Rfunctions/")

list(
  tar_target(
    name = dats,
    command = bwg_get("datasets")
  ),
  tar_target(
    name = visits,
    command = bwg_get("datasets")
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
  )
)
