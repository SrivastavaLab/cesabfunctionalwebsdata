## MERGING the data together!

## er, should it be a list, or a CSV?


# read all data in --------------------------------------------------------
require(readr)
datasets <- read_csv("data-raw/01_datasets.csv")
visits <-   read_csv("data-raw/01_visits.csv")
traits <-   read_csv("data-raw/07_traits.csv")
bromeliads <- read_csv("data-raw/61_bromeliad_wide.csv")
abundance <- read_csv("data-raw/02_abundance.csv")

all_data_list <- list(
  datasets   = datasets,
  visits     = visits,
  traits     = traits,
  bromeliads = bromeliads,
  abundance  = abundance)

saveRDS(all_data_list, "releases/all_data.rds")
