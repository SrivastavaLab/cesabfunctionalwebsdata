## MERGING the data together!

## er, should it be a list, or a CSV?


# read all data in --------------------------------------------------------
require(readr)
datasets <- read_csv("data-raw/01_datasets.csv")
visits <-   read_csv("data-raw/01_visits.csv")
traits <-   read_csv("data-raw/13_T_matrix.csv")
bromeliads <- read.csv("data-raw/29_full_bromeliad_wide.csv", stringsAsFactors = FALSE)
abundance <- read_csv("data-raw/51_abundance.csv")
wbe <- readRDS("data-raw/81_WBE.RDS")

all_data_list <- list(
  datasets   = datasets,
  visits     = visits,
  traits     = traits,
  bromeliads = bromeliads,
  abundance  = abundance,
  WBE        = wbe)

saveRDS(all_data_list, "~/Desktop/all_data.rds")
