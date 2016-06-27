library(dplyr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

Tmatrix <- read.csv("data-raw/13_T_matrix.csv")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")

source("R/output_cleaning_functions.R")



# Filtering Rows and columns -- Ematrix -----------------------------------

Ematrix_row_col <- Ematrix %>%
  drop_row_col(c("detritus_sum", "max_water"), all_NA)

# E matrix -- List acceptable bromelid_ids --------------------------------


brom_names_list <- c("detritus_sum", "max_water") %>%
  set_names() %>%
  map(get_bromid_nonNA, data_matrix = ematrix_row_col)


Brom_names_vol_det <- ematrix_row_col %>%
  filter(complete.cases(.)) %>%
  .[['bromeliad_id']]

output_list <- splice(brom_names_list, complete = Brom_names_vol_det)

### Trait filtering

# Filter out terrestrial and micro

T_aquatic <- Tmatrix %>%
  filter(realm != "terrestrial", micro_macro != "micro")

trait_names <- names(T_aquatic)[grepl("^[A-Z]{2}\\d", names(T_aquatic))]
Tmatrix_row_col <- T_aquatic %>%
  drop_row_col(trait_names, any_NA_or_0)

## symm for report


## W filtering ----------------------

brom_names <- names(Wmatrix)[grepl("^X", names(Wmatrix))]
Wmatrix_row_col <- Wmatrix %>%
  drop_row_col(brom_names, any_NA_or_0)


full_list <- list(E = Ematrix_row_col,
     W = Wmatrix_row_col,
     B = Tmatrix_row_col,
     filter_environments = output_list)

saveRDS(full_list, "data-raw/81_WBE.RDS")




