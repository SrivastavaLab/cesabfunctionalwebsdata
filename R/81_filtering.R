library(dplyr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

T_aquatic <- readRDS("data-raw/14_B_matrix.RDS")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")

source("R/output_cleaning_functions.R")



# Filtering Rows and columns -- Ematrix -----------------------------------

Ematrix_row_col <- Ematrix %>%
  drop_row_col(c("detritus_sum", "max_water", "open.canopy"), all_NA)

# E matrix -- List acceptable bromelid_ids --------------------------------


brom_names_list <- c("detritus_sum", "max_water", "open.canopy") %>%
  set_names() %>%
  map(get_bromid_nonNA, data_matrix = Ematrix_row_col)


Brom_names_vol_det <- Ematrix_row_col %>%
  filter(complete.cases(.)) %>%
  .[['bromeliad_id']]

output_list <- splice(brom_names_list, complete = Brom_names_vol_det)

### Trait filtering

# Filter out terrestrial and micro

Tmatrix_row_col <- T_aquatic %>%
  drop_row_col(names(T_aquatic), any_NA_or_0)

## symm for report


## W filtering ----------------------

brom_names <- names(Wmatrix)[grepl("^X", names(Wmatrix))]
# Wmatrix_row_col <- Wmatrix %>%
#   drop_row_col(brom_names, any_NA_or_0)

rownames(Wmatrix) <- Wmatrix$brm

Wmatrix_correct <- Wmatrix %>% select(-brm)

## format E
row.names(Ematrix_row_col) <- Ematrix_row_col$bromeliad_id
Ematrix_correct <- Ematrix_row_col %>%
  select(-visit_id, -bromeliad_id)

## format W
head(Wmatrix_correct)

rownames(Tmatrix_row_col) <- paste0("X", rownames(Tmatrix_row_col))

head(Tmatrix_row_col)

## We need the strata
setdiff(rownames(Tmatrix_row_col), colnames(Wmatrix_correct))

setdiff(colnames(Wmatrix_correct), rownames(Tmatrix_row_col))

full_list <- list(E = Ematrix_row_col,
     W = Wmatrix_correct,
     B = Tmatrix_row_col,
     filter_environments = output_list)

saveRDS(full_list, "data-raw/81_WBE.RDS")




