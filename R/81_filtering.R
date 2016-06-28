library(dplyr)
require(purrr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

T_aquatic <- readRDS("data-raw/14_B_matrix.RDS")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")

source("R/output_cleaning_functions.R")

#' ## Filtering the Ematrix
#' Filtering the E matrix is not necesary, because `organize.syncsa()` will do that.

# Filtering Rows and columns -- Ematrix -----------------------------------

# Ematrix_row_col <- Ematrix %>%
#   drop_row_col(c("detritus_sum", "max_water", "open.canopy"), all_NA)
#

# the following lines were removed.
# setdiff(Ematrix, Ematrix_row_col)

# E matrix -- List acceptable bromelid_ids --------------------------------


brom_names_list <- c("detritus_sum", "max_water", "open.canopy") %>%
  set_names() %>%
  map(get_bromid_nonNA, data_matrix = Ematrix)


Brom_names_vol_det <- Ematrix %>%
  filter(complete.cases(.)) %>%
  .[['bromeliad_id']]

output_list <- splice(brom_names_list, complete = Brom_names_vol_det)

#' ## Trait filtering

# Filter out terrestrial and micro -- already done in anothr script

Tmatrix_row_col <- T_aquatic %>%
  drop_row_col(names(T_aquatic), any_NA_or_0)

#' here are the rows that were dropped:
#'
setdiff(T_aquatic, Tmatrix_row_col)

#' here is the name of that species
bad_rows <- setdiff(row.names(T_aquatic), row.names(Tmatrix_row_col))
bad_rows

T_aquatic[bad_rows, ]

#' how many are there?!
#'
length(bad_rows)

#' columns should be the same
ncol(T_aquatic) == ncol(Tmatrix_row_col)


## W filtering ----------------------

brom_names <- names(Wmatrix)[grepl("^X", names(Wmatrix))]
# Wmatrix_row_col <- Wmatrix %>%
#   drop_row_col(brom_names, any_NA_or_0)

rownames(Wmatrix) <- Wmatrix$brm

## remove bromeliads
Wmatrix_correct <- Wmatrix %>% select(-brm)

## NOW YOU FILTER out the bad columsn
bad_colnames <- paste0("X", bad_rows)

## all the bad names are in the Wmatrix
stopifnot(length(setdiff(bad_colnames, colnames(Wmatrix_correct))) == 0)

## the other names are Good
good_colname <- setdiff(colnames(Wmatrix_correct), bad_colnames)

## Have the Good
Wmatrix_only_good <- Wmatrix_correct[good_colname]


# E formatting ------------------------------------------------------------


row.names(Ematrix) <- Ematrix$bromeliad_id
Ematrix_correct <- Ematrix %>%
  select(-visit_id, -bromeliad_id)

## format W
head(Wmatrix_only_good)

## make Tmatrix rows look just like Wmatrix columns by adding X
rownames(Tmatrix_row_col) <- paste0("X", rownames(Tmatrix_row_col))

head(Tmatrix_row_col)

## there should be no rows of the traits matrix absent from the W matrix
assertthat::assert_that(identical(
  setdiff(colnames(Wmatrix_only_good),
          rownames(Tmatrix_row_col)),
  character(0L)
  ))


full_list <- list(
  E = Ematrix_correct,
  W = Wmatrix_only_good,
  B = Tmatrix_row_col,
  filter_environments = output_list)



# final assertions --------------------------------------------------------

library(assertthat)

## w columsn less than rows of b
assert_that(ncol(full_list$W) <= nrow(full_list$B))
## colnames of w are a strict subset of rownames of b
assert_that(identical(setdiff(colnames(full_list$W),
                              rownames(full_list$B)),
                      character(0)))

## nubmer of W rows less than number of E rows
assert_that(nrow(full_list$W) <= nrow(full_list$E))
## rownames of W are a strict subset of rownames of E
assert_that(identical(
  setdiff(rownames(full_list$W),
          rownames(full_list$E)),
  character(0L)))

in_W_not_E <- setdiff(rownames(full_list$W),
        rownames(full_list$E))

assert_that(identical(in_W_not_E, character(0)))



# strata ------------------------------------------------------------------

## this could become a funciton to work on arbitrary species pools
datasets <- names(full_list$W) %>%
  set_names %>%
  stringr::str_split("_") %>%
  map_chr(1)

strata <- datasets %>%
  unique %>%
  set_names(seq_along(.), .) %>%
  .[datasets] %>%
  set_names(names(full_list$W))


full_list <- splice(full_list, strata = strata)


saveRDS(full_list, "data-raw/81_WBE.RDS")




