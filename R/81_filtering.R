library(dplyr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

Tmatrix <- read.csv("data-raw/13_T_matrix.csv")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")


# Filter out NAs

any_NA <- function(vector){
  any(any(is.na(vector)), sum(vector) == 0)
}

all_NA <- function(vector){
  all(is.na(vector))
}


### Environmental filtering

E_all_NA <- Ematrix %>%
  select(max_water, detritus_sum) %>%
  apply(1, all_NA)

E_complete <- Ematrix %>%
  .[!E_all_NA, ]

Brom_names_det <- Ematrix %>%
  filter(!is.na(detritus_sum)) %>%
  select(bromeliad_id) %>%
  .[['bromeliad_id']]

Brom_names_vol <- Ematrix %>%
  filter(!is.na(max_water)) %>%
  select(bromeliad_id) %>%
  .[['bromeliad_id']]

Brom_names_vol_det <- Ematrix %>%
  filter(complete.cases(.)) %>%
  select(bromeliad_id) %>%
  .[['bromeliad_id']]

### Trait filtering

# Filter out terrestrial and micro

T_aquatic <- Tmatrix %>%
  filter(realm != "terrestrial", micro_macro != "micro")

T_r_all_NA <- Tmatrix %>%
  select(matches("^[A-Z]{2}\\d")) %>%
  apply(1, all_NA)

## symm for report

T_r_no_na <- Tmatrix %>%
  .[!T_r_all_NA, ]

T_c_all_NA <- T_r_no_na %>%
  select(matches("^[A-Z]{2}\\d")) %>%
  apply(2, all_NA)

T_complete <- T_r_no_na %>%
  .[!T_c_all_NA, ]

## W filtering

Wmatrix %>% glimpse()
  apply(1, sum) %>%









