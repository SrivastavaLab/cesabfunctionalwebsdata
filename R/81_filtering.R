library(dplyr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

Tmatrix <- read.csv("data-raw/13_T_matrix.csv")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")


# Filter out NAs

any_NA <- function(vector){
  any(is.na(vector))
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

## is detritus negative.

sum(E_complete$detritus_sum < 0, na.rm = TRUE)


names(E_all_NA) <-

E_final <- E_all_NA %>%
  filter(!all_NA) %>%
  select(-all_NA)





### Trait filtering

# Filter out terrestrial and micro

T_aquatic <- Tmatrix %>%
  filter(realm != "terrestrial", micro_macro != "micro")






Wmatrix %>%
  apply(1, sum) %>%

  apply(1, filter_if_all_NA) %>%
  cbind(Wmatrix) %>%







