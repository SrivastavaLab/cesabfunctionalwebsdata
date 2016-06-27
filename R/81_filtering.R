library(dplyr)

Wmatrix <- read.csv("data-raw/51_W_matrix.csv")

Tmatrix <- read.csv("data-raw/13_T_matrix.csv")

Ematrix <- read.csv("data-raw/41_E_matrix.csv")


# Filter out NAs

filter_if_any_NA <- function(vector){
  if(any(is.na(vector))){
    TRUE
  } else {FALSE}
}

filter_if_all_NA <- function(vector){
  if(all(is.na(vector))){
    TRUE
  } else {FALSE}
}


### Environmental filtering

E_all_NA <- Ematrix %>%
  apply(1, filter_if_all_NA) %>%
  cbind(Ematrix)

names(E_all_NA) <- c('all_NA', 'visit_id', 'bromeliad_id', "detritus_sum", 'max_water')

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







