## summarizing a few of the bigger datasets
## Before summarizing, we modify the data with allometric equations.

require(dplyr)
require(readr)
require(tidyr)
library(stringr)

# bromeliads imporing with correct col types--------------------------------------------------------------

broms <- read_csv("data-raw/01_broms.csv")
is_number <- broms %>% sapply(function(x) any(str_detect(x, "[0123456789]"), na.rm = TRUE))
correct_cols <- paste(c("c", "n")[is_number +1], collapse = "")
broms<-read_csv("data-raw/01_broms.csv", col_types = correct_cols)
str(broms)

## the "problems" are probaby OK?
problems(broms)
broms$ph
## yes --- they look just fine

## here we make the different detritus categories into a wide format, one column
## for every unique pair of detritus ranges (min max)
detritus_wide <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  unite(min_max, min, max) %>%
  mutate(min_max = paste0("detritus", min_max)) %>%
  spread(min_max, mass)

## combine back with the visit ids
detritus_wider <- broms %>%
  select(visit_id, bromeliad_id) %>%
  group_by(bromeliad_id) %>%
  summarize(visit = first(visit_id)) %>%
  left_join(detritus_wide)


detrital_table <- detritus_wider %>%
  select(-bromeliad_id) %>%
  group_by(visit) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) #useful table to see what detritus each visit

View(detrital_table)

## predicting the fine detritus from the rest of the data
## for cardoso 2008
fine_predict <- function(coarse){
  exp(0.68961 * log(coarse) - 0.11363)
}

detritus_wider %>%
  mutate(detritus0_150_2 = ifelse(visit == 21, fine_predict(detritus150_20000), detritus0_150)) %>%
  select(detritus0_150, detritus0_150_2, detritus150_20000) %>%
  View

## for cardoso 2008
fine_predict <- function(coarse){
  exp(0.68961 * log(coarse) - 0.11363)
}

EXP (0.79031*ln(Detrital dry mass medium (g))-0.070033) + +"Detrital dry mass medium (g)"  +"Detrital dry mass coarse (g)" 

detritus_wider %>%
  mutate(detritus0_150_2 = ifelse(visit == 21, fine_predict(detritus150_20000), detritus0_150)) %>%
  select(detritus0_150, detritus0_150_2, detritus150_20000) %>%
  View



detritus_wider$detritus0_150[detritus_wider$visit==21]<-exp(0.68961*log(detritus_wider$detritus150_20000[detritus_wider$visit==21])-0.11363)#cardoso2008
#cardoso2008 is visit_id=21, dataset_id="6", name is "Cardoso2008"

### this script summarizes detritus amounts -- run it after you have imputed missing values
det <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  # semi_join(manys %>% filter(n > 4)) ## just to check with the eye.
  group_by(bromeliad_id) %>%
  summarise(detritus_mass_total = sum(mass, na.rm = TRUE))

summ_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  left_join(det)


write_csv(summ_brom, "data-raw/02_broms.csv")



# abundance ---------------------------------------------------------------

abund <- read_csv("data-raw/01_abundance.csv")

glimpse(abund)

summ_abund <- abund %>%
  select(-category_range, -measurement) %>%
  group_by(dataset_id, species_id, bwg_name, brm) %>%
  summarise(abundance = sum(abd, na.rm = TRUE))

write_csv(summ_abund, "data-raw/02_abundance.csv")

