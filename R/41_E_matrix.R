library(dplyr)
library(readr)

detritus_final <- read.csv("data-raw/22_detritus_summary.csv")
volume_final <- read.csv("data-raw/31_broms.csv")

vol <- volume_final %>%
  select(bromeliad_id, max_water)

det <- detritus_final %>%
  select(-detritus_range)

emat <- left_join(det, vol)

write_csv(emat, "data-raw/41_E_matrix.csv")
