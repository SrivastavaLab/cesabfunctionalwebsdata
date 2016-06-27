library(dplyr)
library(readr)

detritus_final <- read.csv("data-raw/22_detritus_summary.csv")
volume_final <- read.csv("data-raw/24_volume.csv")
open_final <- read.csv("data-raw/23_open.csv")


det <- detritus_final %>%
  select(-detritus_range)

e_det_vol <- left_join(det, volume_final)

emat <- left_join(e_det_vol, open_final)

write_csv(emat, "data-raw/41_E_matrix.csv")
