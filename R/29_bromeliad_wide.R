library(dplyr)

bromeliad_wide <- read.csv("data-raw/02_broms.csv")
detritus_final <- read.csv("data-raw/22_detritus_summary.csv")
volume_final <- read.csv("data-raw/24_volume.csv")
open_final <- read.csv("data-raw/23_open.csv")

brom_vol <- left_join(bromeliad_wide, volume_final)

det <- detritus_final %>%
  select(-detritus_range)

brom_det_vol <- left_join(brom_vol, det)

brom_det_vol_open <- left_join(brom_det_vol, open_final)

write_csv(emat, "data-raw/29_full_bromeliad_wide.csv")
