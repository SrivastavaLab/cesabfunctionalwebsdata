library(dplyr)

bromeliad_wide <- read.csv("data-raw/02_bromeliad_wide.csv", stringsAsFactors = FALSE)
detritus_final <- read.csv("data-raw/22_detritus_summary.csv", stringsAsFactors = FALSE)
volume_final <- read.csv("data-raw/24_volume.csv", stringsAsFactors = FALSE)
open_final <- read.csv("data-raw/23_open.csv", stringsAsFactors = FALSE)

brom_vol <- left_join(bromeliad_wide, volume_final, by = c("bromeliad_id"))

det <- detritus_final %>%
  select(-detritus_range)

brom_det_vol <- left_join(brom_vol, det, by = c("bromeliad_id"))

brom_det_vol_open <- left_join(brom_det_vol, open_final, by = c("bromeliad_id"))

write.csv(brom_det_vol_open, "data-raw/29_full_bromeliad_wide.csv")
