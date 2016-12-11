

optional_cols <- reduce(list(names(detritus_wide),
                             names(detritus_wider),
                             names(bromeliad_detritus)), setdiff)


bromeliad_optional_info <- distinct(detritus_wide[c("bromeliad_id", optional_cols)])
# TODO check these rows

bromeliad_detritus_opts <- left_join(bromeliad_detritus, bromeliad_optional_info)



# investigating weird spot changes ----------------------------------------


#  Apparently we first change this particular, single bromeliad.. but why?

# apparently we are supposed to turn its value of canopy.openess[281]<-"open"
bromeliad_detritus_opts %>% filter(bromeliad_id == "2431")

#  what's around there?
target_brom <- which(bromeliad_detritus_opts$bromeliad_id == "2431")

bromeliad_detritus_opts %>%
  slice((target_brom - 5):(target_brom + 5)) %>%
  select(bromeliad_id, dataset_name, dplyr::contains("canopy"))

# Argentina data

# decimal in the wrong place....

# bromeliad_wide$incident_radiation_percentage[517]<-14.59

bromeliad_detritus_opts$incident_radiation_percentage %>% max(na.rm = TRUE)

bromeliad_detritus_opts %>%
  mutate(incident_radiation_percentage = if_else(bromeliad_id == "3846" & incident_radiation_percentage > 0,
                                                 true = incident_radiation_percentage / 10,
                                                 false = incident_radiation_percentage))

convert_words_binary <- frame_data(
  ~canopy_openess_chr, ~open.canopy,
  "open",              1,
  "closed",            0,
  "edge",              0
)

convert_visitid <- bind_rows(data_frame(visit_id = c("331", "311", "231"),       open.canopy = 1),
                             data_frame(visit_id = c("326", "316", "306", "21"), open.canopy = 0))

new_openness_values <- expand.grid(visit_id = c("281","266","271"),
                                   canopy_openess_chr = c("open", "closed", "edge"),
                                   stringsAsFactors = FALSE) %>%
  left_join(convert_words_binary) %>%
  bind_rows(convert_visitid) %>%
  arrange(open.canopy)

bromeliad_detritus_opts %>%
  left_join(new_openness_values)




