

optional_cols <- reduce(list(names(detritus_wide),
                             names(detritus_wider),
                             names(bromeliad_detritus)), setdiff)




#  Apparently we first change this particular, single bromeliad.. but why?

bromeliad_detritus %>% filter(bromeliad_id == "2431")

#  what's around there?
target_brom <- which(bromeliad_detritus$bromeliad_id == "2431")

bromeliad_detritus %>% slice((target_brom - 5):(target_brom + 5))

# Argentina data

