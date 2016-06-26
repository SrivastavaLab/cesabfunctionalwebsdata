#The script will fill in missing values for max volume and total detritus in BWGdb data
#D. Srivastava, F. Ospina Bautista, G. Romero, I. Barberis
#================================




#===restart

require(dplyr)
require(readr)
require(tidyr)
library(stringr)

#dianes local link: setwd("C:/Users/Diane/Dropbox/BWG BromeliadCommunityAnalysis/CESAB trait working group/cesabfunctionalwebsdata")
broms <- read_csv("data-raw/01_broms.csv")
is_number <- sapply(broms, function(x) any(str_detect(x, "[0123456789]"), na.rm = TRUE))
correct_cols <- paste(c("c", "n")[is_number +1], collapse = "")
broms<-read_csv("data-raw/01_broms.csv", col_types = correct_cols)
str(broms)

## the "problems" are probaby OK?
problems(broms)
broms$ph
## yes --- they look just fine

detritus_wide <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  unite(min_max, min, max) %>%
  mutate(min_max = paste0("detritus", min_max)) %>%
  spread(min_max, mass)

#==above from script 02 : remove when finished====

detritus_wider<-broms%>%select(visit_id, bromeliad_id)%>%
  group_by(bromeliad_id)%>%
  summarize(visit=first(visit_id))%>%
  left_join(detritus_wide)

detrital_table<-detritus_wider%>%group_by(visit)%>%summarise_each(funs(mean))#useful table to see what detritus each visit

detritus_wider$detritus0_150[detritus_wider$visit==21]<-exp(0.68961*log(detritus_wider$detritus150_20000[detritus_wider$visit==21])-0.11363)#cardoso2008
#cardoso2008 is visit_id=21, dataset_id="6", name is "Cardoso2008"

#vol
datasets<- read_csv("data-raw/01_datasets.csv")
names(datasets)
visits<- read_csv("data-raw/01_visits.csv")
names(visits)
visitnames<-select(visits,visit_id,dataset_name)
broms$plant_height_cm<-as.numeric(broms$plant_height_cm)
broms$num_leaf<-as.numeric(broms$num_leaf)
vol_table<-broms%>%group_by(visit_id)%>%summarise(max_water=mean(max_water,na.rm=TRUE), species=first(species),extended_diameter=mean(extended_diameter,na.rm=TRUE), diameter=mean(diameter, na.rm=TRUE),
                                                  longest_leaf=mean(longest_leaf, na.rm=TRUE), num_leaf=mean(num_leaf, na.rm=TRUE), leaf_width=mean(leaf_width, na.rm=TRUE), plant_height_cm=mean(plant_height_cm, na.rm=TRUE))%>%
  left_join(visitnames)

broms$num_leaf[broms$visit_id==111]
visits$meta[visits$visit_id==111]

write_csv(vol_table, "data-raw/vol_table.csv")

