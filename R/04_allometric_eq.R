#The script will fill in missing values for max volume and total detritus in BWGdb data
#D. Srivastava, F. Ospina Bautista, G. Romero, I. Barberis
#================================




#===restart

require(dplyr)
require(readr)
require(tidyr)

#dianes local link: setwd("C:/Users/Diane/Dropbox/BWG BromeliadCommunityAnalysis/CESAB trait working group/cesabfunctionalwebsdata")
broms <- read_csv("data-raw/01_broms.csv")
## the "problems" are probaby OK?
problems(broms)
broms$ph
## yes --- they look just fine

detritus_wide <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  unite(min_max, min, max) %>%
  mutate(min_max = paste0("detritus", min_max)) %>%
  spread(min_max, mass)

==above from script 02 : remove when finished====

detritus_wider<-broms%>%select(visit_id, bromeliad_id)%>%
  group_by(bromeliad_id)%>%
  summarize(visit=first(visit_id))%>%
  left_join(detritus_wide)

detrital_table<-detritus_wider%>%group_by(visit)%>%summarise_each(funs(mean))#useful table to see what detritus each visit

detritus_wider$detritus0_150[detritus_wider$visit==21]<-exp(0.68961*log(detritus_wider$detritus150_20000[detritus_wider$visit==21])-0.11363)#cardoso2008
#cardoso2008 is visit_id=21, dataset_id="6", name is "Cardoso2008"  




