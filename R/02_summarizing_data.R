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

visits<- read_csv("data-raw/01_visits.csv")
names(visits)
visitnames<-select(visits,visit_id,dataset_id,dataset_name)

datasets<- read_csv("data-raw/01_datasets.csv")
names(datasets)

vol_table<-broms%>%group_by(visit_id)%>%summarise(max_water=mean(max_water,na.rm=TRUE), species=first(species),extended_diameter=mean(extended_diameter,na.rm=TRUE), diameter=mean(diameter, na.rm=TRUE),
                                                  longest_leaf=mean(longest_leaf, na.rm=TRUE), num_leaf=mean(num_leaf, na.rm=TRUE), leaf_width=mean(leaf_width, na.rm=TRUE), plant_height_cm=mean(plant_height_cm, na.rm=TRUE))%>%
  left_join(visitnames)

##
diam_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  select(diameter, bromeliad_id)

fpom_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  select(fpom_ml, bromeliad_id)

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
  summarize(visit_id = first(visit_id)) %>%
  left_join(detritus_wide)%>%
  left_join(visitnames)%>%
  left_join(diam_brom)%>%
  left_join(fpom_brom)

detrital_table <- detritus_wider %>%
  select(-bromeliad_id) %>%
  group_by(visit_id) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))#useful table to see what detritus each visit

View(detrital_table)

## predicting the fine detritus from the rest of the data
## for cardoso 2008
fine_cardoso2008<- function(coarse){
  exp(0.68961 * log(coarse) - 0.11363)
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(visit_id == 21, fine_cardoso2008(detritus150_20000), detritus0_150))
  

## for saba datasetid=111 visits 451 121 126 116 detritus150_20000 detritus20000_NA
fine_saba2009 <- function(med){
  exp(0.79031 * log(med) - 0.070033)
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_1500 = ifelse(dataset_id == 111, fine_saba2009(detritus1500_20000), NA))

## Although Puerto Rico 2010 dataset=116 based only on relaxed diameter the adj r sq is 0.79
#eqn from all 1990s El verde plants (n=189, rsq = 0.78) interestingly v. similar eqn from pitilla 2002 secondary

elverde90s<-detritus_wider%>%filter(dataset_id==131| dataset_id==126|dataset_id==121|dataset_id==221)
elverde90s$detritus0_NA<-(elverde90s$detritus10_1500+elverde90s$detritus1500_20000+elverde90s$detritus20000_NA)
summary(glm(log(detritus0_NA)~log(diameter), data=elverde90s))
plot(log(elverde90s$detritus0_NA)~log(elverde90s$diameter))

total_elverde2010 <- function(dia){
  exp(-6.223+ 2.179* log(dia))
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_NA = ifelse(dataset_id == 116, total_elverde2010(diameter), NA))

#Dominica dataset=136, visit = 191, 196, 201 is just fine

#Macae detritus could not be estimated with rsq > 0.5

#argentina Las gamas datasets 166, 171, 181

fine_lasgamas<- function(med){
  ((0.9857 *med) + 1.496)
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id%in%c(166,171,181), fine_lasgamas(detritus150_850), detritus0_150))

#French Guiana
all_frenchguiana<- function(FPOM){
  (73.668*(FPOM - 298.09)/1000 + exp(0.8933* (73.668*FPOM - 298.09)/1000)) + 0.8056) 
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id%in%c(166,171,181), fine_lasgamas(detritus150_850), detritus0_150))


names(detritus_wider)
#(73.668FPOM - 298.09)/1000 + EXP(0.8933* (73.668FPOM - 298.09)/1000)) + 0.8056) 

#FPOM transforms from milliliters to grams


##easier!

#detritus_wider$detritus0_150[detritus_wider$visit==21]<-exp(0.68961*log(detritus_wider$detritus150_20000[detritus_wider$visit==21])-0.11363)#cardoso2008
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

