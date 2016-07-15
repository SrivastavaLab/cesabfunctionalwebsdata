## summarizing a few of the bigger datasets
## Before summarizing, we modify the data with allometric equations.

require(dplyr)
require(readr)
require(tidyr)
library(stringr)
library(assertr)
## convenience function for reading
source("R/reading_functions.R")

# bromeliads:  imporing with correct col types--------------------------------------------------------------

broms <- read.csv("data-raw/01_broms.csv", stringsAsFactors = FALSE)

visits <- read_csv("data-raw/01_visits.csv", col_types = "nncDnnnnnncnncc")
datasets <- read_csv("data-raw/01_datasets.csv")

## making some summary tables to help with visualizing missing data -----------------------------------------


visitnames <- visits %>%
  select(visit_id, dataset_id, dataset_name)

datasetnames <- visitnames %>%
  select(dataset_id, dataset_name) %>%
  distinct()


## generate bromeliad-level data for each visit and each species

vol_table <- broms %>%
  group_by(visit_id, species) %>%
  summarise(max_water         = mean(max_water,na.rm=TRUE),
            extended_diameter = mean(extended_diameter,na.rm=TRUE),
            diameter          = mean(diameter, na.rm=TRUE),
            longest_leaf      = mean(longest_leaf, na.rm=TRUE),
            num_leaf          = mean(num_leaf, na.rm=TRUE),
            leaf_width        = mean(leaf_width, na.rm=TRUE),
            plant_height_cm   = mean(plant_height_cm, na.rm=TRUE)) %>%
  left_join(visitnames)

## For each bromeliad select the diameter
diam_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  select(bromeliad_id, diameter)

## for each bromeliad select the fine detrius
fpom_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct %>%
  select(bromeliad_id, fpom_ml, fpom_mg, fpom_g, cpom_g, dead_leaves, num_leaf)


## here we make the different detritus categories into a wide format, one column
## for every unique pair of detritus ranges (min max)
detritus_wide <- broms %>%
  select(bromeliad_id, min, max, mass) %>%
  unite(min_max, min, max) %>%
  mutate(min_max = paste0("detritus", min_max)) %>%
  spread(min_max, mass)

no_detritus_brom <- broms %>%
  select(-min, -max, -mass) %>%
  distinct

bromeliad_wide <- detritus_wide %>%
  left_join(no_detritus_brom) %>%
  verify(nrow(.) == nrow(no_detritus_brom))

write_csv(bromeliad_wide, "data-raw/02_bromeliad_wide.csv")


## combine back with the visit ids
detritus_wider <- broms %>%
  select(visit_id, bromeliad_id) %>%
  distinct %>%
  # group_by(bromeliad_id) %>%
  # summarize(visit_id = first(visit_id)) %>%
  left_join(detritus_wide)%>%
  left_join(visitnames)%>%
  left_join(diam_brom)%>%
  left_join(fpom_brom) #%>%
  # verify(nrow(.) == nrow(broms))

## create for references
detritus_original <- detritus_wider

#collapse to dataset level for easy checking
detrital_table <- detritus_wider %>%
  select(-bromeliad_id, -dataset_name, dataset_id) %>%
  group_by(visit_id) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  left_join(datasetnames)

### predicting the fine detritus from the rest of the data####################

## should be no copying over
where_values <- function(x) which(!is.na(x))


## for cardoso 2008
#first we realized that the larger detritus in cardoso has been input into the wrong column
#remove the next few lines if this gets fixed on BWGdb
detritus_wider <- detritus_wider %>%
  mutate(detritus150_NA = ifelse(visit_id == 21, detritus150_20000, detritus150_NA))
detritus_wider <- detritus_wider %>%
  mutate(detritus150_20000 = ifelse(visit_id == 21, NA, detritus150_20000))

fine_cardoso2008<- function(coarse){
  exp(0.68961 * log(coarse) - 0.11363)
}

detritus_wider <- detritus_wider %>%
  mutate(detritus0_150 = ifelse(visit_id == 21, fine_cardoso2008(detritus150_20000), detritus0_150))


## for saba datasetid=111 visits 451 121 126 116 detritus150_20000 detritus20000_NA
fine_saba2009 <- function(med){
  exp(0.79031 * log(med) - 0.070033)
}

detritus_wider<-detritus_wider %>%
  mutate(detritus0_1500 = ifelse(dataset_id == 111, fine_saba2009(detritus1500_20000), NA))

## Although Puerto Rico 2010 dataset=116 based only on relaxed diameter the adj r sq is 0.79
#eqn from all 1990s El verde plants (n=189, rsq = 0.78) interestingly v. similar eqn from pitilla 2002 secondary

elverde90s <- detritus_wider %>%
  filter(dataset_id==131| dataset_id==126|dataset_id==121|dataset_id==221)

elverde90s$detritus0_NA <- with(elverde90s, detritus10_1500 + detritus1500_20000 + detritus20000_NA)
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

#French Guiana only 186 (petit saut 2007) has fpom in ml, 211 (sinnamary 2011) has fpom cpom and deadleaves

#this allometric equation is from data held offline by regis
fpom_convert_ml_into_g<-function(FPOMml){(0.0013*(FPOMml)^2+0.0243*(FPOMml)+0.0369)}
fpom_convert_ml_into_g(7.6)

#now french guiana Sinnamary (dataset 211) generate detritus from detritus allometric equations and fns
#NOTE there is an input error: dead_leaves in Sinnamary was detritus>2cm x 2cm roughly, but in other sites #brown bromeliad leaves!

sinn<-detritus_wider%>%filter(dataset_id==211)
summary(glm(log(cpom_g)~log(fpom_g), data=sinn))#sinnamary based eqn has rsq of 0.64
plot(log(sinn$cpom_g)~log(sinn$fpom_g))
summary(glm(log(dead_leaves)~log(fpom_g), data=sinn))#sinnamary based eqn has rsq of 0.36
plot((sinn$dead_leaves)~(sinn$fpom_g))
sinn$detover150<-sinn$fpom_g+sinn$cpom_g+sinn$dead_leaves
summary(glm(log(detover150)~log(fpom_g), data=sinn))#sinnamary based eqn has rsq of 0.59
plot(log(sinn$detover150)~log(sinn$fpom_g))

over150_frenchguiana<- function(a){
  exp(0.688*(a)+3.075)
}

cpom_frenchguiana<- function(FPOMg){
  exp(0.858*log(FPOMg)+1.872)
}

largedet_frenchguiana<- function(FPOMg){
  exp(0.582*log(FPOMg)+2.5545)
}

#next move sinnamary detritus from "fpom/cpom/dead leaves into correct detrital categories:
#NOTE: this should be corrected on BWGdb as well

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id==211, fpom_g, detritus0_150))%>%
  mutate(detritus150_20000 = ifelse(dataset_id==211, cpom_g, detritus150_20000))%>%
  mutate(detritus20000_NA= ifelse(dataset_id==211, dead_leaves, detritus20000_NA))

#next apply sinnamary-based allometric eqns to dataset 186 (petit saut 2007) where we do the rather dangerous thing
#of converting fpom in ml to fpom in grams to predict detrital grams in another 2 categories
#we do the same thing for petit saut 2014(dataset 216), but here we at least start with fpom in mg

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id==186, fpom_convert_ml_into_g(fpom_ml), detritus0_150))%>%
  mutate(detritus150_20000 = ifelse(dataset_id==186, cpom_frenchguiana(detritus0_150), detritus150_20000))%>%
  mutate(detritus20000_NA= ifelse(dataset_id==186, largedet_frenchguiana(detritus0_150), detritus20000_NA))

detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id==216, (fpom_mg/1000), detritus0_150))%>%
  mutate(detritus150_20000 = ifelse(dataset_id==216, cpom_frenchguiana(detritus0_150), detritus150_20000))%>%
  mutate(detritus20000_NA= ifelse(dataset_id==216, largedet_frenchguiana(detritus0_150), detritus20000_NA))

#first, Nourages 2006 (dataset 201) accidentally has particle counts in detritus categories
#this needs to be corrected in BWGdb but for now:
#next, in dataset 201 need to convert fpom_ml to g then make it into detritus0_150,

detritus_wider<-detritus_wider %>%
  mutate(detritus30_150 = ifelse(dataset_id==201,NA, detritus30_150))%>%
  mutate(detritus0_30 = ifelse(dataset_id==201,NA, detritus0_30))%>%
  mutate(detritus150_300 = ifelse(dataset_id==201,NA, detritus150_300))
detritus_wider<-detritus_wider %>%
  mutate(detritus0_150 = ifelse(dataset_id==201,fpom_convert_ml_into_g(fpom_ml), detritus0_150))
detritus_wider<-detritus_wider%>%
 mutate(detritus150_NA = ifelse(dataset_id==201, over150_frenchguiana(detritus0_150), detritus150_NA))

#DIANE TO DO: chek after dataset updated: detritus_wider%>%filter(dataset_id%in%c(201, 206, 211, 216, 186))%>%View

#pitilla costa rica 200 all present
#pitilla costa rica 2002, 2010 are dataset61, 71; pitilla1997 dataset id is 51

fine_pitilla<- function(med, coarse){
  exp(0.79031 * log(med+coarse) - 0.07033)
}#R2= 0.8965
deadleaves_pitilla<- function(med,coarse){
  exp(1.01680 * log(med+coarse) - 1.09992)
}#R2= 0.776

detritus_wider<-detritus_wider%>%
  mutate(detritus0_150 = ifelse(dataset_id%in%c(51,61), fine_pitilla(detritus150_850, detritus850_20000), detritus0_150))
detritus_wider<-detritus_wider %>%
  mutate(detritus20000_NA = ifelse(dataset_id==51, deadleaves_pitilla(detritus150_850, detritus850_20000), detritus20000_NA))


detritus_wider<-detritus_wider %>%
  mutate(detritus20000_NA = ifelse(dataset_id==71, deadleaves_pitilla(0,detritus150_20000), detritus20000_NA))%>%
  mutate(detritus0_150 = ifelse(dataset_id==71, fine_pitilla(0,detritus150_20000), detritus0_150))


#pitilla 2004 dissection visit 66

pitilla2000s<-detritus_wider%>%filter(dataset_id==56)
pitilla2000s<-pitilla2000s%>%mutate(detritus0_NA = ifelse(visit_id==51, pitilla2000s$detritus0_150+pitilla2000s$detritus150_850+pitilla2000s$detritus20000_NA+pitilla2000s$detritus850_20000,
                                                          pitilla2000s$detritus0_150+pitilla2000s$detritus150_850+pitilla2000s$detritus20000_NA+pitilla2000s$detritus1500_20000+pitilla2000s$detritus850_1500))
summary(glm((detritus0_NA)~(diameter), family=gaussian, data=pitilla2000s)) #rsq=0.78
plot((pitilla2000s$detritus0_NA)~(pitilla2000s$diameter))

totaldet_pitilla<- function(dia){
  (0.7798 * dia - 24.147)
}
detritus_wider<-detritus_wider %>%
  mutate(detritus0_NA = ifelse(dataset_id == 66, totaldet_pitilla(diameter), detritus0_NA))

#Columbia Sisga Guasca datasets 76, 81, and Rio Blanco 2014 (dataset 91) base on pitilla
pitilla2000s$detritus150_NA<-pitilla2000s$detritus0_NA-pitilla2000s$detritus0_150
summary(glm((detritus0_150)~(detritus150_NA), family=gaussian, data=pitilla2000s))
plot((pitilla2000s$detritus0_150)~(pitilla2000s$detritus150_NA))

finealso_pitilla<- function(most){
  (0.407 *most - 0.36633)} #rsq=0.95

detritus_wider<-detritus_wider%>%
  mutate(detritus0_150 = ifelse(dataset_id%in%c(76,81, 91), finealso_pitilla(detritus150_NA), detritus0_150))

#Colombia RioBlanco2012 dataset86 base it on the 2014 rio blanco data
rioblanco<-detritus_wider%>%filter(dataset_id==91)
summary(glm((detritus150_NA)~(num_leaf), family=gaussian, data=rioblanco))
plot((rioblanco$detritus150_NA)~(rioblanco$num_leaf))
#the best model here had an rsquared of 0.52 so we decided to exclude the rio blanco 2012 dataset (86)

#honduras dataset 101 106 has detritus 22- 10000, we could estimate 20000 and greater and ignore the amount missed?
pitilla2000s$detritus0_20000<-pitilla2000s$detritus0_NA-pitilla2000s$detritus20000_NA
summary(glm((detritus20000_NA)~log(detritus0_20000), family=poisson, data=pitilla2000s))#rsq=0.67
plot((pitilla2000s$detritus20000_NA)~(pitilla2000s$detritus0_20000))

deadleavesalso_pitilla<- function(almost){
  exp(0.694 *log(almost) - 0.468)
}

detritus_wider<-detritus_wider%>%
  mutate(detritus10000_NA = ifelse(dataset_id%in%c(101,106), deadleavesalso_pitilla(detritus22_10000), NA))


### Cardoso 2011
vis_46 <- detritus_wider %>%
  filter(visit_id==46)

cardoso2011<-detritus_wider %>%
  filter(visit_id==231)

plot(log(vis_46$detritus0_150),log(vis_46$detritus150_850+vis_46$detritus850_1500))

# plot(log(cardoso2011$detritus0_150),log(cardoso2011$detritus150_NA))

summary(lm(log(detritus0_150)~log(vis_46$detritus150_850+vis_46$detritus850_1500),data=vis_46 ))

### assumption, we considered 150_850 + 850_1500 equivalent to 150_NA; sample size = 11

fine_cardoso2011<- function(coarse){
  exp(0.66648 * log(coarse) + 0.80186)
}

detritus_wider <- detritus_wider %>%
  mutate(detritus0_150 = ifelse(visit_id == 231, fine_cardoso2011(detritus150_NA), detritus0_150))

### Picinguaba2011 - detritus125_NA is actually total detritus0_NA. It must be corrected in the database.

P2011 <- detritus_wider %>%
  filter(visit_id==241)

correctedP2011<-P2011$detritus125_NA
correct125_NA<-rep(NA,20)
length(correctedP2011)
detritus_wider <- detritus_wider %>%
  mutate(detritus0_NA = ifelse(visit_id == 241, correctedP2011, detritus0_NA)) %>%
  mutate(detritus125_NA = ifelse(visit_id == 241, correct125_NA, detritus125_NA))


### Jureia2013 - detritus125_800 is total detritus0_NA. It must be corrected in the database.

J2013 <- detritus_wider %>%
  filter(visit_id==246)

correctedJ2013<-J2013$detritus125_800
correct125_800<-rep(NA,20)

detritus_wider <- detritus_wider %>%
  mutate(detritus0_NA = ifelse(visit_id == 246, correctedJ2013, detritus0_NA)) %>%
  mutate(detritus125_800 = ifelse(visit_id == 246, correct125_800, detritus125_800))


#### Serra do Japi
### we have detritus125_NA, need detritus0_125

Japi2013 <- detritus_wider %>%
  filter(visit_id==251|visit_id==346)
vis_46 <- detritus_wider %>%
  filter(visit_id==46)

plot(log(vis_46$detritus0_150),log(vis_46$detritus150_850+vis_46$detritus850_1500+vis_46$detritus1500_20000+vis_46$detritus20000_NA))
summary(lm(log(detritus0_150)~log(vis_46$detritus150_850+vis_46$detritus850_1500+vis_46$detritus1500_20000+vis_46$detritus20000_NA),data=vis_46 ))

fine_SJ2013<- function(coarse){
  exp(0.70297 * log(coarse) -0.13327)
}

### assumption, we considered 150_850 + 850_1500 + 1550-20000 + 20000_NA equivalent to 125_NA; sample size = 11

detritus_wider <- detritus_wider %>%
  mutate(detritus0_150 = ifelse(visit_id == 251|visit_id==346, fine_SJ2013(detritus125_NA), detritus0_150))

x<-c(2,3,4,5,NA,7)
y<-c(2,3,4,5,6,7)
z<-c(NA, NA, NA)

na.checker<-function(a){
  ifelse((mean(a, na.rm=TRUE)*length(a)-sum(a, na.rm=TRUE))>0,
         1,
         ifelse(mean(a,na.rm=TRUE)>0,
                2,
                NA_real_))
}

### Function identified missing data that was not necessarily correct.

na.checker(x)
na.checker(y)
na.checker(z)

visitdatanames<- visits %>%
  select(visit_id, dataset_name)

detrital_tableNA <- detritus_wider %>%
  select(-bromeliad_id, -dataset_name) %>%  #-bromeliad_id, -dataset_name
  group_by(visit_id) %>%
  #summarize(check = na.checker(detritus0_NA))%>%
  summarise_each(funs(nas = "na.checker"(.)))%>%
  left_join(visitdatanames)


#write.csv(detrital_tableNA, "data-intermediate/detrital_table_NA.csv")
# visualize with a daff ---------------------------------------------------


daff::render_diff(daff::diff_data(detritus_original, detritus_wider))

# write data out ----------------------------------------------------------

write_csv(detritus_wider, "data-raw/02_broms.csv")
