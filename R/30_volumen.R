# read the dataset
#setwd("~/Dropbox/CommunityAnalysis/CESAB trait working group/cesabfunctionalwebsdata")
require(dplyr)
require(readr)
require(tidyr)

# bromeliads --------------------------------------------------------------
broms <- read_csv("data-raw/01_broms.csv")
summ_broms<-read_csv("data-raw/02_broms.csv")
glimpse(summ_broms)

####French Guiana, We used the Regis's equation ####
#FrenchGuianaAechmea2007, Aechmea mertensii,Equation= exp(0.611+1.09*log(diameter))
volu<- function(a){exp(0.611+1.09*log(a))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 286, volu(diameter), max_water))%>%filter(visit_id==286)%>%View

#FrenchGuianaAechmea2008,PetitSaut, Aechmea mertensii, Vmax=exp(4.838+5.5888*log(diameter)-1.1649*(log(fg$diameter))^2+5.7181*log(fg$num_leaf)-1.7982*(log(fg$num_leaf)^2))
volu<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 301, volu(diameter,num_leaf), max_water))%>%filter(visit_id==301)%>%View

#FrenchGuianaAechmea2008,Kaw, Aechmea mertensii,Vmax= exp(0.0292 +0.339*log(fg$diameter) + 1.3563*log(fg$num_leaf))
volu<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 296, volu(diameter,num_leaf), max_water))%>%filter(visit_id==296)%>%View

#Nouragues2009,Sinnamary2011, Aechmea aquilega Vmax= exp(-0.4368 +0.735*log(fg1$diameter)+ 1.4260*log(fg1$num_leaf))
 volu<- function(a,b){exp(-0.4368 +0.735*log(a)+ 1.4260*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 336|visit_id==331, volu(diameter,num_leaf), max_water))%>%filter(visit_id==336|visit_id==331)%>%View

#FrenchGuianaVriesea2007, Vriesea splendens No information of diameter and number of leaf, information about actual water
#select data set only Vriesea splendens and create an equation
vri_only = filter(summ_broms, species=="Vriesea splendens"& visit_id==341)  
summary(lm(max_water~actual_water ,data=vri_only))

# Equation 
vol<- function(a,b){}
summ_broms%>%
  mutate(max_water= ifelse(visit_id==, vol(), max_water))%>%filter(visit_id == )%>%View

#### For the other sites ####

#how many species names are there for bromeliads
unique(summ_broms$species)
#select only Guzmania 
guz_only = filter(summ_broms, grepl("^Gu|^G\\.", species))
unique(guz_only$species)
glimpse(guz_only)
View(guz_only)

# Model for Honduras and Colombia2000, 2001 
plot(log(guz_only$num_leaf),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(actual_water +1),data=guz_only ))

# Equation for Honduras and Colombia2000 and 2001:Vmax=exp(0.78960 + 0.68576*log(Col$num_leaf) + 0.44664*log(Col$actual_water +1))
vol<- function(a,b){exp(0.78960 + 0.68576*log(a) + 0.44664*log(b+1))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 106|visit_id==111|visit_id==91|visit_id==96, vol(num_leaf,actual_water), max_water))%>%filter(visit_id == 106|visit_id==111|visit_id==91|visit_id==96)%>%View

#Model for Colombia.Rioblanco 2012 (366) and ElVerde 
summary(lm(log(max_water)~log(num_leaf),data=guz_only))

# equation for Colombia.Rioblanco 2012 (366) and ElVerde Vol.max=exp(-2.3418 + 2.1025*log(Col2012$num_leaf))
vol<- function(a){exp(-2.3418 + 2.1025*log(a))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id%in%c(131,146,151,156,161,166,171,176,181,351,356,361,366), vol(num_leaf), max_water))%>%filter(visit_id%in%c(131,146,151,156,161,166,171,176,181,351,356,361))%>%View

# Model for Colombia. Rioblanco 2014(371)
plot(log(guz_only$leaf_width),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(leaf_width) ,data=guz_only))

# Equation for Colombia. Rioblanco 2014(371) Vol.max= exp(-4.18214 + 1.55894*log(Col2014$num_leaf) + 2.25906*log(Col2014$leaf_width))
vol<- function(a,b){exp(-4.18214 + 1.55894*log(a) + 2.25906*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id==371, vol(num_leaf,leaf_width), max_water))%>%filter(visit_id == 371)%>%View

# Model Sonadora
summary(lm(log(max_water)~log(actual_water+1),data=guz_only))

#Equation for Sonora, exp(2.412+ 0.619*log(actual_water+1))
vol<- function(a){exp(2.412+ 0.619*log(a+1))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id%in%c(376,391,396,401,406,411,416,421,426,431,436,441,446), vol(actual_water), max_water))%>%filter(visit_id%in%c(376,391,396,401,406,411,416,421,426,431,436,441,446))%>%View

write_csv(, "data-raw/02_broms.csv")

