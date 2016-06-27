# read the dataset

library(dplyr)
library(readr)
library(tidyr)

# bromeliads --------------------------------------------------------------

summ_broms0 <- read.csv("data-raw/02_broms.csv")

broms01 <- read.csv("data-raw/01_broms.csv")
id_spp <- broms01 %>%
  select(bromeliad_id, species, actual_water) %>%
  distinct()

summ_broms0 <- summ_broms0 %>%
  left_join(id_spp)

summ_broms <- summ_broms0

## French Guiana, We used the Regis's equation ####
## ~/Dropbox/communityanalysis/CESAB\ trait\ working\ group/Max_Volume_models/
## FrenchGuianaAechmea2007,
## Equation= exp(0.611+1.09*log(diameter)) for Aechmea mertensii
volFrench2007<- function(a){exp(0.611+1.09*log(a))}
summ_broms <- summ_broms %>%
  mutate(max_water= ifelse(visit_id == 286, volFrench2007(diameter), NA)) ## NA not max_water, this creates the column as all NA

#FrenchGuianaAechmea2008,PetitSaut, Aechmea mertensii, Vmax=exp(4.838+5.5888*log(diameter)-1.1649*(log(fg$diameter))^2+5.7181*log(fg$num_leaf)-1.7982*(log(fg$num_leaf)^2))
volFrench2008<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id == 301, volFrench2008(diameter,num_leaf), max_water))

#FrenchGuianaAechmea2008,Kaw, Aechmea mertensii,Vmax= exp(0.0292 +0.339*log(fg$diameter) + 1.3563*log(fg$num_leaf))
volFrench2008_2<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id == 296, volFrench2008_2(diameter,num_leaf), max_water))

#Nouragues2009,Sinnamary2011, Aechmea aquilega Vmax= exp(-0.4368 +0.735*log(fg1$diameter)+ 1.4260*log(fg1$num_leaf))
volNouragues2009<- function(a,b){exp(-0.4368 +0.735*log(a)+ 1.4260*log(b))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id == 336|visit_id==331, volNouragues2009(diameter,num_leaf), max_water))


#FrenchGuianaVriesea2007, Vriesea splendens No information of diameter and number of leaf, information about actual water
#select data set only Vriesea splendens and create an equation
# Equation vmax = 1.4144*actual vol + 39.343
volFrench2007Vrisea<- function(a){1.4144*a + 39.343}
summ_broms <-summ_broms%>%
  mutate(max_water= ifelse(visit_id == 341 & species=="Vriesea splendens", volFrench2007Vrisea(actual_water), max_water))

#### For the other sites ####

#how many species names are there for bromeliads
unique(summ_broms$species)
#select only Guzmania
guz_only = filter(summ_broms, grepl("^Gu|^G\\.", species))
unique(guz_only$species)
glimpse(guz_only)

# Model for Honduras and Colombia2000, 2001
plot(log(guz_only$num_leaf),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(actual_water +1),data=guz_only ))

# Equation for Honduras and Colombia2000 and 2001:Vmax=exp(0.78960 + 0.68576*log(Col$num_leaf) + 0.44664*log(Col$actual_water +1))
#Tillandsia
volHonduras2000<- function(a,b){exp(0.78960 + 0.68576*log(a) + 0.44664*log(b+1))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id == 106|visit_id==111|visit_id==91|visit_id==96, volHonduras2000(num_leaf,actual_water), max_water))

#Model for Colombia.Rioblanco 2012 (366) and ElVerde
summary(lm(log(max_water)~log(num_leaf),data=guz_only))

# equation for Colombia.Rioblanco 2012 (366) and ElVerde Vol.max=exp(-2.3418 + 2.1025*log(Col2012$num_leaf))
#Guzmania
volColombia2012<- function(a){exp(-2.3418 + 2.1025*log(a))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id%in%c(131,146,151,156,161,166,171,176,181,351,356,361,366), volColombia2012(num_leaf), max_water))

# Model for Colombia. Rioblanco 2014(371)
plot(log(guz_only$leaf_width),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(leaf_width) ,data=guz_only))

# Equation for Colombia. Rioblanco 2014(371) Vol.max= exp(-4.18214 + 1.55894*log(Col2014$num_leaf) + 2.25906*log(Col2014$leaf_width))
#Guzmania
volColombia2014<- function(a,b){exp(-4.18214 + 1.55894*log(a) + 2.25906*log(b))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id==371, volColombia2014(num_leaf,leaf_width), max_water))

# Model Sonadora
summary(lm(log(max_water)~log(actual_water+1),data=guz_only))

#Equation for Sonora, exp(2.412+ 0.619*log(actual_water+1))
#Vriesea or Guzmania
volSonora<- function(a){exp(2.412+ 0.619*log(a+1))}
summ_broms <- summ_broms%>%
  mutate(max_water= ifelse(visit_id%in%c(376,391,396,401,406,411,416,421,426,431,436,441,446), volSonora(actual_water), max_water))

stopifnot(dim(summ_broms0) == dim(summ_broms))

volume_final <- summ_broms %>%
  select(bromeliad_id, max_water)

write.csv(volume_final, "data-raw/24_volume.csv", row.names = FALSE)

