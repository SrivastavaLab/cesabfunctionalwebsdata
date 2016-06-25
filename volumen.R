# read the dataset
#setwd("~/Dropbox/CommunityAnalysis/CESAB trait working group/cesabfunctionalwebsdata")
require(dplyr)
require(readr)
require(tidyr)

# bromeliads --------------------------------------------------------------
broms <- read_csv("data-raw/01_broms.csv")
summ_broms<-read_csv("data-raw/02_broms.csv")
glimpse(summ_broms)

maxvol=broms %>%select(bromeliad_id,visit_id,diameter,num_leaf) 
View(maxvol)
####French Guiana  equation of Regis
#FrenchGuianaAechmea2007, Aechmea mertensii exp(0.611+1.09*log(fg$diameter))
frenchGuiana<- function(a){exp(0.611+1.09*log(a))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 286, frenchGuiana(diameter), max_water))%>%filter(visit_id==286)%>%View


#FrenchGuianaAechmea2008,PetitSaut, Aechmea mertensii a=diameter, b= num_leaf exp(4.838+5.5888*log(fg$diameter)-1.1649*(log(fg$diameter))^2+5.7181*log(fg$num_leaf)-1.7982*(log(fg$num_leaf)^2))
fg=filter(maxvol,visit_id=="301")

Vmax= exp(0.0292 +0.339*log(fg$diameter) + 1.3563*log(fg$num_leaf))
Vmax

frenchGuiana<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 301, frenchGuiana(diameter,num_leaf), max_water))%>%filter(visit_id==301)%>%View


#FrenchGuianaAechmea2008,Kaw, Aechmea mertensii
fg=filter(maxvol,visit_id=="296")

Vmax= exp(0.0292 +0.339*log(fg$diameter) + 1.3563*log(fg$num_leaf))
Vmax

frenchGuiana<- function(a,b){exp(0.0292 +0.339*log(a) + 1.3563*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 296, frenchGuiana(diameter,num_leaf), max_water))%>%filter(visit_id==296)%>%View

#Sitio 336, Aechmea aquilega 
fg1=filter(maxvol,visit_id=="336")
Vmax= exp(-0.4368 +0.735*log(fg1$diameter)+ 1.4260*log(fg1$num_leaf))
 
frenchGuiana<- function(a,b){exp(-0.4368 +0.735*log(a)+ 1.4260*log(b))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 336, frenchGuiana(diameter,num_leaf), max_water))%>%filter(visit_id==336)%>%View

#FrenchGuianaVriesea2007, Vriesea splendens No information of diameter and number of leaf

# how many species names are there for bromeliads
unique(summ_broms$species)

guz_only = filter(summ_broms, grepl("^Gu|^G\\.", species))
unique(guz_only$species)
glimpse(guz_only)
View(guz_only)
write_csv(guz_only,"guzmania.csv")

# We can use this equation for Honduras and Colombia2000 and 2001
plot(log(guz_only$num_leaf),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(actual_water +1),data=guz_only ))

honduras=filter(summ_broms,visit_id=="106"|visit_id=="111")
Vol.max= exp(0.78960 + 0.68576*log(honduras$num_leaf) + 0.44664*log(honduras$actual_water +1))
Col=filter(summ_broms,visit_id=="96"|visit_id=="91")
Vol.max= exp(0.78960 + 0.68576*log(Col$num_leaf) + 0.44664*log(Col$actual_water +1))

honduras<- function(a,b){exp(0.78960 + 0.68576*log(a) + 0.44664*log(b+1))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 106|visit_id==111|visit_id==91|visit_id==96, honduras(num_leaf,actual_water), max_water))%>%filter(visit_id == 106|visit_id==111|visit_id==91|visit_id==96)%>%View


#We can use this equation for Colombia.Rioblanco 2012 (366)
summary(lm(log(max_water)~log(num_leaf),data=guz_only))

Col2012=filter(summ_broms,visit_id=="366")
Vol.max=exp(-2.3418 + 2.1025*log(Col2012$num_leaf))

honduras<- function(a){exp(-2.3418 + 2.1025*log(a))}
summ_broms%>%
  mutate(max_water= ifelse(visit_id == 366, honduras(num_leaf), max_water))%>%filter(visit_id==366)%>%View



# We can use this equation for Colombia. Rioblanco 2014(371)
plot(log(guz_only$leaf_width),log(guz_only$max_water))
summary(lm(log(max_water)~log(num_leaf) + log(leaf_width) ,data=guz_only))

Col2014=filter(summ_broms,visit_id=="371")
Vol.max= exp(-4.18214 + 1.55894*log(Col2014$num_leaf) + 2.25906*log(Col2014$leaf_width))











  
write_csv(, "data-raw/02_broms.csv")

