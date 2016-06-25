#The script will fill in missing values for max volume and total detritus in BWGdb data
#D. Srivastava, F. Ospina Bautista, G. Romero, I. Barberis
#================================
library(devtools)
install_github("richfitz/datastorr")
install_github("SrivastavaLab/fwdata")

library(dplyr)
library(fwdata)
fw_auth() #collectively angie and sarah 
fw_versions() #what versions present on your machine
fw_versions(local=FALSE)

all_data <- fw_data("0.0.1") #datasets, visits, traits, bromeliads, abundance

#cardoso2008 is visit_id=21, dataset_id="6", name is "Cardoso2008"

all_bromeliads<-all_data$bromeliads
all_visits<-all_data$visits

all_data$visits

all_bromeliads%>%filter(visit_id==21)

all_bromeliads$detritus_mass_total

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

names(detritus_wide)
#need to estimate total bomass for all cardoso2008 bromeliads from detritus>150, 
#first select just those bromeliads from detritus_wide
#then figure out which column is detritus>150
#then use it to estimate detrius total in wide format and join back to broms

broms%>%
#detritus_wide%>%select(bromeliad_id=)
#tapply(mean, detritus_wide)

