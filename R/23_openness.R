
require(dplyr)

## calculating canopy openness
## open, vs closed, for sites where this is acceptable.
## 1 == open?
bromeliad_wide<-read.csv("data-raw/02_bromeliad_wide.csv", stringsAsFactors = FALSE)
str(bromeliad_wide)
bromeliad_wide$canopy.openess[281]<-"open"
bromeliad_wide$open.canopy[281]
bromeliad_wide$visit_id[541]
bromeliad_wide$incident_radiation_percentage[517]<-14.59

ignacioconvert<-function(a){
  ifelse(a=="open",1,(ifelse(a=="closed",0,(ifelse(a=="edge",0,NA)))))
}


bromeliad_wide <- bromeliad_wide %>%
  mutate(open.canopy = ifelse(visit_id %in%c(281,266,271),ignacioconvert(canopy.openess), open.canopy))%>%
  mutate(open.canopy = ifelse(visit_id %in%c(331,311),1, ifelse(visit_id %in%c(326,316,306),0,open.canopy)))%>%
  mutate(open.canopy = ifelse(visit_id %in%c(231),1, ifelse(visit_id %in%c(21),0,open.canopy)))

open_bromeliads <- bromeliad_wide %>%
  select(bromeliad_id, open.canopy)

write.csv(open_bromeliads, "data-raw/23_open.csv", row.names = FALSE)
