# correct elevation

add_elevation <- function(latest_bromeliad_data) {
  latest_bromeliad_data %>%
    mutate(elevation_m = ifelse(visit_id %in%c(141,361,156,171),962, elevation_m))%>% # dwarf forest
    mutate(elevation_m = ifelse(visit_id %in%c(151,166,356,181,136),750, elevation_m))%>% # Palo colorado
    mutate(elevation_m = ifelse(visit_id %in%c(146,161,351,176,131),390, elevation_m))%>% # tabunocco
    mutate(elevation_m = ifelse(visit_id %in%c(451),300, elevation_m))%>% #Saba dry forest
    mutate(elevation_m = ifelse(visit_id %in%c(121),650, elevation_m))%>% #Saba Montagne
    mutate(elevation_m = ifelse(visit_id %in%c(126),840, elevation_m))%>% #Saba cloud forest
    mutate(elevation_m = ifelse(visit_id %in%c(116),560, elevation_m))%>% #Saba SC montagne
    mutate(elevation_m = ifelse(visit_id %in%c(201),1130, elevation_m))%>% #Dominica cloud forest
    mutate(elevation_m = ifelse(visit_id %in%c(196),830, elevation_m))%>% #Dominica montagne thicket
    mutate(elevation_m = ifelse(visit_id %in%c(191),800, elevation_m))%>% #Dominica subtropical
    mutate(elevation_m = ifelse(visit_id %in%c(446),1000, elevation_m))%>% #Sonadora 1000
    mutate(elevation_m = ifelse(visit_id %in%c(376),400, elevation_m))%>% #Sonadora 400
    mutate(elevation_m = ifelse(visit_id %in%c(391),450, elevation_m))%>% #Sonadora 450
    mutate(elevation_m = ifelse(visit_id %in%c(396),500, elevation_m))%>% #Sonadora 500
    mutate(elevation_m = ifelse(visit_id %in%c(401),550, elevation_m))%>% #Sonadora 550
    mutate(elevation_m = ifelse(visit_id %in%c(506),600, elevation_m))%>% #Sonadora 600
    mutate(elevation_m = ifelse(visit_id %in%c(411),650, elevation_m))%>% #Sonadora 650
    mutate(elevation_m = ifelse(visit_id %in%c(416),700, elevation_m))%>% #Sonadora 700
    mutate(elevation_m = ifelse(visit_id %in%c(421),750, elevation_m))%>% #Sonadora 750
    mutate(elevation_m = ifelse(visit_id %in%c(426),800, elevation_m))%>% #Sonadora 800
    mutate(elevation_m = ifelse(visit_id %in%c(431),850, elevation_m))%>% #Sonadora 850
    mutate(elevation_m = ifelse(visit_id %in%c(436),900, elevation_m))%>% #Sonadora 900
    mutate(elevation_m = ifelse(visit_id %in%c(441),950, elevation_m))
}
