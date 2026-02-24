library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

od <- readRDS("data/pass_flighs_od_2024.Rds")
airports <- read_sf("data/airports_clean_first_pass_2024.gpkg")


qtm(airports)

names(airports) <- c("airport","country","geom_from")
od <- left_join(od, airports, by = c("airport1" = "airport","airport1_country" = "country"))
names(airports) <- c("airport","country","geom_to")
od <- left_join(od, airports, by = c("airport2" = "airport","airport2_country" = "country"))

summary(st_is_empty(od$geom_from))
summary(st_is_empty(od$geom_to))

od_good <- od[!st_is_empty(od$geom_from) & !st_is_empty(od$geom_to),]
od_bad <- od[st_is_empty(od$geom_from) | st_is_empty(od$geom_to),]
od_bad <- od_bad[!od_bad$airport2 %in% c("Unknown"),]
od_bad <- od_bad[!od_bad$airport1 %in% c("Unknown"),]
message(paste(round(nrow(od_bad) / nrow(od) * 100,3),"% bad geoms, excluding unknown"))


line <- lapply(1:nrow(od_good), function(i){
  st_cast(st_combine(x = c(od_good$geom_from[i], od_good$geom_to[i])), "LINESTRING")
})
line <- do.call(c, line)
line <- st_segmentize(line, units::set_units(1, km))
od_good <- as.data.frame(od_good)
od_good$geometry <- line
#stop()

od_good$length_km <- round(geodist::geodist(st_coordinates(od_good$geom_from), st_coordinates(od_good$geom_to), paired = TRUE, measure = "geodesic") / 1000, 1)
od_good$geom_from <- NULL
od_good$geom_to <- NULL

od_good <- st_as_sf(od_good, crs = 4326)

#od_good$length_km <- round(as.numeric(st_length(od_good)) / 1000, 1)


od_good$pass_km_2018 <- od_good$`2018` * od_good$length_km
od_good$pass_km_2019 <- od_good$`2019` * od_good$length_km
od_good$pass_km_2020 <- od_good$`2020` * od_good$length_km
od_good$pass_km_2021 <- od_good$`2021` * od_good$length_km

write_sf(od_good, "data/od_flights_pass_2024.gpkg")
