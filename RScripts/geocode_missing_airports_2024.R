# Geocode missing 2021 airports
library(mapsapi)
library(tmap)
tmap_mode("view")


airport_missing = readRDS("data/airports_missing_2024.Rds")

head(airport_missing) # produced from combine_flights_passengers.R

airport_missing$full <- paste0(airport_missing$airport1," airport, ", airport_missing$airport1_country)

res_google <- mp_geocode(
  addresses = airport_missing$full,
  key = Sys.getenv("Google_maps_key"),
  quiet = TRUE,
  timeout = 10
)

res_google2 <- mp_get_points(res_google)
tm_shape(res_google2) +
  tm_dots(fill = "location_type", popup.vars = names(res_google2)[1:5])


airport_missing2 <- left_join(st_drop_geometry(airport_missing), res_google2, by = c("full" = "address"))
airport_missing2$geom <- NULL
airport_missing <- airport_missing2[,c("airport1","airport1_country","pnt")]
airport_missing <- st_as_sf(airport_missing)

# Fix the bad ones
airport_missing$pnt[airport_missing$airport1 == "Green Bay Wisconsin"] <- st_point(c(-88.1305372292538, 44.4916866227166))
airport_missing$pnt[airport_missing$airport1 == "Skegness"] <- st_point(c(0.33327100666535375, 53.175949621375544))

saveRDS(airport_missing, "data/airports_missing_2024.Rds")

