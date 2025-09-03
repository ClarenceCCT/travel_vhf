## sample code to use raster files with terra package
## CCT
## 2025-09-02

## load libraries
library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tidyterra)
library(airportr)

## set directory
my_path <- Sys.getenv(x = "HOME")
my_path <- str_replace(my_path, "Documents", "data/gis/raster")


## load sample data
filename <- paste(my_path, "drc/COD_Population_v4_3_gridded.tif", sep = "/")
filename

r <- rast(filename)
#sources(r)
#hasValues(r)
## [1] TRUE
plot(r, main="DRC")
summary(values(r))

r2 <- aggregate(r, fact = 25, fun = sum, na.rm = TRUE)
plot(r2)

## get shape file
drc_sf <- ne_countries(country = "Democratic Republic of the Congo", returnclass = "sf")
#plot(drc_sf)

## plot
ggplot() +
  geom_spatraster(data = r2) +
  geom_sf(data = drc_sf, fill = NA, color = "black", size = 0.8) +
  geom_po(data = my_coords, aes(x = Latitude, y = "Longitude"), size = 2, colour = "maroon") +
  scale_fill_viridis_c(name = "Values", na.value = "transparent") +
  theme_minimal()

## find airports in DRC
all_airports <- airportr::airports |> 
  janitor::clean_names()
drc_airports <- all_airports |> 
  filter(country_code_alpha_3 == "COD")
my_coords <- airport_location("FIH")
my_coords <- st_as_sf(drc_airports, coords = c("longitude", "latitude"), crs = 4326)

## plot
ggplot() +
  geom_spatraster(data = r2) +
  geom_sf(data = drc_sf, fill = NA, color = "black", size = 0.8) +
  geom_sf(data = my_coords, colour = "maroon", size = 2) +
  scale_fill_viridis_c(name = "Values", na.value = "transparent") +
  theme_minimal()

