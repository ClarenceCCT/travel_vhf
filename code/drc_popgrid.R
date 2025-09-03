## sample code to use raster files with terra package
## CCT
## 2025-09-02

##################################################################################
## LOAD LIBRARIES
##################################################################################
## load libraries
library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tidyterra)
library(airportr)
library(patchwork)
library(here)

options(scipen = 999)

## set directory
my_path <- Sys.getenv(x = "HOME")
my_path <- str_replace(my_path, "Documents", "data/gis/raster")

##################################################################################
## LOAD DATA
##################################################################################
## load DRC gridded population data - source is WorldPop https://wopr.worldpop.org/
## source data has population estimates for 100m grids
filename <- paste(my_path, "drc/COD_Population_v4_3_gridded.tif", sep = "/")
filename

r <- rast(filename)
#sources(r)
#hasValues(r)
## [1] TRUE
#plot(r, main="DRC")
#summary(values(r))

##################################################################################
## PROCESS DATA
##################################################################################
## aggregate raster cells into 10km grids
r2 <- aggregate(r, fact = 100, fun = sum, na.rm = TRUE)
#plot(r2)

## get shape file using rnaturalearth package
drc_sf <- ne_countries(country = "Democratic Republic of the Congo", returnclass = "sf")
#plot(drc_sf)

##################################################################################
## PLOT POPULATION DATA
##################################################################################
## find airports in DRC
all_airports <- airportr::airports |> 
  janitor::clean_names()
drc_airports <- all_airports |> 
  filter(country_code_alpha_3 == "COD")
my_coords <- st_as_sf(drc_airports, coords = c("longitude", "latitude"), crs = 4326)

## plot population 
pop_plot <- ggplot() +
  geom_spatraster(data = r2) +
  geom_sf(data = drc_sf, fill = NA, color = "black", size = 0.8) +
  #geom_sf(data = my_coords, fill = "maroon", shape = 22, size = 2) +
  scale_fill_viridis_c(name = "Values", na.value = "transparent", begin = 0) +
  labs(
    title = "Population in 10km grids"
  ) +
  theme_minimal()

##################################################################################
## CALCULATE DISTANCE TO INTERNATIONAL AIRPORT IN KINSHASA (FIH)
##################################################################################
## get coordinates for Kinshasa airport using airportr package
kinshasa_coords <- vect(cbind(airport_location("FIH")$Longitude, airport_location("FIH")$Latitude), crs = crs(r2))

## calculate straight-line distance using terra package (in meters)
dist_to_fih <- distance(r2, kinshasa_coords)
plot(dist_to_fih / 1000)

## convert NAs in r2 to 0
#r3 <- subst(r2, NA, 0)

##################################################################################
## WEIGHT POPULATION SIZE IN EACH RASTER BY INVERSE SQUARED DISTANCE TO FIH AIRPORT
##################################################################################
## multiply raster population in r3 by distance to Kinshasa in dist_to_fih, normalize to sum to 1
r2_weighted <- r2 * (1 / dist_to_fih^2)
r2_sum <- global(r2_weighted, fun = "sum", na.rm = TRUE) # sum all values
r2_weighted <- r2_weighted / r2_sum[[1]] # normalize values to sum to 1
#r3_weighted <- r3 * (1 / dist_to_fih)

##################################################################################
## CRUDE VS WEIGHTED PLOTS
##################################################################################
## plot population weighted by inverse distance squared
pop_weighted_plot <- ggplot() +
  geom_spatraster(data = r2_weighted) +
  geom_sf(data = drc_sf, fill = NA, color = "black", size = 0.8) +
  #geom_sf(data = my_coords, fill = "maroon", shape = 22, size = 2) +
  scale_fill_viridis_c(name = "Values", na.value = "transparent", begin = 0) +
  labs(
    title = "Normalized population weighted by inverse squared distance to FIH"
  ) +
  theme_minimal()

## combine plots
pop_plot + pop_weighted_plot

