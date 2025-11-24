## sample code to use raster files with terra package
## CCT
## 2025-11-17

##################################################################################
## LOAD LIBRARIES
##################################################################################
## clear environment
rm(list = ls())

## load libraries
library(terra)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(tidyterra)
#library(exactextractr)
library(airportr)
library(patchwork)
library(here)

options(scipen = 999)

## set directory
my_path <- Sys.getenv(x = "HOME")
my_path <- str_replace(my_path, "Documents", "data/gis/countries/eth")

## load functions
source(here("code", "functions.R"))

##################################################################################
## LOAD DATA
##################################################################################
## load ETH gridded population data - source is WorldPop https://wopr.worldpop.org/
## source data has population estimates for 100m grids
#filename <- paste(my_path, "drc/COD_Population_v4_3_gridded.tif", sep = "/")

## 2025 estimates from WorldPop https://hub.worldpop.org/geodata/summary?id=76927 in 1km grids
filename <- paste(my_path, "raster/eth_pop_2025_CN_1km_R2025A_UA_v1.tif", sep = "/")
#filename

r <- rast(filename)
#sources(r)
#hasValues(r)
## [1] TRUE
#plot(r, main="DRC")
#summary(values(r))

## get country shape file using rnaturalearth package
eth_sf <- ne_countries(country = "Ethiopia", returnclass = "sf")
#plot(drc_sf)
## get province boundaries
#drc_provinces <- ne_states(country = "Democratic Republic of the Congo", returnclass = "sf")
prov_file <- paste(my_path, "eth_admin_boundaries", "eth_admin2.shp", sep = "/")
eth_zones <- read_sf(prov_file)

#hz_file <- paste(my_path, "drc_healthzones", "GRID3_COD_health_areas_v6_0.shp", sep = "/")
#hz_file <- paste(my_path, "rdc_zones-de-sante", "RDC_Zones de santé.shp", sep = "/")
#drc_healthzones <- read_sf(hz_file)

## IATA travel volumes to DRC
iata_file <- paste(save_path_rad, "eth_arrivals.rds", sep = "/")
eth_arrivals <- readRDS(iata_file)

##################################################################################
## PROCESS DATA
##################################################################################
## aggregate raster cells into 10km grids
r2 <- aggregate(r, fact = 10, fun = sum, na.rm = TRUE)
#plot(r2)

##################################################################################
## PLOT POPULATION DATA
##################################################################################
## plot population 
pop_plot <- ggplot() +
  geom_spatraster(data = r2) +
  geom_sf(data = eth_sf, fill = NA, color = "black", size = 0.8) +
  #geom_sf(data = my_coords, fill = "maroon", shape = 22, size = 2) +
  scale_fill_viridis_c(name = "Values", na.value = "transparent", begin = 0) +
  labs(
    title = "Population in 10km grids"
  ) +
  theme_minimal()

#pop_plot

##################################################################################
## CALCULATE DISTANCE TO AIRPORTS WITH ITINERARIES ENDING IN CANADA 
##################################################################################
## find airports in ETH from which travel to Canada originates (from IATA data)
my_airport_codes <- eth_arrivals |> 
  distinct(orig) |> 
  pull()

## create a list, with each element containing a single airport and its coordinates
my_airports_list <- airportr::airports |> 
  janitor::clean_names() |> 
  filter(iata %in% my_airport_codes) |> 
  group_by(iata) |>
  group_split(.keep = TRUE)

## get list of airport codes present in both datasets   
my_airport_codes2 <- map_chr(my_airports_list, ~ .x$iata)

## generate a list of SpatVector objects (coordinate vectors for each airport)
my_coords_list <- map(my_airports_list, ~vect(cbind(.x$longitude, .x$latitude), crs = crs(r2)))

## function to calculate distances to each airport
calculate_individual_airport_distances <- function(raster_object, airport_list, 
                                                   airport_names = NULL,
                                                   units = "m") {
  
  require(terra)
  
  # Combine airports
  all_airports <- do.call(rbind, airport_list)
  
  # Create names if not provided
  if (is.null(airport_names)) {
    airport_names <- paste0("Airport_", 1:nrow(all_airports))
  }
  
  # Calculate distance to each airport separately
  distance_layers <- list()
  
  for (i in 1:nrow(all_airports)) {
    message(paste("Processing airport", i, "of", nrow(all_airports)))
    
    single_airport <- all_airports[i, ]
    distance_layers[[i]] <- distance(raster_object, single_airport)
    
    if (units == "km") {
      
      distance_layers[[i]] <- distance_layers[[i]] / 1000
    }
      
  }
  
  # Combine into multi-layer raster
  distance_raster <- rast(distance_layers)
  names(distance_raster) <- airport_names
  
  return(distance_raster)
}

## get distances and output to multi-layer raster
airport_dist_raster <- calculate_individual_airport_distances(
  r2, my_coords_list, my_airport_codes2, units = "km"
)

## sample plot
#plot(airport_dist_raster[["FBM"]])

##################################################################################
## WEIGHT POPULATION SIZE IN EACH RASTER BY INVERSE SQUARED DISTANCE TO EACH AIRPORT
##################################################################################
# Step 1: Calculate inverse  or inverse squared distances
#airport_dist_raster_invsq <- 1 / (airport_dist_raster^2)
airport_dist_raster_invsq <- 1 / (airport_dist_raster)

# Step 2: Multiply by population
r2_weighted <- airport_dist_raster_invsq * r2

# Handle infinite values (where distance = 0)
r2_weighted[is.infinite(r2_weighted)] <- 0

# Normalize layers to sum to 1
# Calculate sums for all layers at once
layer_sums <- global(r2_weighted, "sum", na.rm = TRUE)

# Normalize each layer by its sum
r2_norm <- r2_weighted / as.numeric(layer_sums[[1]])

# Verify normalization
#layer_sums <- global(r2_norm, "sum", na.rm = TRUE)
#print(layer_sums)  # Should all be 1.0

## sample plot
#plot(r2_weighted[["FBM"]])

##################################################################################
## CRUDE VS WEIGHTED PLOTS
##################################################################################
## plot population weighted by inverse distance squared
pop_weighted_plot <- ggplot() +
  geom_spatraster(data = r2_weighted[["ADD"]]) +
  geom_sf(data = eth_sf, fill = NA, color = "black", size = 0.8) +
  #geom_sf(data = my_coords, fill = "maroon", shape = 22, size = 2) +
  scale_fill_viridis_c(name = "Values", na.value = "transparent", begin = 0) +
  labs(
    title = "Normalized population weighted by inverse distance to ADD"
  ) +
  theme_minimal()

## combine plots
pop_plot + pop_weighted_plot

##################################################################################
## INTERSECT POPULATION ESTIMATES WITH PROVINCE BOUNDARIES
##################################################################################

if(st_crs(eth_zones) != crs(r2)) {
  eth_zones <- st_transform(eth_zones, crs(r2))
}

# Extract population estimates for each province (this gets all raster cells that intersect each province)
province_populations <- terra::extract(r2, eth_zones, fun = sum, na.rm = TRUE)

# Add to provinces data
eth_zones$population <- province_populations[,2]  # Second column has the sums

# Extract weighted travel probability densities for each province
province_weights <- terra::extract(r2_norm, eth_zones, fun = sum, na.rm = TRUE)
max_col <- ncol(province_weights)

# rescale weights to sum to 1 (terra::extract values may not add to 1 because of inaccuracies when raster cells cross province boundaries)
province_weights <- province_weights |> 
  mutate(across(c(2, max_col), ~ .x / sum(.x)))

eth_zones <- eth_zones |> 
  bind_cols(province_weights |> select(-ID))

#drc_provinces |> select(NOM, FIH, GOM, FBM) |> View()

## get overall probability of travel based on weighted sum of airport-specific weighted probabilities
## and proportion of flights to Canada originating from each airport
## from file drc_airport_percents.rds
eth_airport_percents <- readRDS(here("output", "eth_airport_percents.rds"))

# reshape weighted travel probability densities for each airport to long format
# and merge with proportion of flights to Canada originating from each airport
eth_zones_long <- eth_zones |> 
  st_drop_geometry() |> 
  select(adm2_name, ADD:MQX) |> 
  pivot_longer(ADD:MQX, names_to = "airport_code", values_to = "popweight") |> 
  left_join(eth_airport_percents, by = c("airport_code" =  "orig"))

# multiply travel probability densities by airport relative travel volume to Canada
eth_zones_long <- eth_zones_long |> 
  mutate(
    weighted_prop = popweight * p,
    weighted_volume = round(popweight * volume / 24, 0) # per month
  ) |> 
  group_by(adm2_name) |> 
  summarise(
    weighted_prop = sum(weighted_prop),
    weighted_volume = sum(weighted_volume)
  )

eth_zones <- eth_zones |> 
  left_join(eth_zones_long)

##################################################################################
## PLOT
##################################################################################
# identify South Omo zone
eth_zones <- eth_zones |> 
  mutate(
    southomo = factor(if_else(adm2_name == "South Omo", 1, 0))
  )

# map
my_outline_cols <- c("gray50", "yellow")
#my_alphas <- c(0.8, 1)
my_breaks <- c(0, 0.01, 0.1, 1, 10, 100)

province_weighted_map <- eth_zones |> 
  ggplot() +
  geom_sf(aes(fill = weighted_prop * 100, color = southomo), size = 0.8) +
  #geom_sf(data = my_coords, fill = "maroon", shape = 22, size = 2) +
  scale_fill_viridis_c(name = "Percentage\n(log scale)", na.value = "transparent",
                       breaks = my_breaks,
                       trans = "log10") +
  scale_colour_manual(values = my_outline_cols) +
  #scale_alpha_manual(values = my_alphas) +
  labs(
    title = "Estimated % of travellers to Canada by zone in ETH",
    subtitle = "Weighted by population size and distance to international airports",
    #fill = "Percentage"
  ) +
  guides(
    colour = "none",
    #size = "none"
  ) +
  theme_minimal() 
  

province_weighted_map

province_weighted_plot <- eth_zones |> 
  ggplot(aes(x = weighted_prop, y = fct_reorder(adm2_name, weighted_prop))) +
  geom_col(aes(fill = southomo)) +
  geom_text(aes(label = weighted_volume), hjust = -0.5, size = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  guides(
    fill = "none"
  ) +
  labs(
    title = "Average estimated monthly travel volume to Canada by ETH zone",
    y = "",
    x = "Percentage of travellers from ETH"
  )

province_weighted_plot +
  theme(
    aspect.ratio = 1,
    axis.text.y = element_text(size = 6)
  )

##################################################################################
## INTERSECT WITH HEALTH ZONE BOUNDARIES TO CALCULATE HEALTH ZONE POPULATION
##################################################################################
# if(st_crs(drc_healthzones) != crs(r2)) {
#   drc_healthzones <- st_transform(drc_healthzones, crs(r2))
# }
# 
# # Extract population estimates for each health zone
# hz_populations <- terra::extract(r2, drc_healthzones, fun = sum, na.rm = TRUE)
# 
# # Add to health zone data
# drc_healthzones$population <- hz_populations[,2]  # Second column has the sums
# 
# # tabulate populations
# my_healthzones <- c("Mweka", "Bulape", "Mushenge", "Dekese")
# drc_healthzones |> 
#   filter(Nom %in% my_healthzones) |> 
#   select(Nom, PROVINCE, population)

##################################################################################
## ALTERNATIVE METHOD: DEFINE A RADIUS OF X KILOMETERS AROUND EVENT
##################################################################################