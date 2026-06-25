## code to map health zone mobility data from HDX
## CCT
## 2026-06-23

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
require(scico)

options(scipen = 999)

## set directory
my_path <- Sys.getenv(x = "HOME")
my_path <- str_replace(my_path, "Documents", "data/gis/countries/drc")

## load functions
source(here("code", "functions.R"))

##################################################################################
## LOAD DATA
##################################################################################
## load DRC gridded population data - source is WorldPop https://wopr.worldpop.org/
## source data has population estimates for 100m grids
#filename <- paste(my_path, "drc/COD_Population_v4_3_gridded.tif", sep = "/")

## 2025 estimates from WorldPop https://hub.worldpop.org/geodata/summary?id=76927 in 1km grids
filename <- paste(my_path, "raster/cod_pop_2025_CN_1km_R2025A_UA_v1.tif", sep = "/")
#filename

r <- rast(filename)
#sources(r)
#hasValues(r)
## [1] TRUE
#plot(r, main="DRC")
#summary(values(r))

## get country shape file using rnaturalearth package
drc_sf <- ne_countries(country = "Democratic Republic of the Congo", returnclass = "sf")
#plot(drc_sf)
## get province boundaries
#drc_provinces <- ne_states(country = "Democratic Republic of the Congo", returnclass = "sf")
prov_file <- paste(my_path, "drc_provinces", "COD_admbnda_adm1_20170407.shp", sep = "/")
drc_provinces <- read_sf(prov_file)

## get health zone boundaries
#hz_file <- paste(my_path, "drc_healthzones", "GRID3_COD_health_areas_v6_0.shp", sep = "/")
hz_file <- paste(my_path, "rdc_zones-de-sante", "RDC_Zones de santé.shp", sep = "/")
drc_healthzones <- read_sf(hz_file)

## mobility data: these are from flowminder.org
## https://data.humdata.org/dataset/democratic-republic-of-congo-detected-mobility-from-2026-bvd-outbreak-health-zones
## data represent the % of a cohort of mobile phone users defined by presence in Bunia, Mongbwalu or Rwampara health zones 
## during 3–23 April 2026 - who were later detected at least once in each of the other DRC health zones during 24 April – 24 May 2026 
## (D+31). 
mob_dat <- read_csv("W:/DCAP_PACD/Group/RAD_DER/bvd/mobility_data_hdx/drc-bvd_cohort-detection_pct-2026_05_24-v1.0-external.csv")

##################################################################################
## MERGE HEALTH ZONE AND MOBILITY DATA
##################################################################################
## merge data
drc_healthzones <- drc_healthzones |> 
  left_join(mob_dat, by = c("Pcode" = "hz_id_cd"))

## highlight Kinshasa province
drc_healthzones <- drc_healthzones |> 
  mutate(
    kinshasa = if_else(province_name_short == "Kinshasa", 1, 0),
    kinshasa_ituri = if_else(province_name_short == "Kinshasa" | province_name_short == "Ituri", 1, 0)
  )

my_breaks <- c(0, 2, 5, 10, 15, 20)

## define plot colours
my_outline_cols <- c("gray50", "maroon")

drc_healthzones |> 
  ggplot() +
  geom_sf(aes(fill = detection_pct_20260524, colour = factor(kinshasa)), size = 0.05) +
  scale_fill_scico(palette = "acton", direction = -1, name = "Percentage", na.value = "transparent", breaks = my_breaks) +
  scale_colour_manual(values = my_outline_cols) +
  labs(
    title = "Monthly mobility from Bunia, Mongbwalu or Rwampara health zones",
    subtitle = "% of mobile subscribers during 3-23 April 2026 detected in other health zones in the next 31 days"
  ) +
  guides(
    colour = "none"
  ) +
  theme_minimal() 

## hybrid palette to distinguish 0 values
drc_healthzones <- drc_healthzones %>%
  mutate(fill_var = ifelse(detection_pct_20260524 == 0, NA, detection_pct_20260524))

ggplot(drc_healthzones) +
  geom_sf(aes(fill = fill_var, colour = factor(kinshasa_ituri)), size = 0.05) +
  geom_sf(data = filter(drc_healthzones, detection_pct_20260524 == 0), fill = "gray90") +
  scale_fill_scico(palette = "acton", direction = -1, name = "Percentage", na.value = "transparent", breaks = my_breaks) +
  scale_colour_manual(values = my_outline_cols) +
  labs(
    title = "Monthly mobility from Bunia, Mongbwalu or Rwampara health zones",
    subtitle = "% of mobile subscribers during 3-23 April 2026 detected in other health zones in the next 31 days"
  ) +
  guides(
    colour = "none"
  ) +
  theme_minimal() 

## % travel to Kinshasa
drc_healthzones |> 
  st_drop_geometry() |> 
  group_by(PROVINCE) |> 
  summarise(sum(detection_pct_20260524, na.rm = TRUE)) |> 
  View()
