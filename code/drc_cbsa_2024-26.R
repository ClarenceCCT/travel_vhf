## code to import CBSA data on DRC
## note: DRC and Republic of Congo are aggregated in CBSA data
## this imports disaggregated data in Excel files

require(readxl)
require(tidyverse)
require(here)

my_path <- "W:/DCAP_PACD/PUBLIC/CBSA/Data/XLS_reports/aggregated_xls"
files_list <- list.files(my_path, full.names = TRUE)
files_list <- files_list[!str_detect(files_list, "~\\$")] ## remove temporary file names
files_list <- files_list[str_detect(files_list, "2025|2026")]
#drc1 <- readxl::read_xlsx(files_list[1], sheet = "Congo Aggregation")

#drc_cbsa <- map_df(files_list, ~readxl::read_xlsx(.x, sheet = "Congo Aggregation"))

sheet_name <- "Congo Aggregation"

import_cbsa <- function(x) {
  
  if(!sheet_name %in% readxl::excel_sheets(x)) return(NULL) 
  
  d <- readxl::read_xlsx(x, sheet = sheet_name, col_types = "text")
  
  
  d <- d |> 
    mutate(
      source = x
    )
  
  return(d)
  
}

## import files
drc_cbsa <- map_df(files_list, import_cbsa)
names(drc_cbsa) <- c("week", "country", "status", "volume", "source")

## filter DRC data and convert travel numbers to numerical
drc_cbsa2 <- drc_cbsa |> 
  filter(country == "Democratic Republic of Congo") |> 
  mutate(
    volume = as.numeric(volume),
    year = str_extract(source, "202[0-9]"),
    month = str_extract(week, "\\w+"),
    day = str_extract(week, "(?<=\\w\\s)[0-9]+"),
    date = as.Date(paste(year, month, day), format = "%Y %B %d")
  )

## convert to wide format
drc_cbsa_wide <- drc_cbsa2 |> 
  pivot_wider(id_cols = c(country, date, year, month, day), names_from = "status", values_from = "volume")

## save
saveRDS(drc_cbsa2, file = "W:/DCAP_PACD/Group/RAD_DER/vhf/drc_cbsa.rds")

## load data
drc_cbsa <- readRDS("W:/DCAP_PACD/Group/RAD_DER/vhf/drc_cbsa.rds")

drc_cbsa_wide <- drc_cbsa |> 
  pivot_wider(id_cols = c(country, date, year, month, day), names_from = "status", values_from = "volume") |> 
  mutate(
    mm = month(date)
  )
names(drc_cbsa_wide) <- c("country", "date", "year", "month", "day", "cc", "fn", "mm")

drc_cbsa_wide <- drc_cbsa_wide |> 
  mutate(
    all = cc + fn,
    cc_p = cc / all
  )

## annual
drc_cbsa_wide |> 
  group_by(year) |> 
  summarise(
    across(c(cc, fn, all), sum)
  )

## monthly
drc_cbsa_wide |> 
  group_by(year, mm) |> 
  summarise(
    across(c(cc, fn, all), sum)
  )

## monthly average
drc_cbsa_wide |> 
  group_by(year, mm) |> 
  summarise(
    across(c(cc, fn, all), sum)
  ) |> 
  group_by(year) |> 
  summarise(
    across(c(cc, fn, all), mean)
  )

## since March 2026
drc_cbsa_wide |> 
  filter(date >= as.Date("2026-03-01")) |> 
  group_by(year) |> 
  summarise(across(c(cc, fn, all), sum))

## Uganda, Rwanda, South Sudan
get_cc_prop
