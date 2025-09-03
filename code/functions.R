## RAD IATA data folder
iata_path_rad <- "D:/DCAP_PACD/Group/RAD_DER/travel_data/iata"

## DCAP IATA data folder 
iata_path_dcap <- "D:/DCAP_PACD/PUBLIC/IATA/"

## function to import travel data for specific airports
import_iata <- function(x, origcodes, destcodes) { #origcodes and destcodes are vectors of 3-letter airport codes for origin and destination countries
  
  d <- read_delim(x, col_names = FALSE, id = "source") #, #skip = 1,
  #col_types = cols("c","c","c","c","c","c","c","c","c"),
  #col_select = c(1:8)) 
  
  # remove empty columns (this includes empty columns after the last column that contains passenger volume)
  # convert all columns to numeric for appending
  d <- d |>
    select_if(~!(all(is.na(.)))) |>
    mutate(
      across(everything(), as.character)
    )
  
  ## note that IATA files can have varying numbers of columns, e.g. some have 5 columns for stopover destinations, some have 3
  ## need to identify which column contains the end destination
  ## find the column index for the right-most column containing a numeric variable
  ## this should correspond to the number of itineraries/passengers
  ## the preceding column should contain the end destination airport code
  #npass_col <- which(sapply(d, function(x) is.numeric(x)))
  npass_col <- length(d)
  dest_col <- npass_col - 1
  print(dest_col)
  # 
  names(d)[1] <- "source"
  names(d)[2] <- "orig"
  names(d)[npass_col] <- "npass"
  names(d)[dest_col] <- "dest"
  #names(d) <- c("source", "orig", "stop1", "stop2", "stop3", "stop4", "stop5", "dest", "npass")
  print(names(d))
  
  d <- d |> 
    filter(orig %in% origcodes & dest %in% destcodes)
  
}