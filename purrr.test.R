

ne <- read.csv("ne_latlong.csv")
head(ne)

# Function to extract climate data
get_daymet <- function(...){
  print(...)
  my.row <- as.tibble(...)
  #my.row <- first.row
  # Extract plot, lat, and long for each by row
  temp_site <- my.row[,1]
  temp_lat <- my.row[,2]
  temp_lon <- my.row[,3]
  print(temp_site)
  # Run download_daymet on each location
  temp_daymet <- download_daymet(site = temp_site,
                                 lat = temp_lat,
                                 lon = temp_lon,
                                 start = 2018,
                                 end = 2019) %>% 
    # Extract df with environmental vars from list
    .$data %>% 
    # Convert to tibble
    as_tibble() %>% 
    # Only include the variables you want
    dplyr::select(., -dayl..s., -srad..W.m.2., -swe..kg.m.2.) %>% 
    # Give each its plot_id
    mutate(site_id = temp_site) %>% 
    # Get date from day of the year using lubridate
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j"))
  return(temp_daymet)
}  

returnmap_dfr(ne, get_daymet)

test <- ne %>% purrr::map_dfr(get_daymet)

test2 <- purrr::map_dfr(ne, .f= get_daymet)

test <- ne %>% purrr::pmap_dfr(get_daymet)