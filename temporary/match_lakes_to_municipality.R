# matching lakes to municipality and write the resulting file back to "./data/input/locations.csv"

library(sf)
library(GADMTools)

# load lake list
lakes <- read.csv("./data/input/locations.csv")
lakes <- lakes[!is.na(lakes$decimalLatitude),] # some missing lat/long in data (n=4), removing those 
lakes_sf = st_as_sf(lakes, coords = c("decimalLongitude", "decimalLatitude"), 
                 crs = 4326)

# load spatial layer of muncipalites etc, from https://gadm.org 
tempdir <- tempdir()
download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_NOR_2_sf.rds", paste0(tempdir,"/gadm_NO.rds"))
NO_gadm <- readRDS(paste0(tempdir,"/gadm_NO.rds")) %>%
  select(countryCode=GID_0,county=NAME_1,municipality=NAME_2)

locations_tmp <- st_join(lakes_sf, NO_gadm, join = st_nearest_feature) # NB: using "st_nearest_feature" because of some border lakes with centroid in other countries
lakes$municipality <- locations_tmp$municipality
lakes$county <- locations_tmp$county
lakes$countryCode <- locations_tmp$countryCode

write.csv(lakes,"./data/input/locations.csv",row.names = FALSE)
