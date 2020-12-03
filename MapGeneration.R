################################################################################
#generate map data to visualize in Kepler
################################################################################


################################################################################
#libraries
################################################################################

library(maptools)
library(ggmap)
library(sf)
library(tidyverse)
library(janitor)
library(lubridate)

################################################################################
#load the base map for municipalities (from INEGI)
################################################################################

#load base shapefile
mun_nac <- st_read("data/base_shp.shp") #from inegi 

################################################################################
#Visualize days from first mun case to 0.01% of mun population infected
################################################################################

#load information about municipalities
mun_info <- vroom::vroom(file = "data/municipios.tsv")

mun_info <- 
  mun_info %>% 
  mutate(CVEGEO = str_pad(string = CVEGEO, width = 5, side = "left", pad = "0"))

#merge with base shapefile
mun_info_shp <- 
  merge(x = mun_nac, y = mun_info, by=c("CVEGEO"))

#writeout as csv with wkt 

mun_info_shp %>% 
  st_write("results/mun_info_shp", 
         driver="CSV", 
         layer_options = "GEOMETRY=AS_WKT",
         delete_dsn=TRUE)
