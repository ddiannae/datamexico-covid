library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(maptools)
library(ggmap)
library(sf)

#read shapefile
rnc_shapes <- sf::st_read(dsn = "data/carreteras/red_min.shp")

#read network (analysed)
nw_carretera.line <- igraph::read_graph(file = "results/red_carretera_linegraph.graphml")

rnc_shapes.analysed <- 
nw_carretera.line %>% 
  as_tibble() %>% 
  select(ID_RED, grado, betweenness_c) %>% 
  merge(x = rnc_shapes, y = ., by=c("ID_RED"))

st_write(obj = rnc_shapes.analysed, 
         dsn = "results/wkt_rnc_analysed",
         driver= "CSV", 
         layer_options = "GEOMETRY=AS_WKT",
         delete_dsn=TRUE)
