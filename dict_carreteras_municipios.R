library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(maptools)
library(ggmap)
library(sf)

#read data

red_carretera <- vroom::vroom(file = "data/RNC_dbf_to_CSV/Red_Vial.csv")
uniones_carreteras <- vroom::vroom(file = "data/RNC_dbf_to_CSV/Union.csv")
localidades <- vroom::vroom(file = "data/RNC_dbf_to_CSV/Localidad.csv")

#read shapefiles

rnc_shapes <- sf::st_read(dsn = "data/carreteras/red_min.shp")

mun_shp    <- sf::st_read(dsn = "../ensanut_mun/municipal.shp")

mun_shp <- 
mun_shp %>% 
  mutate(counter_mun = 1:nrow(.))

rnc_carreteras <-
  rnc_shapes %>%
  filter(TIPO_VIAL=="Carretera") %>%
  mutate(counter_road = 1:nrow(.))

#find muns where the roads are 

road_belongs <- st_intersects(x = st_transform(x = rnc_carreteras, crs=st_crs(mun_shp)), #need to have same coordinate set
                              y=mun_shp)

road_belongs <-
road_belongs %>% 
  as.data.frame() %>% 
  rename(counter_road = row.id) %>%
  rename(counter_mun =  col.id) %>%
  as_tibble()

road_mun_dict <- 
  rnc_carreteras %>% 
  select(ID_RED, NOMBRE, CODIGO, counter_road) %>% 
  left_join(road_belongs) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  left_join(y = select(mun_shp, CVEGEO, NOM_ENT, NOM_MUN, counter_mun)) %>% 
  select(-geometry) 


# prueba <-
# rnc_shapes %>% 
#   filter(TIPO_VIAL=="Carretera") %>% 
#   head(50) %>% 
#   mutate(counter_road = 1:nrow(.))


#prueba2 <- st_intersects(x = st_transform(x = prueba, crs=st_crs(mun_shp)), y=mun_shp)
# prueba2 %>% 
#   as.data.frame() %>% 
#   rename(counter = row.id) %>% 
#   as_tibble()


prueba
prueba2 %>% str

mun_shp$CVEGEO[unlist(prueba2)]

st_crs(prueba)
st_crs(mun_shp)

st_transform(x = prueba, crs=st_crs(mun_shp))
