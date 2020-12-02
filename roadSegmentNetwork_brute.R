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

#get intersections
#mis_uniones <- uniones_carreteras$ID_UNION
#names(mis_uniones) <- mis_uniones

uniones_carreteras_a <-
  red_carretera %>% 
  filter(TIPO_VIAL=="Carretera") %>% 
  pull(UNION_INI) %>% unique()

uniones_carreteras_b <-
  red_carretera %>% 
  filter(TIPO_VIAL=="Carretera") %>% 
  pull(UNION_FIN) %>% unique()

mis_uniones <- union(uniones_carreteras_a, uniones_carreteras_b)

#search for uniones in red
# 
# red_carretera %>% 
#   #get those sections that begin or end at the intersection
#   filter(UNION_INI==1 | UNION_FIN==1) %>% 
#   #pull the road id
#   pull(ID_RED) %>% 
#   #since we consider that things that begin or end at the same junction
#   #are connected
#   #we expand grid to get the combinations of roads that connect at the junction
#   expand.grid(.,.) %>% 
#   #we remove the expanded rows of road connecting with itself
#   filter(Var1!=Var2) %>% 
#   #now we operate rowwise
#   rowwise() %>% 
#   #and we order the roads so that the first column is the one with lower number
#   mutate(x = min(Var1, Var2),
#          y = max(Var1, Var2)) %>% 
#   select(x,y) %>% #select only this new vars
#   #this way we can use distinct to keep only the unique connections
#   distinct() %>% 
#   #and finally we mutate the union ID so that we know where do they intersect
#   mutate(ID_UNION=1) %>% 
#   ungroup()

tempus <- Sys.time()
road_segment_nw <-
lapply(mis_uniones[1:10], function(i){
  #print(i)
  
  r <- 
  red_carretera %>% 
    #get those sections that begin or end at the intersection
    filter(UNION_INI==i | UNION_FIN==i) %>% 
    #pull the road id
    pull(ID_RED) %>% 
    #since we consider that things that begin or end at the same junction
    #are connected
    #we expand grid to get the combinations of roads that connect at the junction
    expand.grid(.,.) %>% 
    #we remove the expanded rows of road connecting with itself
    filter(Var1!=Var2) %>% 
  
  #print(r)
  
  #r %>% 
    #now we operate rowwise
    rowwise() %>% 
    #and we order the roads so that the first column is the one with lower number
    mutate(x = min(Var1, Var2),
           y = max(Var1, Var2)) %>% 
    select(x,y) %>% #select only this new vars
    #this way we can use distinct to keep only the unique connections
    distinct() %>% 
    #and finally we mutate the union ID so that we know where do they intersect
    mutate(ID_UNION=i) %>% 
    ungroup()
}) %>% bind_rows
tempus <- Sys.time() - tempus
print(tempus)

vroom::vroom_write(road_segment_nw, "results/road_segment_nw.txt")

