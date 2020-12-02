library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

red_carretera <- vroom::vroom(file = "data/RNC_dbf_to_CSV/Red_Vial.csv")
red_carretera

red_carretera

uniones_carreteras <- vroom::vroom(file = "data/RNC_dbf_to_CSV/Union.csv")



nw_carretera <- 
red_carretera %>% 
  filter(TIPO_VIAL=="Carretera") %>% 
  filter(ESTATUS=="Habilitado") %>% 
  select(UNION_INI, UNION_FIN, everything()) %>% 
  igraph::graph_from_data_frame(d = ., directed = F) %>% 
  as_tbl_graph()

nw_carretera <-
nw_carretera %>% 
  mutate(grado = centrality_degree()) %>%
  mutate(betweenness_c = centrality_betweenness(),
         w_betweenness = centrality_betweenness(weights = LONGITUD)) 


write_graph(graph = nw_carretera, file = "results/red_carretera.graphml", format = "graphml")

# ?centrality_betweenness
# ?betweenness()
# 
# nw_carretera %>% 
#   as_tibble() %>% 
#   select(betweenness_c, w_betweenness) #%>% 
#   ggplot(mapping = aes(x=w_betweenness)) + 
#   geom_density()


nw_carretera <-
nw_carretera %>% 
  activate("nodes") %>% 
  mutate(n_idx = 1:vcount(nw_carretera)) %>% 
  activate("edges") %>% 
  mutate(e_idx = 1:ecount(nw_carretera))

nw_carretera.line <-
  nw_carretera %>% 
  convert(to_linegraph)


nw_carretera.line <-
nw_carretera.line %>% 
  activate("nodes") %>% 
  left_join(y = get.data.frame(nw_carretera), 
            by = c(".tidygraph_edge_index" = "e_idx"))

