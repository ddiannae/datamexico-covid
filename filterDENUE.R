library(vroom)
library(stringr)
library(dplyr)
library(data.table)
data_path <- "/datos/ot/diana"

denue_files <-dir(path = data_path, 
                  pattern = "th_denue_act_esenciales_ent\\d+_\\d+.csv",
                  full.names = TRUE)

denue_dt1 <- fread(denue_files[1])

### uno por uno porque no tienen el mismo numero de columnas
denue1 <- vroom(denue_files[1], delim= "\t", quote = "\"", col_select = c(id, nom_estab, codigo_act, per_ocu, tipo_vial, 
                                            tipo_asent, nomb_asent, cve_ent, entidad, cve_mun, municipio, cve_loc, localidad,
                                            ageb, latitud, longitud)
    )

head(denue1)

denue1 %>% select(entidad) %>% table()
denue_dt1$entidad %>% table()
denue_wrong_id <- denue %>% mutate(id_num = as.numeric(id)) %>% filter(is.na(id_num))
denue_wrong_cp <- denue %>% mutate(cod_postal_num = as.numeric(cod_postal)) %>% filter(is.na(cod_postal_num))
denue_wrong_act <- denue %>% mutate(cod_act_num = as.numeric(cod_act)) %>% filter(is.na(cod_act_num))

table(denue$tipo_vial[1:1000])

denue_min <- denue[1:3000, ]
hoteles_rest <- denue  %>% filter(str_starts(codigo_act, "72")) 
hoteles_rest <- hoteles_rest %>% mutate(per_ocu_num = as.numeric(per_ocu)) 
hoteles_rest_wrong <- hoteles_rest %>% filter(is.na(per_ocu_num))
