library(tidyverse)
library(vroom)
library(lubridate)
library(jsonlite)
library(pals)

dm_covidmun <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=gobmx_covid_stats_mun"
dm_pop <- "https://api.datamexico.org/tesseract/data.jsonrecords?cube=inegi_population"
meas <- "measures="
drill <- "drilldowns="

dds <- c("Time", "Municipality", "State")
mms <- c("Daily+Cases", "Daily+Hospitalized", "Daily+Deaths", "Accum+Cases", "Accum+Deaths")
dds_pop <- c("Municipality")
mms_pop <- c("Population")

covid_url <- paste0(dm_covidmun, "&",
                    drill, dds[1], "%2C", dds[2], "%2C", dds[3], "&",
                    meas, mms[1], "%2C", mms[2], "%2C", mms[3], "%2C",mms[4], "%2C",mms[5], "&parents=false&sparse=false")

pop_url <- paste0(dm_pop, "&", 
                  drill, dds_pop[1], "&", 
                  meas, mms_pop[1], "&parents=false&sparse=false")

xxx <- jsonlite::fromJSON(txt = covid_url)
xxx$data <- xxx$data %>% 
  janitor::clean_names() %>% 
  mutate(day = as.Date(time)) 

ppp <-  jsonlite::fromJSON(txt = pop_url)

ppp$data <- ppp$data %>%
  janitor::clean_names() %>% 
  mutate(pop_100 = population/100000)

municipios <- vroom("data/municipios.tsv") 
m_carreteros <- municipios %>% filter(is_carretero == TRUE)
m_carreteros %>% filter(! is.na(days_to_01))
## Seleccionamos San Felipe del Progreso, México
m_no_carreteros <- municipios %>% filter(is_carretero == FALSE)
### Seleccionamos Atlixco, Puebla
m_no_carreteros %>% filter(poblacion_total > 68000 & poblacion_total < 68500)

mun_plots = c(22012, 11004)
names(mun_plots) <- c("Pedro Escobedo, Querétaro", "Apaseo el Alto, Guanajuato")
getActiveBetweenDates <- function(start, end, mun) {
  dcs <- xxx$data %>% filter(municipality_id == mun, day >= start, day <= end) %>%
    summarise(cases = sum(daily_cases), deaths = sum(daily_deaths)) %>% unlist(use.names = F)
  return(dcs[1] - dcs[2])
}

for (i in 1:length(mun_plots)) {
  ### Casos activos
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    mutate(prev_active = day - 14, 
           active_cases = purrr::pmap_dbl(list(prev_active, day, mun_plots[i]), 
                                          getActiveBetweenDates)) %>%
    ggplot(mapping = aes(x = day, 
                         y = active_cases)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(names(mun_plots[i])) +
    ylab("Casos activos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_casos_activos.png"))
  
  ### Nuevos casos
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_cases)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(names(mun_plots[i])) +
    ylab("Nuevos casos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_nuevos_casos.png"))
  
  ### Decesos
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_deaths)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(names(mun_plots[i])) +
    ylab("Decesos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_decesos.png"))
  
  ### Casos activos por 100 mil hab
  pop_100 <- ppp$data %>% filter(municipality_id == mun_plots[i]) %>% 
    select(pop_100) %>% unlist(use.names = F)
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    mutate(prev_active = day - 14, 
           active_cases = purrr::pmap_dbl(list(prev_active, day, mun_plots[i]), 
                                          getActiveBetweenDates)) %>%
    ggplot(mapping = aes(x = day, 
                         y = active_cases/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(names(mun_plots[i])) +
    ylab("Casos activos por 100 mil hab.") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_casos_activos_100k_hab.png"))
  
  ### Nuevos casos por 100 mil hab
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_cases/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun_plots[i]) +
    ylab("Nuevos casos por 100 mil hab.") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_nuevos_casos_100k_hab.png"))
  
  ### Decesos
  xxx$data %>%
    filter(municipality_id == mun_plots[i], 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_deaths/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun_plots[i]) +
    ylab("Decesos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun_plots[i], "_decesos_100k_hab.png"))
}

