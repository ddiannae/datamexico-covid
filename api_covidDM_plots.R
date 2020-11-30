library(tidyverse)
library(vroom)
library(lubridate)
library(jsonlite)
library(pals)

dm_covidmun <- "https://dev-api.datamexico.org/tesseract/cubes/gobmx_covid_stats_mun/aggregate.jsonrecords"
dm_pop <- "https://dev-api.datamexico.org/tesseract/cubes/inegi_population/aggregate.jsonrecords"
meas <- "measures%5B%5D="
drill <- "drilldowns%5B%5D="

dds <- c("Reported+Date.Time.Time", "Geography.Geography.Municipality")
mms <- c("Daily+Cases", "Daily+Hospitalized", "Daily+Deaths")
covid_url <- paste0(dm_covidmun, "?",
                 drill, dds[1], "&", drill, dds[2], "&", 
                 meas, mms[1], "&", meas, mms[2], "&", meas, mms[3], 
                 "&parents=false&sparse=false")

dds_pop <- c("Geography.Municipality")
mms_pop <- c("Population")
pop_url <- paste0(dm_pop, "?", 
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

municipios <- ppp$data %>% arrange(desc(pop_100)) %>% head(20) %>%
  select(municipality) %>% unlist(use.names = F)

getActiveBetweenDates <- function(start, end, mun) {
  dcs <- xxx$data %>% filter(municipality == mun, day >= start, day <= end) %>%
    summarise(cases = sum(daily_cases), deaths = sum(daily_deaths)) %>% unlist(use.names = F)
  return(dcs[1] - dcs[2])
}

for (mun in municipios) {
  ### Casos activos
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    mutate(prev_active = day - 14, 
           active_cases = purrr::pmap_dbl(list(prev_active, day, mun), 
                                          getActiveBetweenDates)) %>%
    ggplot(mapping = aes(x = day, 
                         y = active_cases)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Casos activos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_casos_activos.png"))
  
  ### Nuevos casos
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_cases)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Nuevos casos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_nuevos_casos.png"))
  
  ### Decesos
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_deaths)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Decesos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_decesos.png"))
  
  ### Casos activos por 100 mil hab
  pop_100 <- ppp$data %>% filter(municipality == mun) %>% 
    select(pop_100) %>% unlist(use.names = F)
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    mutate(prev_active = day - 14, 
           active_cases = purrr::pmap_dbl(list(prev_active, day, mun), 
                                          getActiveBetweenDates)) %>%
    ggplot(mapping = aes(x = day, 
                         y = active_cases/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Casos activos por 100 mil hab.") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_casos_activos_100k_hab.png"))
  
  ### Nuevos casos por 100 mil hab
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_cases/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Nuevos casos por 100 mil hab.") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_nuevos_casos_100k_hab.png"))
  
  ### Decesos
  xxx$data %>%
    filter(municipality == mun, 
           day > "2020-02-28") %>%
    ggplot(mapping = aes(x = day, 
                         y = daily_deaths/pop_100)) +
    geom_line(color="slategray2") + 
    geom_point(color="steelblue4") +
    ggtitle(mun) +
    ylab("Decesos") +
    xlab("") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    theme_minimal() 
  ggsave(paste0("figures/", mun, "_decesos_100k_hab.png"))
}

