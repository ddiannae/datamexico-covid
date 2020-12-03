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
  janitor::clean_names()

### Gráfica decesos por estados (solo porque sí) 
xxx$data %>%
  group_by(week = cut(day, "week"), state) %>%
  summarise(total_hosp  = sum(daily_hospitalized),
            total_cases = sum(daily_cases),
            total_deaths = sum(daily_deaths)) %>%
  filter(total_deaths > 0) %>%
  ggplot(mapping = aes(x = week, 
                       y = total_deaths, 
                       group = as.factor(state),
                       color = as.factor(state))) + 
  geom_line() + 
  xlab("Semana") +
  ylab("Decesos") +
  scale_color_manual(values = pals::glasbey(), name = "Estados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) 


### Obtención de factores por municipio
  ## Número máximo de casos
  xxx$data %>% 
    group_by(municipality_id) %>% 
    slice(which.max(daily_cases)) %>%
    select(municipality_id, daily_cases) %>% rename("max_cases" = daily_cases) %>%
left_join(
  ## Número máximo de hospitalizaciones
  xxx$data %>% 
    group_by(municipality_id) %>% 
    slice(which.max(daily_hospitalized)) %>% 
    select(municipality_id, daily_hospitalized) %>% rename("max_hospitalized" = daily_hospitalized),
  by = "municipality_id"
) %>% 
left_join(
  ## Número máximo de muertes
  xxx$data %>% 
    group_by(municipality_id) %>% 
    slice(which.max(daily_deaths)) %>% 
    select(municipality_id, daily_deaths) %>% rename("max_deaths" = daily_deaths),
  by = "municipality_id"
) %>%
left_join(
  ## Fecha de primer caso
  xxx$data %>% 
    filter(daily_cases > 0) %>%
    group_by(municipality_id) %>% 
    slice(which.min(day)) %>% 
    select(municipality_id, day) %>% rename("day_first_case" = day),
  by = "municipality_id"
) %>%
left_join(
  ## Fecha de primera muerte
  xxx$data %>% 
    filter(daily_deaths > 0) %>%
    group_by(municipality_id) %>% 
    slice(which.min(day)) %>% 
    select(municipality_id, day) %>% rename("day_first_death" = day),
  by = "municipality_id"
) %>%
left_join(
  ## Fecha de primeros 50 casos
  xxx$data %>% filter(accum_cases > 50) %>%
    group_by(municipality_id) %>%
    slice(which.min(day)) %>%
    select(municipality_id, day) %>% rename("day_50_cases" = day),
  by = "municipality_id"
) %>%
left_join(
  ## Fecha de primeras 10 muertes
  xxx$data %>% filter(accum_deaths > 10) %>%
    group_by(municipality_id) %>%
    slice(which.min(day)) %>%
    select(municipality_id, day) %>% rename("day_10_deaths" = day),
  by = "municipality_id"
) %>% left_join(
  xxx$data %>% 
    janitor::clean_names() %>%
    group_by(municipality_id, municipality) %>% 
    summarise(total_cases = sum(daily_cases),
              total_deaths = sum(daily_deaths),
              total_deaths = sum(total_deaths)),
  by = "municipality_id"
)  %>% select(municipality_id, municipality, everything()) %>% write_tsv("data/factores_covid_municipio.tsv")

### Casos diarios ajustados a poblacion
xxx$data <- xxx$data %>% left_join(ppp$data, by = c("municipality_id",  "municipality"))
xxx$data %>% mutate(daily_cases_adj = daily_cases/population) %>% 
  filter(time > "2020-02-28") %>%
  select(time, municipality_id, municipality, population, daily_cases_adj) %>% 
  pivot_wider(names_from = "time", values_from="daily_cases_adj") %>% write_tsv("data/casos_diarios_ajustados.tsv")
