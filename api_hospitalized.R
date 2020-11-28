library(tidyverse)
library(vroom)
library(lubridate)
library(jsonlite)

my_url <- "https://dev-api.datamexico.org/tesseract/cubes/gobmx_covid_stats_mun/aggregate.jsonrecords?drilldowns%5B%5D=Reported+Date.Time.Day&drilldowns%5B%5D=Geography.National+Urban+System.National+Urban+System&measures%5B%5D=Daily+Cases&measures%5B%5D=Daily+Hospitalized&parents=false&sparse=false"
xxx <- jsonlite::fromJSON(txt = my_url)
xxx$data %>% head

xxx$data %>% 
  janitor::clean_names() %>% 
  group_by(national_urban_system) %>% 
  mutate(total_hosp  = sum(daily_hospitalized),
         total_cases = sum(daily_cases)) %>% 
  filter(total_cases >100000) %>% 
  filter(day > max(day)-15) %>% 
  ggplot(mapping = aes(x=day, 
                       y=daily_hospitalized, 
                       colour = as.factor(national_urban_system)
  )
  ) + 
  geom_line() + 
  theme_minimal() #+ 
#theme(legend.position = "none")
