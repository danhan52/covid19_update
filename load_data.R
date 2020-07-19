library(tidyverse)
library(lubridate)
library(rvest)

# US data
us_pop <-
  read_csv(url(paste0('https://www2.census.gov/programs-surveys/popest/',
                      'datasets/2010-2019/counties/totals/',
                      'co-est2019-alldata.csv'))) %>%
  rename_all(tolower) %>%
  mutate(fips = paste0(state, county)) %>%
  select(fips, popestimate2019)

covid_us <-
  read_csv(url(paste0('https://raw.githubusercontent.com/nytimes/',
                      'covid-19-data/master/us-counties.csv'))) %>%
  left_join(us_pop, by = c('fips')) %>%
  left_join(tibble(state.abb, state.name), by = c('state'='state.name')) %>%
  group_by(county, state) %>%
  mutate(
    new_cases = cases - lag(cases, default = 0, order_by = date),
    new_deaths = deaths - lag(deaths, default = 0, order_by = date)
  ) %>%
  ungroup() %>%
  filter(new_cases > -100, new_deaths > -100)

geo_counties <-
  rjson::fromJSON(file = paste0('https://raw.githubusercontent.com/plotly/',
                                'datasets/master/geojson-counties-fips.json'))

geo_states <-
  rjson::fromJSON(file = paste0('https://raw.githubusercontent.com/rstudio/',
                                'leaflet/gh-pages/json/us-states.geojson'))

# global data
world_pop <-
  read_html('https://worldpopulationreview.com/') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[1]] %>%
  as_tibble() %>%
  mutate(
    country = Country,
    cntry_pop = as.numeric(str_remove_all(`2019 Population`, ','))
  ) %>%
  select(country, cntry_pop)

covid_global <- 
  c(confirmed = 'confirmed', deaths = 'deaths') %>%
  map(
    function(x) {
      paste0(
        'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
        'master/csse_covid_19_data/csse_covid_19_time_series/',
        'time_series_covid19_',
        x,
        '_global.csv'
      )
    }
  ) %>%
  map_df(function(x) read_csv(url(x)), .id = 'metric') %>%
  pivot_longer(cols = `1/22/20`:last_col(),
               names_to = 'date',
               values_to = 'people') %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  pivot_wider(names_from = metric, values_from = people) %>%
  group_by(country = `Country/Region`, date) %>%
  summarize(
    cases = sum(confirmed, na.rm = T),
    deaths = sum(deaths, na.rm = T)
  ) %>%
  left_join(world_pop, by = 'country') %>%
  filter(cases > 0 | deaths > 0) %>%
  group_by(country) %>%
  mutate(
    new_cases = cases - lag(cases, default = 0, order_by = date),
    new_deaths = deaths - lag(deaths, default = 0, order_by = date)
  ) %>%
  ungroup() %>%
  mutate(country = if_else(country == 'US', 'United States of America', country)) %>%
  filter(new_cases > -1000, new_deaths > -100)

geo_countries <- 
  rjson::fromJSON(file = paste0('https://raw.githubusercontent.com/AshKyd/',
                                'geojson-regions/master/countries/110m/',
                                'all.geojson'))

for (i in 1:length(geo_countries$features)) {
  geo_countries$features[[i]]$id <- geo_countries$features[[i]]$properties$admin
}

today_global <-
  covid_global %>%
  filter(date == max(date, na.rm = T))

today_states <- 
  covid_us %>%
  group_by(date, state, state.abb) %>%
  summarize(
    cases = sum(cases, na.rm = T),
    deaths = sum(deaths, na.rm = T),
    new_cases = sum(new_cases, na.rm = T),
    new_deaths = sum(new_deaths, na.rm = T),
    pop = sum(popestimate2019, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(date == max(date, na.rm = T)) %>%
  left_join(
    geo_states$features %>%
      map_df(function(x) tibble(id = x$id, state = x$properties$name)),
    by = c('state')
  )

today_counties <- 
  covid_us %>%
  filter(date == max(date, na.rm = T))
