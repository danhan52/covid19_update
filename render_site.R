setwd('C:/Users/danny/repos/covid19_update/')

source('load_data.R')
dir.create('data')
save(
  covid_global, covid_us,
  file = 'data/covid_ts.RData'
)
save(
  today_global, today_global_m5, today_states, today_counties,
  file = 'data/covid_today.RData'
)
save(
  geo_countries, geo_states, geo_counties,
  file = 'data/geojson.RData'
)

# rmarkdown::clean_site()
file.remove(file.path('docs', dir('docs')))
file.remove('docs')
rmarkdown::render_site()
