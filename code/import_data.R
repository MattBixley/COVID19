# Johns Hiopkins Dataset

jhu_confirmed <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                       "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                       "time_series_covid19_confirmed_global.csv", sep = "")

jhu_deaths <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                    "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                    "time_series_covid19_deaths_global.csv", sep = "")

# Download
download.file(url = jhu_confirmed, destfile = "data/jhu_confirmed.csv")
download.file(url = jhu_deaths, destfile = "data/jhu_deaths.csv")

cases <- read_csv(jhu_confirmed) %>% rename(state = "Province/State", country = "Country/Region") %>% 
  pivot_longer(-c(state, country, Lat, Long), names_to = "Date", values_to = "cumulative_cases") %>% 
  mutate(Date = mdy(Date)) %>%
  
  # cumultaive cases by province and country
  group_by(country,state) %>% 
  mutate(cases = cumulative_cases - lag(cumulative_cases)) %>% 
  group_by(country, Date) %>%
  mutate(country_cum_cases = sum(cumulative_cases)) %>% 
  ungroup() %>% 
  mutate(country_cases = country_cum_cases - lag(country_cum_cases)) %>% 
  
  # clean up the NA and negative values
  mutate(country_cases = ifelse(country_cases < 0, 0, country_cases)) %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases ))

deaths <- read_csv(jhu_deaths) %>% rename(state = "Province/State", country = "Country/Region") %>% 
  pivot_longer(-c(state, country, Lat, Long), names_to = "Date", values_to = "cumulative_deaths") %>% 
  mutate(Date = mdy(Date)) %>%
  
  # cumultaive deaths by province and country
  group_by(country,state) %>% 
  mutate(deaths = cumulative_deaths - lag(cumulative_deaths)) %>% 
  group_by(country, Date) %>%
  mutate(country_cum_deaths = sum(cumulative_deaths)) %>% 
  ungroup() %>% 
  mutate(country_deaths = country_cum_deaths - lag(country_cum_deaths)) %>% 
  
  # clean up the NA and negative values
  mutate(country_deaths = ifelse(country_deaths < 0, 0, country_deaths)) %>% 
  mutate(deaths = ifelse(is.na(deaths), 0, deaths ))

