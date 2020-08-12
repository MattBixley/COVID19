
# Import from the European CDC
ecdc_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/"
download.file(url = ecdc_url, destfile = "data/ecdc.csv")

covid19 <- read_csv("data/ecdc.csv") %>% 
  rename(date = "dateRep", country = "countriesAndTerritories", population = "popData2019") %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date, country) %>%
  
  # calculate cumulative values
  group_by(country) %>% 
  mutate(cum_cases = cumsum(cases)) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  ungroup() %>% 
  
  # tidy some country names
  mutate(country = ifelse(country == "United_States_of_America", "USA",country)) %>%   # per capita
  filter(country != "Cases_on_an_international_conveyance_Japan")  %>% 
  
  # calculate some percapita data
  mutate(population = ifelse(country == "China", 58500000/0.83, population)) %>% #correct pop to reflect hubei
  mutate(per_million = round(cum_cases/population*1000000,3)) %>%  # cumulative per million
  mutate(cases_million = round(cases/population*1000000,3)) # daily cases per million

