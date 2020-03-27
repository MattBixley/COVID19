
# Import from the European CDC
ecdc_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/"
download.file(url = ecdc_url, destfile = "data/ecdc.csv")

read_csv("data/ecdc.csv") %>% 
  rename(date = "dateRep", country = "countriesAndTerritories", population = "popData2018") %>% 
  left_join(.,jhu) %>% # merge with jhu to get the lat long
  mutate(date = dmy(date)) %>% 
  arrange(date, country) %>% 
  mutate(population = ifelse(country == "China", 58500000/0.83, population)) %>% #correct pop to reflect hubei
  group_by(country) %>% 
  mutate(cum_cases = cumsum(cases)) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  ungroup() %>% 
  mutate(country = ifelse(country == "United_States_of_America", "USA",country)) %>%   # per capita
  filter(country != "Cases_on_an_international_conveyance_Japan")
