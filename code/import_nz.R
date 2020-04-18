### function to insert todays values for NZ

# insert_today <- function(nzcases = 0, nzdeaths = 0){
#   insert <- covid19 %>% filter(country == "New_Zealand", date == max(date)) %>% 
# 
#     # add date values
#     mutate(date = date + 1) %>% 
#     mutate(day = day(date), month = month(date), year = year(date)) %>% 
#     
#     # add the cases
#     mutate(cases = nzcases, deaths = nzdeaths) %>% 
#     mutate(cum_cases = cum_cases + nzcases, cum_deaths = cum_deaths + nzdeaths) %>% 
#     mutate(per_million = round(cum_cases/population*1000000,1)) %>%
#     mutate(cases_million = round(cases/population*1000000,1))
#   
# 
#   
#   newcovid <- rbind(covid19, insert) %>% 
#   arrange(date, country)
#   return(newcovid)
# }

#covid19 <- insert_today(cases = 85, deaths = 0) %>% filter(country == "New_Zealand")


### import the MOH data directly to add to the ECDC dataset
nz_moh <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details"

library(rvest)
library(xml2)
library(lubridate)

nz_moh_cases <- read_html(nz_moh) %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>% # table one is the confirmed
  mutate(cases = "confirmed")

nz_moh_probable <- read_html(nz_moh) %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[[2]] %>% # ttable 3 is the probables
  mutate(cases = "probable")

nz_march <- read_csv("data/nz_moh_march.csv") %>% 
  mutate(date = dmy(date), flight_date = dmy(flight_date))

nz_moh <- bind_rows(nz_moh_cases, nz_moh_probable) %>% 
  rename(date = "Date of report", last_country = "Last country before return", flight = "Flight number",
         flight_date = "Arrival date", age_group = "Age group", travel = "Overseas travel", 
         flight_dep_date = "Flight departure date") %>% 
  
  # deal with flights and merge to one date, last known date
  mutate(flight_date = ifelse(is.na(flight_date), flight_dep_date, flight_date)) %>% 
  mutate(date = dmy(date), flight_date = dmy(flight_date)) %>%
  select(-flight_dep_date) %>% 
  
  # hack to push the ovelap cases back one day
  #mutate(date = ifelse(date == max(date),  max(date) - 1 , date )) %>%
  #mutate(date = as_date(date)) %>% 
  arrange(date) %>% 
  bind_rows(.,nz_march)
  

write_csv(nz_moh, "data/nz_moh.csv")

# reconstruct the ecdc data for NZ
nz_moh_short <- nz_moh %>% 
  count(date) %>% 
  rename(cases = "n") %>% 
  mutate(cum_cases = cumsum(cases))

nzcdc <- tibble(date = seq(min(covid19$date), max(covid19$date), 1),
  day = day(date),
  month = month(date),
  year = year(date),
  country = "New_Zealand",
  geoId = "NZ",
  countryterritoryCode = "NZL",
  population = 4885500) %>% 
  left_join(., nz_moh_short, by = "date") %>% 
  mutate_at(vars(cases), funs(replace_na(., 0))) %>% 
  mutate(per_million = round(cum_cases/population*1000000,3)) %>%  # cumulative per million
  mutate(cases_million = round(cases/population*1000000,3)) %>% # daily cases per million
  mutate(deaths = 0, cum_deaths = 0) #%>% 
  #filter(date != max(date))
  
covid19 <- covid19 %>% filter(country != "New_Zealand") %>% 
  bind_rows(.,nzcdc)

       