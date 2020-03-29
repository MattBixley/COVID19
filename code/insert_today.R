# function to insert todays values for NZ

insert_today <- function(nzcases = 0, nzdeaths = 0){
  insert <- covid19 %>% filter(country == "New_Zealand", date == max(date)) %>% 

    # add date values
    mutate(date = date + 1) %>% 
    mutate(day = day(date), month = month(date), year = year(date)) %>% 
    
    # add the cases
    mutate(cases = nzcases, deaths = nzdeaths) %>% 
    mutate(cum_cases = cum_cases + nzcases, cum_deaths = cum_deaths + nzdeaths) %>% 
    mutate(per_million = round(cum_cases/population*1000000,1)) %>%
    mutate(cases_million = round(cases/population*1000000,1))
  

  
  newcovid <- rbind(covid19, insert) %>% 
  arrange(date, country)
  return(newcovid)
}

#covid19 <- insert_today(cases = 85, deaths = 0) %>% filter(country == "New_Zealand")


