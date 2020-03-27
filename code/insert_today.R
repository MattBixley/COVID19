# function to insert todays values for NZ

insert_today <- function(cases = 0, deaths = 0){
  insert <- covid19 %>% filter(country == "New_Zealand")
  insert <- insert %>% filter(date == max(date))
  insert$date <- insert$date + 1
  insert$day <- insert$day + 1
  insert$cases <- cases
  insert$deaths <- deaths
  insert$cum_cases <- insert$cum_cases + cases
  insert$cum_deaths <- insert$cum_deaths + deaths
  newcovid <- rbind(covid19, insert) %>% 
  arrange(date, country)
  return(newcovid)
}

#covid19 <- insert_today(cases = 85, deaths = 0) %>% filter(country == "New_Zealand")
