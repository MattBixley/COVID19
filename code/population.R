# read population data from wiki
# https://www.worldometers.info/world-population/population-by-country/

library(rvest)
library(xml2)

world_url <- "https://www.worldometers.info/world-population/population-by-country/"

world_pop <- read_html(world_url) %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>% 
  rename(country = "Country (or dependency)", population = "Population (2020)", 
         density = "Density (P/Km²)", area = "Land Area (Km²)") %>% 
  select(country, population, density, area) %>% 
  map_df(str_replace_all, pattern = ",", replacement = "") %>% 
  map_df(str_remove, pattern = "%") %>% 
  mutate_at(vars(-country), as.numeric)
    
# population of states in china, australia and usa

usa <- "https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"
china <- "https://en.wikipedia.org/wiki/List_of_Chinese_administrative_divisions_by_population"

# Australia - ""
state = c("New South Wales", "Queensland", "Victoria", "Australian Capital Territory",
                                     "Northern Territory","South Australia","Tasmania","Western Australia")
population = c(8089525, 5095100, 6594804, 426709, 245869, 1751693, 534281, 2621680)
area = c(809952,1851736,237657, 2358, 1419630, 1044353, 90758, 2642753)
aus_pop <- tibble(state, population, area)
