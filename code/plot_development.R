# Plot development

# plot themes
mytheme <- theme_classic() + 
  theme(legend.position="bottom") +
  theme(axis.line = element_line(colour = "grey50")) + # plot axis to grey
  theme(panel.grid.major = element_line(colour = "grey40", size = 0.4)) + # grid to grey
  theme(panel.grid.minor = element_line(colour = "grey70", size = 0.3)) + # grid to grey
  theme(panel.grid.major.x = element_blank()) + #remove the vertical grid lines 
  theme(panel.grid.minor.x = element_blank()) #remove the vertical grid lines


# plot nz growth
#who <- "New Zealand"
who <- c("New_Zealand", "Australia", "USA", "Italy", "China", "South_Korea", "Spain", "France", "Germany", "United_Kingdom")
fiveper <-  covid19 %>% filter(country %in% who, per_million > 5) %>%
  group_by(country) %>% 
  mutate(days5 = dense_rank(date))
fivenz <- fiveper %>% filter(country == "New_Zealand")

p1 <- fiveper %>% 
  ggplot(aes(x = days5, y = per_million, colour = country)) +
  geom_point() +
  geom_line() +
  labs(title = "Per Capita Cumulative Cases",
       subtitle = "days since 5 cases per million",
       x = "Days", y = "Cases\n(per Million People)") +
  scale_y_continuous(breaks = seq(0, 10000, 500)) +
  # overplot NZ
  geom_point(data = fivenz, aes(x = days5, y = per_million, colour = country), size = 3) +
  geom_line(data = fivenz, aes(x = days5, y = per_million, colour = country))

p1 + mytheme + labs(caption = "Matt Bixley") + 
  scale_colour_brewer(palette = "Paired")


# log log of percapita nz growth
# gganimate, should show fall off in incidence when effective measure take over

who <- c("Australia", "USA", "Italy", "China", "South_Korea", "Spain", "France", "Germany", "United_Kingdom")

nzdat <- covid19 %>% filter(country == "New_Zealand", cases > 0)
plotdata <- covid19 %>% filter(country %in% who, cases_million < 2000, cases > 0, population > 4800000)

p4 <-  plotdata %>% 
 
  ggplot(aes(x = per_million, y = cases_million, group = country)) +
  geom_point(col = "grey10") +
  geom_line(col = "grey10") +
  labs(title = "log log cases",
       subtitle = "",
       x = "log of Cumulative Cases\n(per Million)", y = "log of Daily Cases\n(per Million)") +
  
  
  # label the last point
  geom_point_interactive(data = plotdata %>% filter(date == max(date)), 
             aes(x = per_million, y = cases_million, size = 1, colour = "blue", tooltip = country)) +
  geom_text(data =  plotdata %>% filter(date == max(date)), 
            aes(label=country),hjust=0, vjust=0, size = 4, colour = "blue") +

  # overplot NZ
  geom_point(data = nzdat, aes(x = per_million, y = cases_million), size = 3, colour = "red") +
  geom_line(data = nzdat, aes(x = per_million, y = cases_million), size = 1.5, colour = "red") +
  geom_text(data = nzdat %>% filter(date == max(date)), aes(label=country),hjust = 0, vjust = 0, size = 5, colour = "red") +
  
  # log scales
  scale_y_continuous(trans = "log10", breaks = c(0, 1, 10, 100,1000),
                     minor_breaks = c(seq(0,1,0.1),
                                      seq(1,10,1),
                                      seq(10,100,10),
                                      seq(100,1000,100))) +
  scale_x_continuous(trans = "log10", labels = c(0, 10, 100, 1000,10000),
                     breaks = c(0, 10, 100, 1000,10000),
                     minor_breaks = c(seq(0, 10,1),
                                      seq(0, 100, 10),
                                      seq(100, 1000, 100),
                                      seq(1000, 10000, 1000)))

p4 + theme_classic() + 
  theme(legend.position=NULL) +
  theme(axis.line = element_line(colour = "grey50")) + # plot axis to grey
  theme(panel.grid.major = element_line(colour = "grey40", size = 0.4)) + # grid to grey
  theme(panel.grid.minor = element_line(colour = "grey70", size = 0.3)) + # grid to grey
  labs(caption = "Matt Bixley") + theme(legend.position="none")

girafe(ggobj = p4 + theme_classic() + 
         theme(legend.position=NULL) +
         theme(axis.line = element_line(colour = "grey50")) + # plot axis to grey
         theme(panel.grid.major = element_line(colour = "grey40", size = 0.4)) + # grid to grey
         theme(panel.grid.minor = element_line(colour = "grey70", size = 0.3)) + # grid to grey
         labs(caption = "Source: ECDC") + theme(legend.position="none")
)

library(ggplot2)
library(ggiraph)
data <- mtcars
data$carname <- row.names(data)

gg_point = ggplot(data = data) +
  geom_point_interactive(aes(x = wt, y = qsec, color = disp,tooltip = disp)) + 
  theme_minimal()

girafe(ggobj = gg_point)


### NZ Cases Barchart
p5 <- covid19 %>% 
  filter(country == "New_Zealand") %>% 
  ggplot(aes(x = date, y = cases, fill = "brown")) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "New Zealand", subtitle = "Cases/Day",
       y = "Cases", x = "Date")

p5 + mytheme + labs(caption = "Source: ECDC") + theme(legend.position="none")
  
### europe plot
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.position = "bottom")

today = format(Sys.time()-24*60*60, "%Y-%m-%d")

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe")

europecov <- europe %>% inner_join(.,covid19, by = c("adm0_a3" = "countryterritoryCode")) %>% 
  filter(per_million < 6000, date == today)

p6 <- ggplot(data = europecov) +
  labs(title = "European density of COVID19",
       fill = "Cumulative Cases at \n(per Million)") +
  geom_sf(aes(fill = per_million)) +
  scale_fill_gradient(low = "#FDD0A2", high = "#D94801") +
  coord_sf(xlim = c(-30, 80), ylim = c(35, 85), expand = FALSE) # limits in the sf() object

p6 + theme_classic() + plain

library(tidyverse)  
library(gganimate)

# log log of percapita nz growth
# gganimate, should show fall off in incidence when effective measure take over

who <- c("Australia", "USA", "Italy", "China", "South_Korea", "Spain", "France", "Germany", "United_Kingdom", "Taiwan", "Singapore")

nzdat <- covid19 %>% filter(country == "New_Zealand", cases > 0)
plotdata <- covid19 %>% filter(country %in% who, cases_million < 2000, cases > 0, population > 4800000)

p4 <-  plotdata %>% 
  
  ggplot(aes(x = per_million, y = cases_million, group = country)) +
  geom_point(col = "grey30", alpha = 0.3) +
  geom_line(col = "grey30", alpha = 0.3) +
  labs(title = "Beating the Curve",
       subtitle = "When restrictions take affect",
       x = "log of Cumulative Cases\n(per Million)", y = "log of Daily Cases\n(per Million)") +
  
  
  # label the last point
  #geom_point(data = plotdata %>% filter(date == max(date)), 
  #           aes(x = per_million, y = cases_million)) +
  geom_text(data =  plotdata %>% filter(date == max(date)), 
            aes(label=country),hjust=0, vjust=0, size = 3, size = 3, colour = "blue") +
  
  # overplot NZ
  geom_point(data = nzdat, aes(x = per_million, y = cases_million), size = 3, colour = "red", alpha = 0.5) +
  geom_line(data = nzdat, aes(x = per_million, y = cases_million), size = 1.5, colour = "red", alpha = 0.5) +
  geom_text(data = nzdat %>% filter(date == max(date)), aes(label=country),hjust=0, vjust=0, size = 5, colour = "red") +
  
  # log scales
  scale_y_continuous(trans = "log10", breaks = c(0, 1, 10, 100,1000),
                     minor_breaks = c(seq(0,1,0.1),
                                      seq(1,10,1),
                                      seq(10,100,10),
                                      seq(100,1000,100))) +
  scale_x_continuous(trans = "log10", labels = c(0, 10, 100, 1000,10000),
                     breaks = c(0, 10, 100, 1000,10000),
                     minor_breaks = c(seq(0, 10,1),
                                      seq(0, 100, 10),
                                      seq(100, 1000, 100),
                                      seq(1000, 10000, 1000)))

p <- p4 + theme_classic() + 
  theme(legend.position=NULL) +
  theme(axis.line = element_line(colour = "grey50")) + # plot axis to grey
  theme(panel.grid.major = element_line(colour = "grey40", size = 0.4)) + # grid to grey
  theme(panel.grid.minor = element_line(colour = "grey70", size = 0.3)) + # grid to grey
  labs(caption = "Source: ECDC") + theme(legend.position="none")





p + transition_time(per_million) 



library(gganimate)
#> Loading required package: ggplot2

# We'll start with a static plot
p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(p)
anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim


# 2 day average
library(slider)
covid19 %>% filter(country == "Australia") %>% 
  mutate(cases3 = slide_dbl(cases, mean, .before = 2)) %>% 
  select(country, date, cases, cases3) %>% 
  ggplot(aes(x = date, y = cases3)) +
  geom_bar(stat = "identity")
