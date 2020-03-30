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
  geom_point(data = plotdata %>% filter(date == max(date)), 
             aes(x = per_million, y = cases_million, size = 1, colour = "blue" )) +
  geom_text(data =  plotdata %>% filter(date == max(date)), 
            aes(label=country),hjust=0, vjust=0, size = 4, colour = "blue") +

  # overplot NZ
  geom_point(data = nzdat, aes(x = per_million, y = cases_million), size = 3, colour = "red") +
  geom_line(data = nzdat, aes(x = per_million, y = cases_million), size = 1.5, colour = "red") +
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

p4 + theme_classic() + 
  theme(legend.position=NULL) +
  theme(axis.line = element_line(colour = "grey50")) + # plot axis to grey
  theme(panel.grid.major = element_line(colour = "grey40", size = 0.4)) + # grid to grey
  theme(panel.grid.minor = element_line(colour = "grey70", size = 0.3)) + # grid to grey
  labs(caption = "Matt Bixley") + theme(legend.position="none")
