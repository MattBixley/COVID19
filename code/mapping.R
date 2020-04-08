install.packages("DHBins")
library(DHBins)
library(gganimate)
library(transformr)
library(gifski)
library(av)

nz_covid <- nz_moh %>% 
  count(DHB, date) %>% 
  pivot_wider(names_from = date, values_from = n, values_fill = list(n = 0)) %>% 
  pivot_longer(names_to = "date", values_to = "cases", cols = -DHB ) %>% 
  mutate(date = ymd(date)) %>% 
  arrange(DHB, date) %>% 
  group_by(DHB) %>% 
  mutate(cum_case = cumsum(cases)) %>% 
  ungroup()

p <- ggplot(nz_covid)+
  geom_dhb(aes(fill = cum_case, map_id=dhb_fixname(DHB))) +
  scale_fill_gradient(low = low, high = high, name = "Count") +
  geom_label_dhb(short=F,colour="black") +
  labs(title = "Cumulative Cases as at: {nz_covid$date[as.integer(frame)]}", caption = "Source: MOH") +
  theme_void() +
  theme(legend.position="right") +
  theme(title = element_text(size=16, face='bold')) +
  transition_manual(date)

# Video output
animate(
  p + enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out'),
  #renderer = av_renderer()
  fps = 5,
  width = 600, height = 800,
  renderer = gifski_renderer()
)

#anim_save("images/dhbcases.gif")

