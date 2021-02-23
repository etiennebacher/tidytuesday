library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gganimate)
library(tidytuesdayR)


tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

color_background <- "#99ccff"
color_background_2 <- "#0059b3"

plot1 <-
  tuesdata$earn %>% 
  filter(race == "All Races", ethnic_origin == "All Origins",
         age == "16 years and over") %>% 
  select(sex, year, quarter, median_weekly_earn) %>% 
  unite(col = "period", year, quarter, sep = "-") %>% 
  mutate(sex = recode(sex, 
                      "Both Sexes" = "both", 
                      "Women" = "women", 
                      "Men" = "men"),
         sex = as.factor(sex),
         period = ym(period)) %>% 
  ggplot(aes(x = period, y = median_weekly_earn, group = sex)) +
  geom_line(aes(linetype = sex), color = color_background_2) +
  labs(
    x = "Time",
    y = "Median weekly earning ($)",
    title = "Evolution of men's and women's weekly earnings\n\n Year: {frame_along}",
    caption = "Made by Etienne Bacher"
  ) +
  ylim(c(500, 1100)) +
  transition_reveal(period) +
  ease_aes('linear') +
  theme(
    ### Legend
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(color = color_background, 
                                     fill = color_background),
    legend.key = element_rect(color = color_background, 
                              fill = color_background),
    legend.text = element_text(colour = color_background_2, size = 11),
    
    ### Background
    panel.background = element_rect(color = color_background,
                                   fill = color_background),
    plot.background = element_rect(color = color_background_2,
                                   fill = color_background_2),
    
    ### Text
    axis.title.x = element_text(colour = "white", size = 13),
    axis.title.y = element_text(colour = "white", size = 13),
    axis.text.x  = element_text(colour = "white", size = 11),
    axis.text.y  = element_text(colour = "white", size = 11),
    plot.title = element_text(colour = "white", hjust = 0.5, size = 15),
    plot.caption = element_text(colour = "white", size = 11),
    
    ### Graduations
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  ) 

animate(plot1, end_pause = 20, width = 700, duration = 25)
