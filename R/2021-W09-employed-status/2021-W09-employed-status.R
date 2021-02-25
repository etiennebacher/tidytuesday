library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gganimate)
library(tidytuesdayR)
library(systemfonts)


tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

color_background <- "#99ccff"
color_background_2 <- "#0059b3"

plot1 <-
  
  ### Play with the data
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
         period = yq(period)) %>% 
  
  ### Base plot
  ggplot(aes(x = period, y = median_weekly_earn, group = sex)) +
  geom_line(aes(linetype = sex), color = color_background_2) +
  scale_linetype_discrete(labels=c("Both", "Men", "Women")) +
  labs(
    x = "Time",
    y = "Median weekly earning ($)",
    title = "Evolution of men's and women's weekly earnings\n\n Year: {substr(frame_along, 1, 7)}",
    caption = "Made by Etienne Bacher\n Data from US Bureau of Labor Statistics"
  ) +
  ylim(c(500, 1100)) +
  
  ### Customize the plot
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
    text = element_text(family = "Times"),
    axis.title.x = element_text(colour = "white", size = 13),
    axis.title.y = element_text(colour = "white", size = 13),
    axis.text.x  = element_text(colour = "white", size = 11),
    axis.text.y  = element_text(colour = "white", size = 11),
    plot.title = element_text(colour = "white", hjust = 0.5, size = 17),
    plot.caption = element_text(colour = "white", size = 11),
    
    ### Graduations
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  
  ### Animate the plot
  transition_reveal(period) +
  ease_aes('linear')

animate(plot1, end_pause = 20, width = 700, duration = 44, nframes = 66,
        width = 1000, height = 600, detail = 5,
        renderer = gifski_renderer("R/2021-W09-employed-status/employed_status.gif"))
