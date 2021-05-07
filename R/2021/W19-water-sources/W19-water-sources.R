library(data.table)
library(forcats)
library(ggplot2)
library(tidytuesdayR)
library(pdftools)


###########################
## Treat data and make plot ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

water <- as.data.table(tuesdata$water)

color_earth <- "#663300"

water[grepl("Well", water_source)] %>% 
  .[, .(.N), by = country_name] %>% 
  .[order(-N)] %>% 
  .[1:10] %>% 
  as_tibble() %>% 
  ggplot(aes(x = fct_reorder(country_name, N), y = N)) +
  geom_bar(stat='identity') +
  scale_y_continuous(trans = "reverse") +
  geom_text(
    aes(label = N), 
    position = position_dodge(width = 0.9), 
    vjust = 1.5,
    color = "white"
  ) +
  scale_x_discrete(position = "top") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = color_earth),
    panel.background = element_rect(fill = color_earth),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "white", linetype = "dashed")
  )
