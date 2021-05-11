library(data.table)
library(tibble)
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
  labs(
    title = "Which African countries have the most wells?\n",
    caption = "\nMade by Etienne Bacher | Data from Water Point Data Exchange"
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "white"),
    plot.background = element_rect(fill = color_earth),
    panel.background = element_rect(fill = color_earth),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "white", linetype = "dashed"),
    plot.title = element_text(hjust = 0.5, size = 20, color = "white"),
    plot.subtitle = element_text(hjust = 0.5, color = "white"),
    plot.caption = element_text(color = "white"),
    text = element_text(family = "Carlito", size = 13)
  ) 


###########################
## Export ##
###########################

ggsave("R/2021/W19-water-sources/water-sources.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W19-water-sources/water-sources.pdf", 
            filenames = "R/2021/W19-water-sources/water-sources.png",
            format = "png", dpi = 350) 
