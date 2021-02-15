library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(ggrepel)
library(glue)
library(ggtext)
library(pdftools)
library(extrafont)

### Try to reproduce https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge01/original-plate-07.jpg

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

# extrafont::font_import("/home/etienne/Téléchargements/Orbitron/")

tuesdata$georgia_pop %>%
  pivot_longer(
    cols = !Year,
    names_to = "Color",
    values_to = "Percent"
  ) %>% 
  ggplot(aes(x = Year, y = Percent, linetype = Color)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "longdash"), labels = c("COLORED", "WHITE")) +
  scale_x_continuous(breaks = seq(1790, 1890, 10), expand = c(0, 0)) +
  scale_y_continuous(trans = "reverse", breaks = seq(0, 100, 5), expand = c(0, 0)) +
  coord_flip() +
  theme(
    # text = element_text(family = "Orbitron-ExtraBold"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ffc7b3"),
    panel.grid.major.y = element_line(color = "#ffc7b3"),
    plot.background = element_rect(fill = "#f7ddbb"),
    panel.background = element_rect(fill = "#f7ddbb", colour = "#808080"),
    plot.title = element_text(hjust = 0.5, size = "20"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(colour = "#808080"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7ddbb"),
    legend.key = element_rect(fill = "#f7ddbb"),
    legend.text = element_text(margin = margin(r = 17, unit = "cm")),
    legend.key.width = unit(2.3, "cm"),
    legend.box.margin = margin(l = 17, unit = "cm"),
    plot.margin = margin(l = 5, r = 5, unit = "cm")
  ) +
  labs(title = toupper("\ncomparative increase of white and colored\npopulation in georgia.\n\n")) 
