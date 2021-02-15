library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(ggrepel)
library(glue)
library(ggtext)
library(pdftools)

### Try to reproduce https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge01/original-plate-07.jpg

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

tuesdata$georgia_pop %>%
  pivot_longer(
    cols = !Year,
    names_to = "Color",
    values_to = "Percent"
  ) %>% 
  ggplot(aes(x = Year, y = Percent, linetype = Color)) +
  geom_line() +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(1790, 1890, 10)) +
  scale_y_continuous(trans = "reverse", breaks = seq(0, 100, 5)) +
  coord_flip() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ffc7b3"),
    panel.grid.major.y = element_line(color = "#ffc7b3"),
    plot.background = element_rect(fill = "#f7ddbb"),
    panel.background = element_rect(fill = "#f7ddbb"),
    plot.title = element_text(hjust = 0.5, size = "20")
  ) +
  labs(title = toupper("comparative increase of white and colored\npopulation in georgia.\n\n"))
