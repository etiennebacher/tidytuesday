library(dplyr)
library(tidyr)
library(ebmisc)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

data_clean <- tuesdata$games %>% 
  mutate(
    month_decimal = as.numeric(ebmisc::month_to_number(month)) * 8/100,
    x_axis = year + month_decimal
  ) %>% 
  group_by(x_axis) %>% 
  mutate(tot_players = sum(avg, na.rm = T)) %>% 
  select(x_axis, year, tot_players) %>% 
  distinct()

options(scipen = 999)

data_clean %>% 
  ggplot(aes(x = x_axis, y = tot_players)) +
  geom_line(color = "#e69900") +
  scale_x_continuous(breaks = seq(2012, 2021, 2)) +
  scale_y_continuous(labels = paste0(seq(0, 5, 1), "M"), 
                     limits = c(0, 5000000)) +
  labs(
    y = "Average number of players\n",
    title = "Average number of players simultaneously playing on Steam"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "#e69900"),
    axis.text.x = element_text(color = "#e69900"),
    axis.text.y = element_text(color = "#e69900"),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#ffeecc"),
    plot.background = element_rect(fill = "#ffeecc"),
    panel.grid.major = element_line(color = "#ffe6b3"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#ffe6b3"),
    plot.title = element_text(hjust = 0.5, color = "#e69900",
                              size = 15)
  ) +
  geom_curve(
    aes(x = 2016.3, xend = 2017.2, y = 3500000, yend = 2400000),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = -0.4
  ) +
  geom_curve(
    aes(x = 2020.1, xend = 2020.25, y = 1500000, yend = 3350000),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate(
    "text",
    x = 2015,
    y = 3500000,
    label = paste0('Release of "Player', "'s \nUnknown Battlegrounds", '"'),
    color = "#e69900"
  ) +
  annotate(
    "text",
    x = 2019.8,
    y = 1100000,
    label = "Start of anti-Covid \nmeasures",
    color = "#e69900"
  ) +
  annotate(
    "text",
    x = 2020.32,
    y = 4680000,
    label = "4.527M",
    color = "#e69900"
  ) +
  annotate(
    "text",
    x = 2018.08,
    y = 4250000,
    label = "4.102M",
    color = "#e69900"
  )
  
  
  
  
