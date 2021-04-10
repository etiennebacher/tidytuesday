library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)
library(gghighlight)
library(ggrepel)
library(WDI)

##########
## Treat data ##
##########

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)


## Get EU countries

eu_countries <- read.csv("https://raw.githubusercontent.com/jfjelstul/EU-infringement-data/master/data/member-state-years.csv") %>% 
  filter(member_state != "United Kingdom") %>% 
  pull(member_state) %>% 
  unique 


###### PAS LES BONNES DONNEES
clean_data <- tuesdata$forest_area %>% 
  filter(entity %in% eu_countries) %>% 
  group_by(year) %>% 
  mutate(
    highest_2020 = max(forest_area),
    highest_2020 = ifelse(year == 2020 & forest_area == highest_2020, 1, 0),
    lowest_2020 = min(forest_area),
    lowest_2020 = ifelse(year == 2020 & forest_area == lowest_2020, 1, 0),
  ) %>% 
  ungroup() %>% 
  group_by(entity) %>% 
  mutate(
    highest_2020 = ifelse(max(highest_2020) == 1, 1, 0),
    lowest_2020 = ifelse(max(lowest_2020) == 1, 1, 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    label = case_when(
      (entity == "France" | highest_2020 == 1 | lowest_2020 == 1)
      & year == 2020 ~ entity,
      TRUE ~ NA_character_
    )
  )

fr_color = "blue"
low_color = "red"
high_color = "darkgreen"

clean_data %>% 
  filter(entity == "France" | highest_2020 == 1 | lowest_2020 == 1) %>% 
  ggplot(aes(x = year, y = forest_area)) +
  geom_line(
    data = clean_data %>% 
      filter(entity != "France" | highest_2020 == 0 | lowest_2020 == 0),
    aes(group = entity),
    color = "gray"
  ) +
  geom_line(
    data = clean_data %>% 
      filter(entity == "France"),
    color = fr_color
  ) +
  geom_line(
    data = clean_data %>% 
      filter(highest_2020 == 1),
    color = high_color
  ) +
  geom_line(
    data = clean_data %>% 
      filter(lowest_2020 == 1),
    color = low_color
  ) +
  
  ### Labels on the right
  
  geom_curve(
    aes(x = 2020, xend = 2022, y = 0.425, yend = 0.425),
    curvature = 0,
    size = 0.1,
    linetype = "dashed",
    color = fr_color
  ) +
  annotate(
    "text",
    x = 2022.7, y = 0.425,
    label = "France",
    color = fr_color
  ) +
  
  geom_curve(
    aes(x = 2020, xend = 2022, y = 0, yend = 0.3),
    curvature = 0,
    size = 0.1,
    linetype = "dashed",
    color = low_color
  ) +
  annotate(
    "text",
    x = 2022.7, y = 0.3,
    label = clean_data %>% 
      filter(lowest_2020 == 1, year == 2020) %>% 
      pull(label),
    color = low_color
  ) +
  
  geom_curve(
    aes(x = 2020, xend = 2022, y = 0.689, yend = 0.55),
    curvature = 0,
    size = 0.1,
    linetype = "dashed",
    color = high_color
  ) +
  annotate(
    "text",
    x = 2022.7, y = 0.55,
    label = clean_data %>% 
      filter(highest_2020 == 1, year == 2020) %>% 
      pull(label),
    color = high_color
  ) +
  xlim(c(1990, 2024)) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    plot.background = element_blank()
  )
