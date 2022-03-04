library(dplyr)
library(lubridate)
library(ggplot2)
library(ggbeeswarm)
library(sf)
library(raster)
library(ggtext)
library(ragg)
library(pdftools)
library(rnaturalearth)


fuel_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') 

names(fuel_raw) <- tolower(names(fuel_raw))


fuel <- fuel_raw %>%  
  filter(fuel_type_code == "ELEC", state %in% datasets::state.abb) %>% 
  mutate(
    year = year(open_date)
  ) 

test <- fuel %>%
  group_by(state) %>% 
  mutate(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  dplyr::select(state, min_year, max_year) %>% 
  distinct() %>% 
  arrange(state)


first_electric_station <- ggplot(test, aes(x = min_year, y = state)) +
  geom_point() +
  xlim(c(1994, 2022)) +
  xlab("Year") +
  geom_curve(
    aes(x = 1997, xend = 1995, y = 20, yend = 6),
    arrow = arrow(length = unit(2, "mm")),
    colour = "black",
    size = 0.5,
    curvature = 0.4
  ) +
  geom_text(
    aes(
      x = 1998.5,
      y = 21
    ),
    label = "California",
    color = "black"
  ) +
  geom_curve(
    aes(x = 2017, xend = 2014.5, y = 11, yend = 1),
    arrow = arrow(length = unit(2, "mm")),
    colour = "black",
    size = 0.5,
    curvature = -0.4
  ) +
  geom_text(
    aes(
      x = 2017.5,
      y = 14
    ),
    label = "Arkansas",
    color = "black"
  ) +
  geom_curve(
    aes(x = 2016, xend = 2013, y = 42, yend = 49.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "black",
    size = 0.5,
    curvature = -0.4
  ) +
  geom_text(
    aes(
      x = 2018,
      y = 42
    ),
    label = "Wyoming",
    color = "black"
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank()
  )




fuel_state <- fuel %>% 
  group_by(state, year) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(state)
