library(dplyr)
library(lubridate)
library(ggplot2)
library(ggbeeswarm)
library(geojsonio)
library(sf)
library(raster)
library(broom)
library(ggtext)
library(rgeos)
library(ragg)
library(pdftools)
library(rnaturalearth)


fuel_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') 

names(fuel_raw) <- tolower(names(fuel_raw))
us_states <- data.frame(id = datasets::state.abb, name = datasets::state.name)

fuel <- fuel_raw %>%  
  filter(fuel_type_code == "ELEC", state %in% datasets::state.abb) %>% 
  mutate(
    year = year(open_date)
  ) %>% 
  left_join(us_states, c("state" = "id"))

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




##########
## Make plot for first station with electric per state ##
##########

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
    label = "Alaska",
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




##########
## Maks US hex map ##
##########

fuel_state <- fuel %>% 
  filter(year == 2021) %>% 
  group_by(state, name) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(state)


map_hex <- geojson_read(
  "C:\\Users\\etienne\\Desktop\\Divers\\TidyTuesday\\R\\2022\\W09-alternative-fuel-stations\\us_states_hexgrid.geojson.json",
  what = "sp"
)

map_hex@data <- map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
map_hex_fortified <- tidy(map_hex, region = "google_name") %>% 
  left_join(fuel_state, c("id" = "name"))

centers <- cbind.data.frame(data.frame(gCentroid(map_hex, byid=TRUE), id=map_hex@data$iso3166_2))

map_hex_fortified %>%
  mutate(log_n = log(n)) %>%
  ggplot() +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = log_n
  )) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "white") +
  theme_void() +
  coord_map()



n_stations <- fuel %>% 
  dplyr::select(state, year) %>% 
  group_by(state, year) %>% 
  count() %>%
  ungroup() %>% 
  group_by(state) %>% 
  mutate(log_n_cum = log(cumsum(n))) %>% 
  ungroup()


n_stations %>% 
  ggplot(aes(x = year, y = log_n_cum, group = state), color = "grey") +
  geom_line()
