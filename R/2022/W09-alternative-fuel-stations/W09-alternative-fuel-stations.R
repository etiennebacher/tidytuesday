library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(geojsonio)
library(sf)
library(raster)
library(broom)
library(luciole) # dreamRs/luciole
library(rgeos)
library(pdftools)
library(patchwork)

luciole::add_luciole()
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



luciole::add_luciole()
bg_color <- "#333333"
text_color <- "#e6e6e6"



##########
## Make plot for first station with electric per state ##
##########

first_electric_station <-
  ggplot(test, aes(x = min_year, y = state)) +
  geom_point(color = "#0080ff") +
  xlab("Year") +
  geom_curve(
    aes(x = 1997, xend = 1995, y = 20, yend = 6),
    arrow = arrow(length = unit(2, "mm")),
    colour = text_color,
    size = 0.25,
    curvature = 0.4
  ) +
  geom_text(
    aes(
      x = 1998.5,
      y = 21
    ),
    label = "California",
    color = text_color
  ) +
  geom_curve(
    aes(x = 2017, xend = 2014.5, y = 11, yend = 1),
    arrow = arrow(length = unit(2, "mm")),
    colour = text_color,
    size = 0.25,
    curvature = -0.4
  ) +
  geom_text(
    aes(
      x = 2017.5,
      y = 14
    ),
    label = "Alaska",
    color = text_color
  ) +
  geom_curve(
    aes(x = 2016, xend = 2013, y = 42, yend = 48.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = text_color,
    size = 0.25,
    curvature = -0.4
  ) +
  geom_text(
    aes(
      x = 2017.25,
      y = 44
    ),
    label = "Wyoming",
    color = text_color
  ) +
  labs(
    title = "When did the first charging station appear?"
  ) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = text_color),
    axis.title = element_text(color = text_color),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#d9d9d9", linetype = "dashed", size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 15, color = text_color),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    text = element_text(family = "Luciole")
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
  here::here("R/2022/W09-alternative-fuel-stations/us_states_hexgrid.geojson.json"),
  what = "sp"
)

map_hex@data <- map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
map_hex_fortified <- tidy(map_hex, region = "google_name") %>% 
  left_join(fuel_state, c("id" = "name"))

centers <- cbind.data.frame(data.frame(gCentroid(map_hex, byid=TRUE), id=map_hex@data$iso3166_2))

map_electric_station <-
  map_hex_fortified %>%
  ggplot() +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = n
  )) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "white") +
  theme_void() +
  coord_map() +
  scale_fill_continuous() +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    legend.text = element_text(color = text_color),
    legend.title = element_text(color = text_color),
    text = element_text(family = "Luciole")
  ) +
  labs(caption = "\n\n", fill = "Number of stations")


#############
### Plot evolution of electric stations ### 
#############

evol_electric_station <-
  fuel |> 
  filter(year <= 2020) |> 
  dplyr::select(year) |> 
  group_by(year) |> 
  count() |> 
  ungroup() |> 
  mutate(n_cum = cumsum(n)) |> 
  arrange(year) |> 
  ggplot(aes(x = year, y = n_cum)) +
  geom_line(color = "#0080ff") +
  labs(title = "Number of charging stations") +
  scale_y_continuous(breaks = c(seq(0, 30000, by = 5000))) +
  xlab("Year") +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = text_color),
    axis.title = element_text(color = text_color),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#d9d9d9", linetype = "dashed", size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 15, color = text_color),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    text = element_text(family = "Luciole")
  )





layout <- "
AA
AA
BC
"

map_electric_station + 
  first_electric_station +
  evol_electric_station + 
  plot_layout(design = layout) &
  plot_annotation(
    title = "\nThe rise of electric vehicle charging stations in the US",
    caption = "<br>Visualization by Etienne Bacher &middot; Data from US DOT",
    theme = theme(
      plot.title = element_text(size = rel(2.5), 
                                hjust = 0.5, color = text_color),
      plot.caption = element_markdown(hjust = 0.5, color = text_color),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      text = element_text(family = "Luciole")
    )
  ) 



###########################
## Export ##
###########################

ggsave("R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.pdf", 
       width = 16, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.pdf", 
            filenames = "R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.png",
            format = "png", dpi = 350)   
