library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)
library(gghighlight)
library(ggrepel)
library(broom)
library(rgeos)
library(geojsonio)
library(rgdal)
library(usdata)


###########################
## Treat data ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 16)


clean_data <- tuesdata$post_offices %>%
  select(state, established) %>% 
  rename(year = established) %>% 
  filter(!is.na(year), year > 1492) %>% 
  group_by(state, year) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(state, year) %>% 
  group_by(state) %>% 
  mutate(
    cumsum = cumsum(n)
  ) 

# Create all combinations state-year possible in a separate df and 
# merge it with the data
complete_data <- data.frame(
  state = rep(unique(clean_data$state), each = 362),
  year = rep(seq(min(clean_data$year), 
                 max(clean_data$year), 
                 1), 
             times = length(unique(clean_data$state)))
)

# Same value for cumsum if NA and if there was a value earlier
# e.g if NA in 1883 and value in 1882, then value in 1883 = value in 1883
# If only NA before, set to 0
clean_data <- left_join(complete_data, clean_data, 
                        by = c("year", "state")) %>% 
  group_by(state) %>% 
  fill(n, cumsum, .direction = "down") %>% 
  mutate(
    n = replace_na(n, 0),
    cumsum = replace_na(cumsum, 0),
    state = usdata::abbr2state(state)
  )


##########
## Data for hex map of US ##
## Taken here: http://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
##########

spdf <- geojson_read("R/2021/W16-us-post-offices/us_states_hexgrid.geojson",  
                     what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label
centers <- cbind.data.frame(
  data.frame(
    gCentroid(spdf, byid=TRUE),
    id=spdf@data$iso3166_2
  )
)

spdf_fortified <- spdf_fortified %>% 
  left_join(clean_data, by = c("id" = "state"))
  
# plots <-
  spdf_fortified %>% 
  filter(year %% 5 == 0) %>% 
  select(-order) %>% 
  group_by(id) %>% 
  slice_head(n = 73) %>% 
  ungroup() %>% 
  ggplot() +
  geom_polygon(
    aes(x = long, y = lat, fill = cumsum, group = group),
    color = "white"
  ) +
  geom_text(
    data = centers, 
    aes(x = x, y = y, label = id), 
    color = "white"
  ) +
  theme_void() +
  labs(title = "Year: {closest_state}") +
  coord_map() +
  transition_states(year)

animate(plots, end_pause = 5)
