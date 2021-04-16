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
library(gganimate)


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
    state_name = usdata::abbr2state(state)
  ) %>%
  filter(!state_name %in% c("Alaska", "Hawaii"),
         year %% 25 == 0)


us_states <- ggplot2::map_data("state") %>% 
  mutate(region = tools::toTitleCase(region))

clean_data_2 <- clean_data %>% 
  left_join(us_states, by = c("state_name" = "region")) %>% 
  distinct() %>% 
  group_by(year) %>% 
  mutate(highest = ifelse(cumsum == max(cumsum), 1, 0)) %>% 
  ungroup() %>% 
  filter(year >= 1800)



### Ajouter un texte : plus forte hausse, plus forte baisse


clean_data_2 %>% 
  ggplot() +
  geom_polygon(
    aes(
      x = long, 
      y = lat, 
      group = group
    ),
    fill  = "#F2F2F2",
    color = "white"
  ) +
  geom_polygon(
    aes(
      x = long, 
      y = lat, 
      group = group,
      fill = cumsum
    ),
    color = "white"
  ) +
  scale_fill_continuous(
    low = "#B0C4DE", high = "#104E8B"
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  facet_wrap(~year)
