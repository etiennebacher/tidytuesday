library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)


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



###########################
## Make plot ##
###########################

bg_color <- "#e6f5ff"

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
    low = "#B0C4DE", high = "#104E8B",
    breaks = c(0, max(clean_data_2$cumsum)),
    labels = c(0, max(clean_data_2$cumsum))
  ) +
  facet_wrap(~year) +
  labs(
    title = paste0("<br><b style='color: ", 
                   '"#104E8B"', 
                   "'> Number of post offices per State</b><br>"),
    caption = paste0("<b style='color: ",
                     '"#104E8B"',  "'> Made by Etienne Bacher &middot; Data from Cameron Blevins and Richard W. Helbock</b><br>")
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(3.5, "cm"),
    legend.text = element_text(size = 13),
    legend.margin = margin(0.5, 0, 0.5, 0, unit = "cm"),
    plot.margin = margin(0, 2.5, 0, 2.5, unit = "cm"),
    plot.title = element_markdown(hjust = 0.5, size = 20),
    plot.caption = element_markdown(hjust = 0.5),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    strip.text = element_text(size = 12),
    text = element_text(family = "Sawasdee")
  ) 


###########################
## Export ##
###########################

ggsave("R/2021/W16-us-post-offices/us-post-offices.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W16-us-post-offices/us-post-offices.pdf", 
            filenames = "R/2021/W16-us-post-offices/us-post-offices.png",
            format = "png", dpi = 350)  
