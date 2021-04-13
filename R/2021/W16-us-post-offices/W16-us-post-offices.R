library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)
library(gghighlight)
library(ggrepel)
library(packcircles)



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
    cumsum = replace_na(cumsum, 0)
  )


packing <- circleProgressiveLayout(clean_data %>% 
                                     filter(year == 1900) %>% 
                                     pull(cumsum), 
                                   sizetype = 'area')
foo <- cbind(clean_data %>% 
                      filter(year == 1900), packing)

clean_data_mod <- circleLayoutVertices(packing, npoints = 50)

ggplot() + 
  geom_polygon(
    data = clean_data_mod, 
    aes(x, y, group = state, fill = as.factor(state)),
    colour = "black", 
    alpha = 0.6
  ) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = foo, aes(x, y, size = cumsum, label = state)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
