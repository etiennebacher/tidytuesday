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
  distinct()


packing <- circleProgressiveLayout(clean_data$n, sizetype = 'area')

clean_data <- cbind(clean_data, packing)


clean_data_mod <- circleLayoutVertices(packing, npoints = 50)

ggplot() + 
  geom_polygon(
    data = clean_data_mod, 
    aes(x, y, group = id, fill = as.factor(id)),
    colour = "black", 
    alpha = 0.6
  ) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = clean_data, aes(x, y, size = n, label = state)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()
