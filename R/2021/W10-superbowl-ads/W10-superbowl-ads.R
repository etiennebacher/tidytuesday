library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytuesdayR)


tuesdata <- tidytuesdayR::tt_load(2021, week = 10)

tuesdata$youtube %>% 
  select(
    -all_of(contains("url")), 
    -c(id, etag, kind, published_at, thumbnail, channel_title, category_id)
  ) %>% 
  mutate(brand = ifelse(brand == "Hynudai", "Hyundai", brand)) %>% 
  arrange(year) %>% 
  slice_max(view_count, n = 20) %>% 
  unite(col = brand_id, brand, year, sep = " ") %>% 
  ggplot(aes(x = brand_id, y = view_count)) +
  geom_point() +
  geom_segment(aes(x = brand_id, xend = brand_id, y = 0, yend = view_count))
  

