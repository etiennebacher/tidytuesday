library(dplyr)
library(tidyr)
library(ebmisc)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

most_played_games <- tuesdata$games %>% 
  group_by(gamename) %>% 
  mutate(tot_avg = mean(avg)) %>% 
  ungroup() %>% 
  select(gamename, tot_avg) %>% 
  distinct() %>% 
  slice_max(order_by = tot_avg, n = 10) %>% 
  pull(gamename)

### Do a plot that display the area between peak and average

# clean_data <- 
tuesdata$games %>% 
  filter(gamename %in% most_played_games) %>% 
  mutate(
    month_n = ebmisc::month_to_number(month),
    time = as.Date(paste0(year, "-", month_n, "-01"))
  ) %>% 
  ggplot(aes(x = time, y = avg)) +
  geom_line() +
  geom_ribbon(aes(ymin = avg, ymax = peak)) +
  geom_line(aes(y = peak)) +
  facet_wrap(~ gamename, scales = "free_y")
