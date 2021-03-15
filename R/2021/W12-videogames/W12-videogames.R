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

tuesdata$games %>% 
  filter(gamename %in% most_played_games) %>% 
  mutate(
    month_n = ebmisc::month_to_number(month)
  ) 
