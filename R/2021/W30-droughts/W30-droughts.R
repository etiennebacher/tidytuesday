library(data.table)
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggstream)
library(patchwork)
library(tidytuesdayR)
library(extrafont)
library(pdftools)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################


### Data for animal rescue

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)

cleaned_data <- tuesdata$drought %>% 
  as.data.table() %>% 
  
  # Each event has five lines for the five levels. But if a level is not reached
  # then area_pct = 0.00 (which means that no land was concerned with a drought
  # of level XXX).
  # So if I want to keep only the levels that happened, I remove lines where
  # area_pct = 0.00
  # Then, for one event, I only want the highest level of drought (e.g for 
  # 20210713, I only keep D0) 
  .[area_pct != 0] %>% 
  
  # I also only want the highest drought level per state, e.g if a state
  # had None, D0, D1 at a time period, I only want D1. The highest level of
  # drought is also the one that impacts the most lands
  group_by(map_date, state_abb) %>% 
  filter(area_pct == min(area_pct)) %>% 
  ungroup() %>% 
  as.data.table()
  


cleaned_data[, year_start := substr(as.character(valid_start), 1, 4)]

cleaned_data %>% 
  .[, .N, by = .(year_start, drought_lvl)] %>% 
  .[order(-year_start, drought_lvl)]
