library(data.table)
library(ggplot2)
library(forcats)
library(tidytuesdayR)
library(extrafont)
library(pdftools)

# Takes a few minutes
# extrafont::font_import()

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

data_used <- tuesdata$records %>% 
  as.data.table()

most_freq_tracks <- data_used %>% 
  .[, .(.N), by = .(track)] %>% 
  .[order(-N)] %>% 
  head(5) %>% 
  .[, track] 

data_used[track %in% most_freq_tracks] %>% 
  .[, .SD[which.min(time)], by = .(track, shortcut)] %>% 
  .[, .(track, shortcut, time)] %>% 
  dcast(., track ~ shortcut, value.var = "time") %>%
  .[, .(diff_time = No-Yes), by = .(track, Yes, No)] %>% 
  ggplot(aes(fct_reorder(track, diff_time), group = 1)) +
  geom_line(aes(y = No)) +
  geom_line(aes(y = Yes)) +
  geom_ribbon(aes(ymin = No, ymax = Yes))
