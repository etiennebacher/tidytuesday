library(data.table)
library(ggplot2)
library(forcats)
library(tidytuesdayR)
library(extrafont)
library(pdftools)

# Takes a few minutes
# extrafont::font_import()


##########
## Treat the data ##
##########

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

data_used <- tuesdata$records %>% 
  as.data.table()

most_freq_tracks <- data_used %>% 
  .[, .(.N), by = .(track)] %>% 
  .[order(-N)] %>% 
  head(5) %>% 
  .[, track] 

data_cleaned <- data_used[track %in% most_freq_tracks] %>% 
  .[, .SD[which.min(time)], by = .(track, shortcut)] %>% 
  .[, .(track, shortcut, time)] %>% 
  dcast(., track ~ shortcut, value.var = "time") %>%
  .[, .(diff_time = No-Yes), by = .(track, Yes, No)] 


##########
## Make the plot ##
##########

data_cleaned %>% 
  ggplot(aes(fct_reorder(track, diff_time), group = 1)) +
  geom_line(aes(y = No), color = "#ff0000") +
  geom_line(aes(y = Yes), color = "#00b33c") +
  geom_ribbon(aes(ymin = No, ymax = Yes), fill = "#d9e6f2") +
  ylab("Number of seconds") +
  scale_color_manual(
    values = c("Without shortcuts" = "#ff0000", "With shortcuts" = "#00b33c")
  ) +
  theme(
    plot.background = element_rect(fill = "#e6f7ff"),
    panel.background = element_rect(fill = "#e6f7ff"),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "How much time do shortcuts save?",
    subtitle = paste0("On the five most played tracks, shortcuts have saved in average ", mean(data_cleaned$diff_time), " seconds."),
    caption = "\nMade by Etienne Bacher, data from Benedikt Claus"
  )
