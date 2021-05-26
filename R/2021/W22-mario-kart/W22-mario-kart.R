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

color_no_short <- "#ff0000"
color_short <- "#00b33c"

data_cleaned %>% 
  ggplot(aes(fct_reorder(track, diff_time), group = 1)) +
  geom_line(aes(y = No), color = color_no_short, size = 2) +
  geom_line(aes(y = Yes), color = color_short, size = 2) +
  geom_ribbon(aes(ymin = No, ymax = Yes), fill = "#d9e6f2") +
  ylab("Number of seconds") +
  scale_color_manual(
    values = c("Without shortcuts" = color_no_short, "With shortcuts" = color_short)
  ) +
  theme(
    plot.background = element_rect(fill = "#e6f7ff"),
    panel.background = element_rect(fill = "#e6f7ff"),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(hjust = 0.5, size = 20)
  ) +
  labs(
    subtitle = paste0("\nOn the five most played tracks in Mario Kart, shortcuts have saved in average ", mean(data_cleaned$diff_time), " seconds.\n"),
    caption = "\nMade by Etienne Bacher, data from Benedikt Claus"
  ) +
  geom_curve(
    aes(x = 5, xend = 4.6, y = 120, yend = 100),
    arrow = arrow(length = unit(2, "mm")),
    colour = color_no_short,
    size = 0.5,
    curvature = -0.4
  ) +
  annotate(
    "text",
    x = 5,
    y = 122,
    label = "Time without shortcuts",
    color = color_no_short,
    # family = "Oxygen Mono",
    size = 5
  ) +
  geom_curve(
    aes(x = 3.7, xend = 3.5, y = 25, yend = 39),
    arrow = arrow(length = unit(2, "mm")),
    colour = color_short,
    size = 0.5,
    curvature = 0.4
  ) +
  annotate(
    "text",
    x = 3.7,
    y = 21,
    label = "Time with shortcuts",
    color = color_short,
    # family = "Oxygen Mono",
    size = 5
  )
