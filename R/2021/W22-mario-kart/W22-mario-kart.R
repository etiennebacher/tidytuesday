library(data.table)
library(ggplot2)
library(forcats)
library(tibble)
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
  geom_ribbon(aes(ymin = No, ymax = Yes), fill = "#f2d9bf") +
  ylab("Number of seconds") +
  geom_curve(
    aes(x = 5, xend = 4.6, y = 118, yend = 100),
    arrow = arrow(length = unit(2, "mm")),
    colour = color_no_short,
    size = 0.5,
    curvature = -0.4
  ) +
  annotate(
    "text",
    x = 5,
    y = 122,
    label = "Best time without shortcuts",
    color = color_no_short,
    family = "Oxygen Mono",
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
    label = "Best time with shortcuts",
    color = color_short,
    family = "Oxygen Mono",
    size = 5
  ) +
  geom_segment(
    data = data_cleaned[order(diff_time)] %>%
      tibble::rowid_to_column(),
    aes(x = rowid, xend = rowid, y = Yes + 1, yend = No - 1),
    arrow = arrow(length = unit(2, "mm"), ends = "both"),
    color = "#95591d"
  ) +
  geom_text(
    data = data_cleaned[order(diff_time)] %>%
      tibble::rowid_to_column(),
    aes(
      x = rowid - 0.05,
      y = Yes + (No-Yes)/2,
      label = paste0("-", round(diff_time, 1), " s")
    ),
    angle = 90,
    color = "#95591d",
    family = "Oxygen Mono",
    size = 5
  ) +
  theme(
    plot.background = element_rect(fill = "#FAF0E6"),
    panel.background = element_rect(fill = "#FAF0E6"),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(color = "#95591d", size = 13),
    axis.ticks = element_blank(),
    text = element_text(color = "#95591d", family = "Oxygen Mono"),
  ) +
  labs(
    subtitle = paste0("\nShortcuts have saved on average ", 
                      mean(data_cleaned$diff_time), 
                      " seconds in Mario Kart's five most played tracks.\n"),
    caption = "\n\nMade by Etienne Bacher, data from Benedikt Claus"
  ) 


###########################
## Export ##
###########################

ggsave("R/2021/W22-mario-kart/mario-kart.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W22-mario-kart/mario-kart.pdf", 
            filenames = "R/2021/W22-mario-kart/mario-kart.png",
            format = "png", dpi = 350)   
