library(dplyr)
library(tidyr)
library(ebmisc)
library(ggplot2)
library(tidytuesdayR)
library(extrafont)
library(pdftools)

# Takes a few minutes
# extrafont::font_import()

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)


##########
## Clean the data ##
##########

data_clean <- tuesdata$games %>% 
  mutate(
    month_decimal = as.numeric(ebmisc::month_to_number(month)) * 8/100,
    x_axis = year + month_decimal
  ) %>% 
  group_by(x_axis) %>% 
  mutate(tot_players = sum(avg, na.rm = T)) %>% 
  select(x_axis, year, tot_players) %>% 
  distinct()


##########
## Make the plot ##
##########

data_clean %>% 
  ggplot(aes(x = x_axis, y = tot_players)) +
  geom_line(color = "#b37700") +
  geom_area(fill = "#333333") +
  scale_x_continuous(
    breaks = seq(2013, 2021, 1),
    limits = c(2012.56, 2021.16)
  ) +
  scale_y_continuous(
    labels = paste0(seq(0, 5, 1), "M"), 
    limits = c(0, 5000000)
  ) +
  labs(
    title = "\nAverage number of players simultaneously playing on Steam",
    caption = "\nMade by Etienne Bacher, with SteamCharts data"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "#333333", size = 13),
    axis.text.y = element_text(color = "#333333", size = 13),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#ffeecc"),
    plot.background = element_rect(fill = "#ffeecc"),
    panel.grid.major = element_line(color = "#ffe6b3"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "#ffe6b3"),
    plot.title = element_text(hjust = 0.5, color = "#b37700", size = 20),
    plot.caption = element_text(color = "#b37700", size = 12),
    text = element_text(family = "Oxygen Mono")
  ) +
  geom_curve(
    aes(x = 2016.3, xend = 2017.2, y = 3500000, yend = 2400000),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = -0.4
  ) +
  geom_curve(
    aes(x = 2020.1, xend = 2020.25, y = 1500000, yend = 3350000),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate(
    "text",
    x = 2015.6,
    y = 3500000,
    label = paste0('Release of \n"PlayerUnknown', "'s \nBattlegrounds", '"'),
    color = "#b37700",
    family = "Oxygen Mono",
    size = 5
  ) +
  annotate(
    "text",
    x = 2019.8,
    y = 1200000,
    label = "Start of anti-Covid \nmeasures in Europe",
    color = "#b37700",
    family = "Oxygen Mono",
    size = 5
  ) +
  annotate(
    "text",
    x = 2020.32,
    y = 4680000,
    label = "4.527M",
    color = "#b37700",
    family = "Oxygen Mono",
    size = 5
  ) +
  annotate(
    "text",
    x = 2018.08,
    y = 4250000,
    label = "4.102M",
    color = "#b37700",
    family = "Oxygen Mono",
    size = 5
  )


##########
## Export ##
##########

ggsave("R/2021/W12-steam-games/steam-games.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W12-steam-games/steam-games.pdf", 
            filenames = "R/2021/W12-steam-games/steam-games.png",
            format = "png", dpi = 350)  
  
  
  
