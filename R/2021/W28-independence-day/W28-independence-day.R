library(data.table) ## Need 1.14.2 (development version at the day of writing)
library(ggplot2)
library(ggtext)
library(patchwork)
library(stringr)
library(dplyr)
library(tidytuesdayR)
library(extrafont)
library(pdftools)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv') %>% 
  as.data.table() %>% 
  .[, .(country, year, independence_from)]


timeline <- function(x) { 
  
  data_plot <- holidays[independence_from %in% x]
  data_min <- data_plot %>% .[year == min(year)]
  data_max <- data_plot %>% .[year == max(year)]
  
  ggplot(data_plot, aes(year, 1)) +
    geom_point(color = "red") +
    geom_segment(
      aes(x = 1550, xend = 2000, y = 1, yend = 1),
      size = 0.1,
      color = "red"
    ) +
    geom_curve(
      aes(x = min(year) - 10, xend = min(year)-1, y = 0.99, yend = 0.999),
      arrow = arrow(length = unit(2, "mm")),
      size = 0.1,
      curvature = 0
    ) +
    geom_curve(
      aes(x = max(year) - 10, xend = max(year)-1, y = 1.01, yend = 1.001),
      arrow = arrow(length = unit(2, "mm")),
      size = 0.1,
      curvature = 0
    ) +
    geom_text(
      data = data_min,
      aes(
        x = year - 10,
        y = 0.987,
        label = paste0(paste(country, collapse = ", "), ": ", year)
      )
    ) +
    geom_text(
      data = data_max,
      aes(
        x = year - 10,
        y = 1.013,
        label = paste0(paste(country, collapse = ", "), ": ", year)
      )
    ) +
    ylim(c(0.975, 1.025)) +
    theme_void()
}
uk <- timeline("United Kingdom")
fr <- timeline("France")
es <- timeline(c("Spain", "Spanish Empire", "Spanish Empire[72]"))

