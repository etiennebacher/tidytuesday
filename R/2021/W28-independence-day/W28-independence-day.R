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

holidays[, country := ifelse(country == "Netherlands, The", "Netherlands", country)]


timeline <- function(x) { 
  
  data_plot <- holidays[independence_from %in% x]
  data_min <- data_plot %>% .[year == min(year)]
  data_max <- data_plot %>% .[year == max(year)]
  
  textcolor <- if ("United Kingdom" %in% x) {
    "white"
  } else if ("France" %in% x) {
    "#0055a4"
  } else {
    "#EF3340"
  }
  
  axiscolor <- if ("United Kingdom" %in% x) {
    "#C8102E"
  } else if ("France" %in% x) {
    "#ef4135"
  } else {
    "white"
  }

  
  ggplot(data_plot, aes(year, 1)) +
    geom_segment(
      aes(x = 1550, xend = 2000, y = 1, yend = 1),
      size = 1,
      color = axiscolor
    ) +
    geom_point(fill = axiscolor, color = textcolor, size = 5, shape = 21) +
    geom_curve(
      aes(x = min(year) - 10, xend = min(year)-1, y = 0.99, yend = 0.999),
      size = 0.1,
      curvature = 0,
      color = textcolor
    ) +
    geom_curve(
      aes(x = max(year) - 10, xend = max(year)-1, y = 1.01, yend = 1.001),
      size = 0.1,
      curvature = 0,
      color = textcolor
    ) +
    geom_text(
      data = data_min,
      aes(
        x = year - 10,
        y = 0.987,
        label = paste0(paste(country, collapse = ", "), ": ", year)
      ),
      color = textcolor
    ) +
    geom_text(
      data = data_max,
      aes(
        x = year - 10,
        y = 1.013,
        label = paste0(paste(country, collapse = ", "), ": ", year)
      ),
      color = textcolor
    ) +
    geom_text(
      aes(
        x = 1600,
        y = 1.02
      ),
      label = x[1], # necessary because Spain has several names
      color = textcolor,
      size = 7
    ) +
    ylim(c(0.975, 1.025)) +
    theme_void() 
}


uk <- timeline("United Kingdom") +
  theme(
    plot.background = element_rect(fill = "#012169"),
    panel.background = element_rect(fill = "#012169")
  )

fr <- timeline("France") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

es <- timeline(c("Spain", "Spanish Empire", "Spanish Empire[72]")) +
  theme(
    plot.background = element_rect(fill = "#FFD100"),
    panel.background = element_rect(fill = "#FFD100")
  )


layout <- 
"
AAAA
BBBB
CCCC
"

uk + fr + es +
  plot_layout(design = layout) +
  plot_annotation(
    title = 'When did countries get their independence?',
    subtitle = "The United Kingdom, France, and Spain were the three main European colonial empires. \nOnly the first and last countries which got their independence are annotated on the graph.",
    caption = 'Made by Etienne Bacher | Data from Wikipedia',
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 22),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#012169")
      # text = element_text(color = "white", family = "Ubuntu Mono")
    )
  ) 

