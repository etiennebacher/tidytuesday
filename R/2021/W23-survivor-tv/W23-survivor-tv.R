library(data.table)
library(survivoR)
library(ggplot2)
library(ggridges)
library(ggtext)
library(forcats)
library(extrafont)
library(pdftools)

# Takes a few minutes
# extrafont::font_import()


##########
## Treat the data and make the plot ##
##########

clean_data <- as.data.table(viewers) %>% 
  .[season == 1 | season %% 5 == 0] %>% 
  .[, .(season = factor(season), viewers)] %>% 
  .[, mean_viewers := mean(viewers, na.rm = T), by = season]

ggplot(clean_data, aes(x = viewers, y = season)) +
  geom_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    vline_color = "white",
    fill = "#804000"
  ) +
  # stat_density_ridges(quantile_lines = TRUE, quantiles = 2)
  scale_x_continuous(
    breaks = seq(5, 30, 5), 
    limits= c(5, 30)
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Number of viewers",
    y = "Season",
    title = '"The Survivor": a less and less popular TV show',
    caption = "Made by Etienne Bacher &middot; Data from the survivoR package"
  ) +
  geom_curve(
    aes(x = 21.6, xend = 20.4, y = 5.7, yend = 6.9),
    arrow = arrow(length = unit(2, "mm")),
    colour = "white",
    size = 0.5,
    curvature = -0.4
  ) +
  geom_textbox(
    aes(
      x = 23.1,
      y = 5.5
    ),
    label = "Average number of \nviewers during the season",
    color = "white",
    fill = "#804000",
    family = "Chilanka",
    width = unit(4, "cm")
  ) +
  theme(
    plot.background = element_rect(fill = "#cc6600"),
    panel.background = element_rect(fill = "#cc6600"),
    plot.caption = element_markdown(),
    text = element_text(color = "white", family = "Chilanka"),
    axis.title = element_text(color = "white", size = 15),
    axis.text = element_text(color = "white", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 28, hjust = 0.5)
  )


##########
## Export ##
##########

ggsave("R/2021/W23-survivor-tv/survivor-tv.pdf", 
       width = 16, height = 10, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W23-survivor-tv/survivor-tv.pdf", 
            filenames = "R/2021/W23-survivor-tv/survivor-tv.png",
            format = "png", dpi = 350)  
