library(dplyr)
library(tidyr)
library(stringr)
library(ggtext) 
library(ggplot2)
library(pdftools)

FONT = "Vollkorn"

showtext::showtext_auto()
sysfonts::font_add_google(FONT)


#############
### Treat data ### 
#############

tuesdata = tidytuesdayR::tt_load('2022-12-20')

test = tuesdata$weather_forecasts |> 
  filter(!is.na(observed_temp), !is.na(forecast_temp)) |> 
  mutate(
    diff = observed_temp - forecast_temp,
    week = lubridate::week(date),
    week = ifelse(nchar(week) == 1, paste0("0", week), week),
    yearweek = paste0(lubridate::year(date), "-", week)
  ) |> 
  select(date, yearweek, forecast_hours_before, forecast_temp, observed_temp, diff) |> 
  group_by(yearweek, forecast_hours_before) |> 
  summarize(mean_diff = mean(diff)) |> 
  ungroup()


#############
### Make plot ### 
#############

test |> 
  pivot_wider(
    names_from = forecast_hours_before,
    values_from = mean_diff
  ) |> 
  ggplot(aes(yearweek)) +
  
  # Make grid (has to be first so that it doesn't overlay the bars)
  geom_segment(
    data = tibble(y = seq(-1, 3, by = 1), x1 = "2021-05", x2 = Inf),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "#cce6ff",
    alpha = 0.5,
    size = 0.6
  ) +
  geom_segment(
    data = tibble(y1 = -1, y2 = 3, x = c(
      paste0("2021-", seq(10, 50, 10)), 
      paste0("2022-", seq(10, 20, 10))
    )),
    aes(x = x, xend = x, y = y1, yend = y2),
    inherit.aes = FALSE,
    alpha = 0.5,
    color = "#cce6ff",
    size = 0.6
  ) +
  
  # Manual bars because otherwise values are stacked on each other
  geom_col(aes(y = `48`, fill = "48")) +
  geom_col(aes(y = `36`, fill = "36")) +
  geom_col(aes(y = `24`, fill = "24")) +
  geom_col(aes(y = `12`, fill = "12")) +
  
  
  geom_curve(
    aes(x = "2021-20", xend = "2021-10", y = 1.75, yend = 1.9),
    arrow = arrow(length = unit(2, "mm")),
    size = 0.1,
    curvature = -0.2
  ) +
  geom_text(
    data = data.frame(x = "2021-26", y = 1.9),
    aes(
      x = x,
      y = y
    ),
    label = str_wrap("The 8th week of 2021, actual temperatures were on average 2.45°F higher than predictions 48h before, and 1.45°F higher than the predictions 12h before.", 30),
    inherit.aes = FALSE,
    family = FONT
  ) +

  scale_x_discrete(
    breaks = c(
      paste0("2021-", seq(10, 50, 10)), 
      paste0("2022-", seq(10, 20, 10))
    ),
    labels = c(
      paste0("2021, week ", seq(10, 50, 10)), 
      paste0("2022, week ", c(10, 20))
    )
  ) +
  scale_fill_manual(
    values = rev(c("#99ddff", "#1ab2ff", "#0077b3", "#00334d")),
    breaks = c(12, 24, 36, 48)
  ) +
  labs(
    x = "",
    y = "Average difference (°F)",
    fill = "Hours between prediction\n and observation",
    title = "Predicted temperatures are often underestimated",
    subtitle = str_wrap("This plot shows the average difference between observed and predicted temperatures per week in the US. Predictions are given 12, 24, 36, and 48 hours in advance. A positive value means that the actual temperature was higher than predicted.", 100),
    caption = "Data from USA National Weather Service, vizualisation by Etienne Bacher\n"
  ) +
  guides(
    fill = guide_legend(title.position = "bottom", label.position = "top", title.hjust = 0.5)
  ) +
  theme(
    plot.background = element_rect(fill = "#e6f2ff"),
    panel.background = element_rect(fill = "#e6f2ff"),
    legend.background = element_rect(fill = "#e6f2ff"),
    legend.position = "bottom",
    legend.key.height = unit(0.2, "cm"),
    legend.key.width = unit(1, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.margin = margin(l = 10, r = 20, t = 10),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 15),
    text = element_text(family = FONT),
    axis.title.y = element_text(vjust = 2, size = 15),
    axis.text = element_text(size = 12)
  )



#############
### Export ### 
#############

ggsave("R/2022/W51-temperature-predictions/temperature-predictions.pdf", 
       width = 13, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2022/W51-temperature-predictions/temperature-predictions.pdf", 
            filenames = "R/2022/W51-temperature-predictions/temperature-predictions.png",
            format = "png", dpi = 350)   

