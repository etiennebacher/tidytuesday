library(raster)
library(dplyr)
library(tidyr)
library(ggtext) 
library(ggplot2)
library(luciole) # dreamRs/luciole
library(pdftools)

luciole::add_luciole()
showtext::showtext_auto()

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

##############
## Treat data ##
##############

total <- flights %>%
  group_by(STATE_NAME) %>% 
  filter(min(YEAR) == 2016) %>% 
  ungroup() %>% 
  group_by(STATE_NAME, YEAR, MONTH_NUM) %>% 
  summarize(
    total_flights = sum(FLT_TOT_1)
  ) %>% 
  ungroup() %>% 
  unite(col = "yearmonth", YEAR, MONTH_NUM, remove = FALSE) %>% 
  distinct() %>% 
  group_by(STATE_NAME) %>% 
  mutate(share = total_flights/sum(total_flights, na.rm = TRUE)*100) %>% 
  ungroup() 



##############
## Make the plot ##
##############

FILL ="#c1d0f0"
LINE_PRIMARY = "#3366cc"
LINE_SECONDARY = "#d9d9d9"
FONT = "Cormorant Garamond"

shades <- data.frame(
  xmin = paste0(2016:2022, "_06"),
  xmax = paste0(2016:2022, "_09")
)


ggplot(
  data = total %>% filter(STATE_NAME != "France"), 
  aes(x = yearmonth, y = share, group = STATE_NAME)
) +

  ### Shaded areas
  
  scale_x_discrete(
    breaks = paste0(2016:2022, "_01"),
    labels = paste0(2016:2022, ", Jan.")
  ) +
  geom_rect(
    data = shades, 
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 3), 
    inherit.aes = FALSE, 
    alpha = 0.3, 
    fill = FILL
  ) +
  
  ### Custom grid lines
  
  geom_segment(
    data = tibble(y = seq(1, 3, by = 1), x1 = "2016_01", x2 = Inf),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "#e6e6e6",
    size = 0.6
  ) +
  geom_segment(
    data = tibble(y1 = 0, y2 = 3, x = paste0(2016:2022, "_01")),
    aes(x = x, xend = x, y = y1, yend = y2),
    inherit.aes = FALSE,
    color = "#e6e6e6",
    size = 0.6
  ) +
  geom_hline(yintercept = 0) +
  
  ### Custom legend
  
  geom_text(
    data = data.frame(x = "2018_03", y = 3.3),
    aes(
      x = x,
      y = y
    ),
    label = " = June, 1 - September, 1",
    inherit.aes = FALSE,
    family = FONT
  ) +
  geom_rect(
    data = data.frame(
      xmin = "2017_08", xmax = "2017_11"
    ), 
    aes(xmin = xmin, xmax = xmax, ymin = 3.2, ymax = 3.4), 
    inherit.aes = FALSE, 
    alpha = 0.3, 
    color = "#e6e6e6",
    fill = FILL
  ) +
  
  
  geom_line(color = LINE_SECONDARY) +
  geom_line(
    data = total %>% filter(STATE_NAME == "France"), 
    color = LINE_PRIMARY, 
    size = 1
  ) +
  
  ### Covid line
  
  geom_vline(
    xintercept = "2020_04", 
    linetype = "dotted",
    size = 1
  ) +
  geom_curve(
    aes(x = "2020_07", xend = "2020_05", y = 3.15, yend = 3.05),
    arrow = arrow(length = unit(2, "mm")),
    size = 0.1,
    curvature = -0.2
  ) +
  geom_text(
    data = data.frame(x = "2020_09", y = 3.2),
    aes(
      x = x,
      y = y
    ),
    label = "March 2020",
    inherit.aes = FALSE,
    family = FONT
  ) +
  
  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = paste0(
      "<br>The seasonality of flights in Europe and in <span style='color:",
      LINE_PRIMARY, "'>France</span>"
    ),
    subtitle = "Each line is associated to a European country with complete flight data from January  2016 to May 2022. <br> The y-axis measures the share of flights from and to each country that each month represents in the total <br> number of flights across the full period. <br> <br> <i>Example: July  2016 contains 1.8% of all flights from and to France over the full period.</i>",
    caption = "Data from Eurocontrol &middot; Graph by Etienne Bacher",
    x = "",
    y = ""
  ) +
  theme(
    plot.background = element_rect(fill = "#f9f2ec"),
    panel.background = element_rect(fill = "#f9f2ec"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = FONT, size = 17),
    plot.title = element_markdown(lineheight = 1.2, size = 25, face = "bold"),
    plot.subtitle = element_markdown(lineheight = 1.2, size = 15),
    plot.caption = element_markdown(hjust = 0)
  ) 



###########################
## Export ##
###########################

ggsave("R/2022/W28-european-flights/european-flights.pdf", 
       width = 16, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2022/W28-european-flights/european-flights.pdf", 
            filenames = "R/2022/W28-european-flights/european-flights.png",
            format = "png", dpi = 350)   
