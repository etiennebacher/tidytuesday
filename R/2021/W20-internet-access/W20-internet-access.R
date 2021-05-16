library(data.table)
library(tibble)
library(geofacet)
library(ggtext)
library(janitor)
library(ggplot2)
library(tidytuesdayR)
library(pdftools)


###########################
## Treat data and make plot ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- as.data.table(tuesdata$broadband) %>% 
  clean_names() %>% 
  .[, c("st", "broadband_usage")]

# Remove unnecessary decimals for 0, 0.5 and 1, to save some space
scaleFUN <- function(x) {
  ifelse(
    x %in% c(0, 1),
    sprintf("%.0f", x),
    ifelse(
      x == 0.5,
      "0.5",
      sprintf("%.2f", x)
    )
  )
}

text_color <- "#00e64d"

broadband[, broadband_usage := as.numeric(broadband_usage)] %>% 
  ggplot(aes(broadband_usage)) +
  geom_density(
    color = "#00e64d"
  ) +
  facet_geo(~ st) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = text_color),
    strip.background = element_rect(fill = "#437070"),
    strip.text = element_text(color = text_color),
    plot.background = element_rect(fill = "#2F4F4F"),
    panel.background = element_rect(fill = "#2F4F4F"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed", 
      color = "#ccffdd",
      size = 0.1
    ),
    plot.title = element_text(color = text_color, hjust = 0.5, size = 25),
    plot.subtitle = element_text(color = text_color, hjust = 0.5),
    plot.caption = element_markdown(color = text_color),
    text = element_text(family = "Padauk")
  ) +
  scale_x_continuous(labels = scaleFUN) +
  labs(
    title = "\nInternet access in the United States",
    subtitle = "\nDistribution of people with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps as of the end of 2017.\n\n",
    caption = "<br>Made by Etienne Bacher &middot; Data from Microsoft & The Verge"
  )



###########################
## Export ##
###########################

ggsave("R/2021/W20-internet-access/W20-internet-access.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W20-internet-access/W20-internet-access.pdf", 
            filenames = "R/2021/W20-internet-access/W20-internet-access.png",
            format = "png", dpi = 350)    