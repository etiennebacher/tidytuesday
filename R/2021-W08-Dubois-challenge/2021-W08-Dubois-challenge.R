library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(pdftools)
library(pBrackets)
library(grid)

### Try to reproduce https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge01/original-plate-07.jpg

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

# Main plot

p <- tuesdata$georgia_pop %>%
  pivot_longer(
    cols = !Year,
    names_to = "Color",
    values_to = "Percent"
  ) %>% 
  ggplot(aes(x = Year, y = Percent, linetype = Color)) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "longdash"), labels = c("COLORED", "WHITE")) +
  scale_x_continuous(breaks = seq(1790, 1890, 10), expand = c(0, 0)) +
  scale_y_continuous(trans = "reverse", breaks = seq(0, 100, 5), expand = c(0, 0)) +
  coord_flip(clip = "off") +
  theme(
    text = element_text(family = "mono"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#ffc7b3"),
    panel.grid.major.y = element_line(color = "#ffc7b3"),
    plot.background = element_rect(fill = "#f7ddbb"),
    panel.background = element_rect(fill = "#f7ddbb", colour = "#808080"),
    plot.title = element_text(hjust = 0.5, size = "20"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(colour = "#808080"),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#f7ddbb"),
    legend.key = element_rect(fill = "#f7ddbb"),
    legend.text = element_text(margin = margin(r = 13, unit = "cm"), size = 8, colour = "#808080"),
    legend.key.width = unit(2.3, "cm"),
    legend.box.margin = margin(l = 13, t = 3.5, unit = "cm"),
    plot.margin = margin(l = 5, r = 5, b = 1, unit = "cm")
  ) +
  labs(title = toupper("\ncomparative increase of white and colored\npopulation in georgia.\n\n")) 


# Add curly brace below
# Thanks to https://stackoverflow.com/a/35662327

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}
b1 <- bracketsGrob(1.02, -0.08,-0.02, -0.08, curvature = 0.5, lwd = 1, col = "#808080")

# Add "PERCENTS"

percent_annotation <- textGrob("PERCENTS", 0.47, -0.16, 0.5, -0.16, 
                               gp = gpar(fontsize = 8, col = "#808080"))


# Final plot

p +
  annotation_custom(b1) +
  annotation_custom(percent_annotation)


# Export

ggsave("R/2021-W08-Dubois-challenge/dubois-challenge-georgia.pdf", 
       width = 13.3, height = 13.5, device = cairo_pdf)


pdf_convert(pdf = "R/2021-W08-Dubois-challenge/dubois-challenge-georgia.pdf", 
            filenames = "R/2021-W08-Dubois-challenge/dubois-challenge-georgia.png",
            format = "png", dpi = 350)
