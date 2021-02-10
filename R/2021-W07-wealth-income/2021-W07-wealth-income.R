library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(hrbrthemes)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(ggrepel)
library(glue)
library(ggtext)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2021, week = 7)


# top_5 <-
plot_owner_perc <- function(ethnic) {

  tuesdata$home_owner %>%
    filter(race == ethnic) %>%
    mutate(owner = round(mean(home_owner_pct, na.rm = T)*100, 0),
           non_owner = 100-owner) %>%
    ungroup() %>%
    pivot_longer(
      cols = c("owner", "non_owner"),
      names_to = "is_owner"
    ) %>%
    select(is_owner, value) %>%
    distinct() %>%
    ggplot(aes(fill = is_owner, values = value)) +
    geom_waffle(n_rows = 5) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() +
    scale_fill_manual(
      name = NULL,
      values = c("#cc9966", "#660000"),
      labels = NULL
    ) +
    ylim(0, 8) +
    theme(legend.position = "none")

}

plot_white <- plot_owner_perc("White") +
  geom_curve(
    aes(x = -1, xend = 0.5, y = 4, yend = 3),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  xlim(-1.5, 20.5) +
  annotate("text", x = -1, y = 4.4, label = "69%") +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "31%")

plot_hispanics <- plot_owner_perc("Hispanic")
plot_black <- plot_owner_perc("Black")

df <- data.frame(
  x = c(0),
  y = c(1)
)

text <- ggplot(df) +
  aes(
    x, y, label = "<span style = 'font-size:12pt;'>The share of Americans that own their home has been quite constant between 1976 and 2016. However, we observe significant disparities between the three main ethnic groups (White, Black, and Hispanic).</span>",
    hjust = 0, vjust = 1
  ) +
  geom_textbox(
    aes(valign = TRUE, box.colour = "white"), 
    width = unit(0.9, "npc"), 
    height = unit(0.9, "npc")
  ) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme_void() 
  


text + plot_hispanics + plot_white + plot_black + plot_layout(ncol = 2)






