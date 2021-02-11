library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(hrbrthemes)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(ggrepel)
library(glue)
library(ggtext)
library(ggpubr)

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
      labels = c("Non-owner", "Owner"),
      guide = guide_legend(reverse=TRUE)
    ) +
    xlim(-1.5, 21.5) +
    ylim(-1, 7) +
    theme(legend.position = "none") +
    ggtitle(ethnic) +
    theme(plot.title = element_text(hjust = 0.55, size = 10))

}

plot_white <- plot_owner_perc("White") +
  geom_curve(
    aes(x = -1, xend = 0.5, y = 4, yend = 3),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = -1, y = 4.4, label = "69%", colour = "#660000") +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "31%", colour = "#cc9966") +
  theme(legend.position = "bottom")

plot_hispanics <- plot_owner_perc("Hispanic") +
  geom_curve(
    aes(x = 3, xend = 5, y = -1, yend = 0.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = 2.4, y = -1, label = "44%", colour = "#660000") +
  geom_curve(
    aes(x = 21.5, xend = 20.5, y = 0.5, yend = 2.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 21.4, y = -0.1, label = "56%", colour = "#cc9966")

plot_black <- plot_owner_perc("Black") +
  geom_curve(
    aes(x = 3, xend = 4, y = 6.5, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = -0.4
  ) +
  annotate("text", x = 2.4, y = 6.5, label = "45%", colour = "#660000") +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "55%", colour = "#cc9966")


# Add the quarter with the text
df <- data.frame(
  x = 0,
  y = 5
)

text <-
  ggplot(df) +
  aes(
    x, y, label = "<span style = 'font-size:12pt;'>The share of Americans that own their home has been quite constant between 1976 and 2016. However, we observe significant disparities between the three main ethnic groups (White, Black, and Hispanic).</span>",
    hjust = -0.1, vjust = 1.1, fill = "#f0e68c"
  ) +
  geom_textbox(
    aes(valign = TRUE, color = "black"), 
    width = unit(0.8, "npc"), 
    height = unit(0.3, "npc")
  ) +
  xlim(0, 20) + 
  ylim(0, 7) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
  theme_void()
  

all_plots <- ggpubr::ggarrange(text, plot_hispanics, plot_white, plot_black, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom") 

annotate_figure(all_plots, 
                top = text_grob("House ownership in the US", 
                                face = "bold",
                                size = 15)
) +
  bgcolor("#f9f5d2")




