library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(hrbrthemes)
library(waffle)
library(ggrepel)
library(glue)

tuesdata <- tidytuesdayR::tt_load(2021, week = 7)


# top_5 <-
plot_owner_perc <- function(race) {

  tuesdata$home_owner %>%
    filter(race == race) %>%
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
      values = c("#ffe6b3", "#cc9966"),
      labels = NULL
    ) +
    theme(legend.position = "none") + 
    geom_curve(
      aes(x = 8, xend = 5, y = 0.5, yend = -2),
      colour = "#cc9966",
      curvature = -0.4
    ) +
    geom_curve(
      aes(x = 20.5, xend = 20.5, y = 2, yend = -1),
      colour = "#ffe6b3",
      curvature = -0.4
    )

}

plot_owner_perc("White")
