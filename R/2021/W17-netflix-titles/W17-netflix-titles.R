library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)


###########################
## Treat data ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)


# Get best rated movies
# Comes from: https://www.kaggle.com/aditya1303/imdb-top-50-movies
best_rated <- read.csv("R/2021/W17-netflix-titles/IMDB Top 50.csv") %>% 
  select(Title, Rating, Year) %>% 
  filter(Title %in% unique(tuesdata$netflix_titles$title))

# clean_data <- 
tuesdata$netflix_titles %>% 
  filter(title %in% best_rated$Title) %>% 
  select(title, date_added, release_year) %>% 
  mutate(
    date_added = substr(date_added, nchar(date_added)-3, nchar(date_added)),
    date_added = as.numeric(date_added),
    diff_date = date_added - release_year
  ) %>%
  ggplot(aes(y = fct_reorder(title, diff_date))) +
  geom_segment(
    aes(x = date_added, xend = release_year, yend = title),
    color = "#e50914",
    size = 2
  ) +
  geom_richtext(
    aes(
      x = release_year - nchar(title)/5,
      label = title,
    ),
    fill = NA,
    label.color = NA,
    text.color = "#F5F5F1"
  ) +
  geom_richtext(
    aes(
      x = 2022,
      label = as.character(diff_date),
    ),
    fill = NA,
    label.color = NA,
    text.color = "#F5F5F1"
  ) +
  geom_point(aes(x = date_added), color = "#F5F5F1", size = 4) +
  geom_point(aes(x = release_year), color = "#F5F5F1", size = 4)  +
  geom_curve(
    aes(x = 2030, xend = 2023, 
        y = "Pulp Fiction", yend = "Schindler's List"),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#F5F5F1",
    curvature = 0.2
  ) +
  annotate(
    "text",
    x = 2030.5,
    y = 9.6,
    label = paste0("Number of years between first release\n and availability on Netflix"),
    color = "white"
  ) +
  xlim(c(1990, 2035)) +
  theme(
    plot.background = element_rect(fill = "#221F1F"),
    panel.background = element_rect(fill = "#221F1F"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(color = "#5a5a3f", linetype = "dashed"),
    axis.text = element_text(color = "#F5F5F1"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Meera", size = 16)
  )
