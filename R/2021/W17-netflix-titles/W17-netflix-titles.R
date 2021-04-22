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

# Get 50 best rated movies from IMDB top 250
# https://www.imdb.com/chart/top
clean_data <- tuesdata$netflix_titles %>% 
  filter(title %in% c("Schindler's List", "The Lord of the Rings: The Return of the King", "Pulp Fiction", "Inception", "The Lord of the Rings: The Two Towers", "Inception", "The Matrix", "Goodfellas", "City of God", "American History X", "The Departed", "The Intouchables", "Once Upon a Time in the West")) %>% 
  select(title, date_added, release_year) %>% 
  mutate(
    date_added = substr(date_added, nchar(date_added)-3, nchar(date_added)),
    date_added = as.numeric(date_added),
    diff_date = date_added - release_year
  )


###########################
## Make the plot ##
###########################

clean_data %>%
  ggplot(aes(y = fct_reorder(title, diff_date))) +
  geom_segment(
    aes(x = date_added, xend = release_year, yend = title),
    color = "#e50914",
    size = 2
  ) +
  geom_richtext(
    aes(
      x = release_year - nchar(title)/3.5,
      label = title,
    ),
    fill = NA,
    label.color = NA,
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_richtext(
    aes(
      x = 2023,
      label = as.character(diff_date),
    ),
    fill = NA,
    label.color = NA,
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_point(aes(x = date_added), color = "#F5F5F1", size = 4) +
  geom_point(aes(x = release_year), color = "#F5F5F1", size = 4)  +
  
  ## Problem: need to have space on the right but that means that 2030 is 
  ## displayed whereas it shouldn't be
  ## 
  ## Solution: hide the axis on the right (for 2030) with a rectangle, remove
  ## x-axis text, and add manually annotations for 1990-2020
  geom_richtext(
    aes(
      x = 1960,
      y = 0.1
    ),
    label = "1960",
    fill = "#221F1F",
    label.color = "#221F1F",
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_richtext(
    aes(
      x = 1980,
      y = 0.1
    ),
    label = "1980",
    fill = "#221F1F",
    label.color = "#221F1F",
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_richtext(
    aes(
      x = 2000,
      y = 0.1
    ),
    label = "2000",
    fill = "#221F1F",
    label.color = "#221F1F",
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_richtext(
    aes(
      x = 2020,
      y = 0.1
    ),
    label = "2020",
    fill = "#221F1F",
    label.color = "#221F1F",
    text.color = "#F5F5F1",
    family = "Meera"
  ) +
  geom_rect(
    aes(xmin = 2029, xmax = 2031,
        ymin = 0, ymax = 12),
    fill = "#221F1F"
  ) +
  geom_rect(
    aes(xmin = 2039, xmax = 2041,
        ymin = 0, ymax = 12),
    fill = "#221F1F"
  ) +
  ###
  
  geom_curve(
    aes(x = 2030, xend = 2024, 
        y = 10, yend = 11),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#F5F5F1",
    curvature = 0.2
  ) +
  annotate(
    "text",
    x = 2030.5,
    y = 9.1,
    label = paste0("Number of years\n between first release\n and availability on Netflix"),
    color = "#F5F5F1",
    family = "Meera"
  ) +
  xlim(c(1950, 2035)) +
  labs(
    title = "\nHow many years before IMDB Top 50 movies are available on Netflix?\n",
    subtitle = "<p>These 11 movies are those in the IMDB top 50 which are also available on Netflix. The point on the left shows the date<br> of release, and the point on the right corresponds to the year it was added on Netflix.<br></p>",
    caption = "\nMade by Etienne Bacher | Data from Shivam Bansal on Kaggle"
  ) +
  theme(
    plot.background = element_rect(fill = "#221F1F"),
    panel.background = element_rect(fill = "#221F1F"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(color = "#5a5a3f", linetype = "dashed"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Meera", size = 16),
    plot.title = element_text(hjust = 0.5, size = 38, color = "#F5F5F1"),
    plot.subtitle = element_markdown(color = "#F5F5F1", hjust = 0.5),
    plot.caption = element_text(color = "#F5F5F1")
  ) 


###########################
## Export ##
###########################

ggsave("R/2021/W17-netflix-titles/netflix-titles.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W17-netflix-titles/netflix-titles.pdf", 
            filenames = "R/2021/W17-netflix-titles/netflix-titles.png",
            format = "png", dpi = 350)  
