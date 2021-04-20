library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
# devtools::install_github("cran/gettingtothebottom")
library(gettingtothebottom)
library(tidytuesdayR)
library(pdftools)


###########################
## Treat data ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)


# Get best rated movies
best_rated <- gettingtothebottom::movieratings %>% 
  select(title, rating, year) %>% 
  filter(year >= min(tuesdata$netflix_titles$release_year),
         title %in% unique(tuesdata$netflix_titles$title),
         rating >= 8.5)

# clean_data <- 
tuesdata$netflix_titles %>% 
  filter(title %in% best_rated$title) %>% 
  select(title, date_added, release_year) %>% 
  mutate(
    date_added = substr(date_added, nchar(date_added)-3, nchar(date_added)),
    date_added = as.numeric(date_added),
    diff_date = date_added - release_year
  ) %>% 
  pivot_longer(
    cols = c("date_added", "release_year")
  ) %>% 
  ggplot(aes(value, fct_reorder(title, diff_date), color = name)) +
  geom_point()
### Faire un plot un peu comme C. Scherer 2021, week 1, mais avec la différence entre release_year et date_added