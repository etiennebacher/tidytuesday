library(dplyr)
library(tidyr)
library(glue)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)
library(ggbump)
library(ggtext)



tuesdata <- tidytuesdayR::tt_load(2021, week = 11)


### Do something on the actors that play the most in movies with
### good Bechdel test writing ?

### Faire un graphique avec des sigmoides (voir https://github.com/Z3tt/TidyTuesday/blob/master/R/2021_02_TransitCosts.Rmd)
### pour comparer le classement des acteurs dans les films les mieux noté sur imdb et les films avec la meilleure note au bechdel test

data_cleaned <-
  tuesdata$movies %>%
  mutate(actors = strsplit(actors, ",")) %>%
  unnest(actors) %>%
  arrange(actors) %>%
  mutate(
    actors = gsub("^ ", "", actors),
    bechdel_rating = case_when(
      clean_test == "ok" ~ "3",
      clean_test == "men" ~ "2",
      clean_test == "notalk" ~ "1",
      clean_test == "nowomen" ~ "0",
      TRUE ~ NA_character_
    ),
    bechdel_rating = as.numeric(bechdel_rating)
  ) %>%
  relocate(bechdel_rating, .after = "clean_test") %>%
  group_by(actors) %>%
  mutate(
    n_movies = n(),
    avg_movie_rating = mean(imdb_rating, na.rm = TRUE),
    avg_bechdel_rating = mean(bechdel_rating, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(actors, n_movies, avg_movie_rating, avg_bechdel_rating) %>%
  filter(!is.na(actors), n_movies >= 4) %>%
  distinct() %>%
  arrange(desc(n_movies), desc(avg_bechdel_rating)) %>%
  slice_max(n_movies, n = 15, with_ties = FALSE) %>%

  # Create ranks for Bechdel
  arrange(desc(avg_bechdel_rating)) %>%
  mutate(rank_bechdel = row_number()) %>%

  # Create ranks for IMDB
  arrange(desc(avg_movie_rating)) %>%
  mutate(rank_imdb = row_number()) %>%
  mutate(
    change = rank_imdb - rank_bechdel
  ) %>%
  pivot_longer(
    cols = contains("rank"),
    names_to = "rank_type",
    values_to = "rank"
  ) %>% 
  mutate(
    rank_type = case_when(
      rank_type == "rank_imdb" ~ "1",
      rank_type == "rank_bechdel" ~ "2",
      TRUE ~ NA_character_
    ),
    rank_type = as.numeric(rank_type),
    
    actors_and_imdb = glue("**{actors}** ({round(avg_movie_rating, 1)})"),
    actors_and_bechdel = glue("**{actors}** ({round(avg_bechdel_rating, 1)})")
  )
  


bump_plot <- ggplot(
  data_cleaned, 
  aes(x = rank_type, y = rank, group = actors)
) +
  
  # Make basic bump plot
  geom_bump(
    aes(color = change),
    size = 1.5, lineend = "round"
  ) +
  geom_point(
    aes(color = change, size = 10)
  ) +
  
  # Add names 
  geom_richtext(
    data = data_cleaned %>% filter(rank_type == 1),
    aes(x = 0.95, y = rank, label = actors_and_imdb),
    fill = NA, 
    label.color = NA,
    hjust = 1
  ) +
  geom_richtext(
    data = data_cleaned %>% filter(rank_type == 2),
    aes(x = 2.05, y = rank, label = actors_and_bechdel),
    fill = NA, 
    label.color = NA,
    hjust = 0
  ) +
  
  # Color of lines
  scale_color_gradient2(
    low = "#b30000", 
    mid = "#999999", 
    high = "#009933", 
    midpoint = .02
  ) +
  
  # The lower the rank number, the better, so reverse the axis
  ylim(15, 1) +
  xlim(-0.5, 3.6) +

  # Arrow for IMDB side
  geom_segment(
    aes(x = -0.4, xend = -0.4, y = 12, yend = 3),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate(
    geom = "text",
    x = -0.48, y = 7.5, angle = 90,
    label = "IMDB ranking"
  ) +
  
  # Arrow for Bechdel side
  geom_segment(
    aes(x = 3.4, xend = 3.4, y = 12, yend = 3),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate(
    geom = "text",
    x = 3.49, y = 7.5, angle = -90,
    label = "Bechdel ranking"
  ) +
  
  # Title, subtitle, caption
  labs(
    title = "<br>**Comparing the top 15 actors/actresses with IMDB and Bechdel ranking**<br>",
    subtitle = "The Bechdel ranking is based on the Bechdel rating: a movie is rated from 0 (no women in the movie) to 3 (at least two women talking about something else \nthan men). This plot focuses on actors that have the highest average IMDB rating (which depends on the movies they have played in), and compares this \nranking with the Bechdel ranking.\n",
    caption = "Made by Etienne Bacher, with FiveThirtyEight data."
  ) +
  
  # Custom theme
  theme(
    
    # Remove useless text
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "",
    
    # Remove graduations
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # background
    plot.background = element_rect(fill = "#d9d9d9"),
    panel.background = element_rect(fill = "#d9d9d9"),
    
    # title, subtitle, caption
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_text(margin = margin(l = 100, r = 100)),
    plot.margin = margin(0, b = 50, r = 175, l = 175),
  )
