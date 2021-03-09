library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)
library(ggbump)



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

  pivot_longer(
    cols = contains("rank"),
    names_to = "rank_type",
    values_to = "rank"
  ) 



### Color the lines according to the degree of increase (green)/decrease (red)

ggplot(data_cleaned, aes(x = rank_type, y = rank, group = actors)) +
  geom_bump(aes(smooth = 10), size = 1.5, lineend = "round") +
  geom_label(aes(label = actors)) 
