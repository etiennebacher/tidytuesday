library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)


tuesdata <- tidytuesdayR::tt_load(2021, week = 11)


### Do something on the actors that play the most in movies with
### good Bechdel test writing ?

### Faire un graphique avec des sigmoides (voir https://github.com/Z3tt/TidyTuesday/blob/master/R/2021_02_TransitCosts.Rmd)
### pour comparer le classement des acteurs dans les films les mieux noté sur imdb et les films avec la meilleure note au bechdel test

tuesdata$movies %>% 
  mutate(actors = strsplit(actors, ",")) %>%
  unnest(actors) %>% 
  arrange(actors) %>% 
  select(year, title, test, clean_test, binary, actors)
