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


##########
## Treat the data ##
##########

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


##########
## Make bump plot ##
##########

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
    aes(x = 0, xend = 0, y = 12, yend = 3),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate(
    geom = "text",
    x = -0.08, y = 7.5, angle = 90,
    label = "IMDB ranking"
  ) +
  
  # Arrow for Bechdel side
  geom_segment(
    aes(x = 3, xend = 3, y = 12, yend = 3),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate(
    geom = "text",
    x = 3.09, y = 7.5, angle = -90,
    label = "Bechdel ranking"
  ) +
  
  # Title, subtitle, caption
  labs(
    title = paste0("<br>**", toupper("Comparing the top 15 actors/actresses with IMDB and Bechdel ranking"), "**<br>"),
    subtitle = "The Bechdel ranking is based on the Bechdel rating: a movie is rated from 0 (no women in the movie) to 3 (at least two women talking about something else than men). This plot focuses on \nactors that have the highest average IMDB rating out of 10 (which depends on the movies they have played in), and compares this ranking with the Bechdel ranking (out of 3). It focuses only \non the actors and actresses that have appeared in at least 4 movies.\n\n"
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
    plot.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
    panel.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
    
    # title, subtitle, caption
    plot.title = element_markdown("Roboto Condensed", hjust = 0.5, size = 16),
    text = element_text("Roboto Condensed")
  )


##########
## Make up and down arrows ##
##########

change_plot <- ggplot(data_cleaned) +
  xlim(0, 1) +
  ylim(0, 0.8) +

  # Red and green triangles
  geom_polygon(
    data = data.frame(
      triangle.x = c(0.2, 0.3, 0.25),
      triangle.y = c(0.6, 0.6, 0.45)
    ),
    aes(x = triangle.x, y = triangle.y),
    fill = "#b30000"
  ) +
  geom_polygon(
    data = data.frame(
      triangle.x = c(0.7, 0.8, 0.75),
      triangle.y = c(0.45, 0.45, 0.6)
    ),
    aes(x = triangle.x, y = triangle.y),
    fill = "#009933"
  ) +

  # descriptive text
  geom_richtext(
    data = data_cleaned %>%
      filter(actors == "x"),
    aes(x = 0.25, y = 0.38),
    label = "**Biggest decrease:**",
    fill = NA,
    label.color = NA,
    color = "#b30000"
  ) +
  geom_richtext(
    data = data_cleaned %>%
      filter(change == min(change)) %>%
      select(actors, change) %>%
      distinct() %>%
      mutate(
        change = as.character(change),
        change = case_when(
          as.numeric(change) > 0 ~ paste0("+", change),
          TRUE ~ change
        )
      ),
    aes(x = 0.25, y = 0.29, label = glue("{actors} ({change})")),
    fill = NA,
    label.color = NA,
    color = "#b30000"
  ) +
  geom_richtext(
    data = data_cleaned %>%
      filter(actors == "x"),
    aes(x = 0.75, y = 0.38),
    label = "**Biggest increase:**",
    fill = NA,
    label.color = NA,
    color = "#009933"
  ) +
  geom_richtext(
    data = data_cleaned %>%
      filter(change == max(change)) %>%
      select(actors, change) %>%
      distinct() %>%
      mutate(
        change = as.character(change),
        change = case_when(
          as.numeric(change) > 0 ~ paste0("+", change),
          TRUE ~ change
        )
      ),
    aes(x = 0.75, y = 0.29, label = glue("{actors} ({change})")),
    fill = NA,
    label.color = NA,
    color = "#009933"
  ) +
  labs(
    caption = "Made by Etienne Bacher, with FiveThirtyEight data."
  ) + 
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
    plot.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
    panel.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
    plot.caption = element_text(hjust = 0.5),
    text = element_text("Roboto Condensed")
  )


##########
## Assemble the two plots ##
##########

layout <- "
AAAA
AAAA
#BB#
"

bump_plot/change_plot + 
  plot_layout(design = layout) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9"),
      panel.background = element_rect(fill = "#d9d9d9", color = "#d9d9d9")
    )
  )
  
  
##########
## Export ##
##########

ggsave("R/2021/W11-bechdel-test/bechdel-test.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W11-bechdel-test/bechdel-test.pdf", 
            filenames = "R/2021/W11-bechdel-test/bechdel-test.png",
            format = "png", dpi = 350)
