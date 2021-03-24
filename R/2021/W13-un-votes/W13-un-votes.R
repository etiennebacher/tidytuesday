library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(WDI)
library(tidytuesdayR)
library(patchwork)
library(pdftools)
library(ggwordcloud)


### Get UN votes

tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

full_data <- full_join(tuesdata$unvotes, tuesdata$roll_calls, by = "rcid") %>% 
  full_join(tuesdata$issues, by = "rcid") %>% 
  mutate(
    vote_result = case_when(
      vote == "yes" ~ 1,
      vote == "no" ~ 0,
      vote == "abstain" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


### Get European countries

european_countries <- read.csv("https://raw.githubusercontent.com/jfjelstul/EU-infringement-data/master/data/member-state-years.csv") %>% 
  pull(member_state) %>% 
  unique


cleaned_data <- full_data %>% 
  filter(!is.na(issue), country %in% european_countries) %>% 
  mutate(
    year = as.numeric(substr(date, 1, 4))
  ) %>% 
  select(country, issue, vote_result) %>%
  
  # Compute share of yes votes
  group_by(country, issue) %>% 
  mutate(
    share_yes = sum(vote_result, na.rm = T) / n()
  ) %>% 
  ungroup() %>% 
  select(-vote_result) %>% 
  distinct() %>% 
  
  # Create ranking for each issue
  group_by(issue) %>% 
  arrange(desc(share_yes)) %>% 
  mutate(
    rank = row_number()
  ) %>% 
  ungroup() 



bg_color <- "#002266"
text_color <- "#ffffff"

cleaned_data %>%
  mutate(
    country = ifelse(country == "France", "**France**", country)
  ) %>% 
  ggplot(aes(x = issue, y = rank)) +
  geom_point(alpha = 0) +
  geom_line(
    data = cleaned_data %>% 
             filter(country == "France") ,
    aes(
      x = issue, 
      y = rank, 
      group = country
    ),
    inherit.aes = F,
    color = text_color
  ) +
  scale_y_reverse() +
  geom_richtext(
    aes(label = country),
    fill = bg_color, 
    color = text_color,
    label.color = bg_color
  ) +
  geom_segment(
    aes(x = 0.4, xend = 0.4, y = 20, yend = 8),
    arrow = arrow(length = unit(0.03, "npc")),
    color = text_color
  ) +
  annotate(
    geom = "text",
    x = 0.3, y = 14, angle = 90,
    label = "Ranking",
    color = text_color
  ) +
  labs(
    title = 'How frequently does France vote "yes" at the UN compared to other EU countries?',
    caption = "Made by Etienne Bacher | Data from Erik Voeten (2013)"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = bg_color),
    panel.background = element_rect(fill = bg_color),
    plot.title = element_text(color = text_color, hjust = 0.5),
    plot.caption = element_text(color = text_color)
  )


  ### FAIRE UN GRAPHIQUE AVEC DES COLONNES COMPOSEES DE NOMS DE PAYS
  ### 5 COLONNES AVEC LES ISSUES LES PLUS IMPORTANTES
  ### LES PAYS EN HAUT DE LA COLONNE SONT CEUX QUI ONT LE PLUS VOTE "OUI"
  ### SURLIGNER LA PLACE DE LA FRANCE DANS CES 5 COLONNES
  
  ### VOIR LE PLOT SIMILAIRE DE Z3TT (2020, W19)