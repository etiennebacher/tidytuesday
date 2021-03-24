library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(WDI)
library(tidytuesdayR)
library(pdftools)



##########
## Treat data ##
##########

### Get UN votes

tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

full_data <- full_join(tuesdata$unvotes, tuesdata$roll_calls, by = "rcid") %>% 
  full_join(tuesdata$issues, by = "rcid") %>% 
  filter(vote != "abstain") %>% 
  mutate(
    vote_result = case_when(
      vote == "yes" ~ 1,
      vote == "no" ~ 0,
      TRUE ~ NA_real_
    )
  )


### Get European countries

european_countries <- read.csv("https://raw.githubusercontent.com/jfjelstul/EU-infringement-data/master/data/member-state-years.csv") %>% 
  pull(member_state) %>% 
  unique


cleaned_data <- full_data %>% 
  filter(!is.na(issue) &
           country %in% c(european_countries, "Czechia", "Czechoslovakia") &
           country != "United Kingdom") %>% 
  mutate(
    country = ifelse(country == "Czechoslovakia", "Czech Republic", country),
    country = ifelse(country == "Czechia", "Czech Republic", country)
  ) %>% 
  select(country, issue, vote_result) %>%
  
  # Compute share of yes votes
  group_by(country, issue) %>% 
  mutate(
    share_yes = sum(vote_result) / n()
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
  ungroup() %>% 
  add_row(country = "***Palestinian conflict***", issue = "Palestinian conflict",
          rank = 0) %>% 
  add_row(country = "***Economic development***", issue = "Economic development",
          rank = 0) %>% 
  add_row(country = "***Colonialism***", issue = "Colonialism",
          rank = 0) %>% 
  add_row(country = "***Arms control and disarm.***", issue = "Arms control and disarmament",
          rank = 0) %>% 
  add_row(country = "***Nuclear weapons and material***", issue = "Nuclear weapons and nuclear material",
          rank = 0) %>% 
  add_row(country = "***Human rights***", issue = "Human rights",
          rank = 0) %>% 
  add_row(issue = "fake category", rank = 0) %>% 
  mutate(
    tweak_reorder = case_when(
      issue == "fake category" ~ 0.5,
      issue == "Arms control and disarmament" ~ 1,
      issue == "Nuclear weapons and nuclear material" ~ 2,
      issue == "Colonialism" ~ 3,
      issue == "Economic development" ~ 4,
      issue == "Human rights" ~ 5,
      issue == "Palestinian conflict" ~ 6,
      TRUE ~ NA_real_
    )
  )


##########
## Make plot ##
##########

bg_color <- "#333333"
text_color <- "#ff8c1a"

cleaned_data %>%
  mutate(
    country = ifelse(country == "France", 
                     '<b style="color:white;">France</b>',
                     country)
  ) %>% 
  ggplot(aes(x = tweak_reorder, y = rank)) +
  geom_point(alpha = 0) +
  geom_line(
    data = cleaned_data %>% 
             filter(country == "France") ,
    aes(
      x = tweak_reorder, 
      y = rank, 
      group = country
    ),
    inherit.aes = F,
    color = "white"
  ) +
  scale_y_reverse() +
  geom_richtext(
    aes(label = country),
    fill = bg_color, 
    color = text_color,
    label.color = bg_color
  ) +
  geom_segment(
    aes(x = 0.5, xend = 0.5, y = 20, yend = 8),
    arrow = arrow(length = unit(0.03, "npc")),
    color = text_color
  ) +
  annotate(
    geom = "text",
    x = 0.4, y = 14, angle = 90,
    label = "Higher is more frequent",
    color = text_color
  ) +
  xlim(0.4, 6.5) +
  labs(
    title = '<br>How frequently does <b style="color:white;">France</b> vote "yes" at the UN compared to other EU countries?<br>',
    subtitle = 'The countries are ordered by the share of "yes" votes in each category. The higher the country, the more frequent it votes "yes".\n Abstentions are excluded, and Czechoslovakia was recoded as Czech Republic.',
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
    plot.title = element_markdown(color = text_color, hjust = 0.5,
                              size = 26),
    plot.subtitle = element_text(color = text_color, hjust = 0.5,
                                 size = 13, vjust = 5),
    plot.caption = element_text(color = text_color)
  )


##########
## Export ##
##########

ggsave("R/2021/W13-un-votes/un-votes.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W13-un-votes/un-votes.pdf", 
            filenames = "R/2021/W13-un-votes/un-votes.png",
            format = "png", dpi = 350)
