library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(tidytuesdayR)
library(pdftools)
library(ggrepel)



###########################
## Treat data ##
###########################

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)


clean_data <-
  tuesdata$vegetable_oil %>% 
  filter(entity %in% c("Asia", "Africa", "Europe", "Northern America", 
                       "South America", "Oceania")) %>% 
  rename(region = entity) %>% 
  select(region, year, production) %>%
  group_by(year, region) %>% 
  mutate(
    yearly_prod = sum(production, na.rm = T)
  ) %>% 
  select(-production) %>% 
  ungroup() %>% 
  distinct() %>%
  group_by(year) %>% 
  mutate(
    highest_2014 = max(yearly_prod),
    highest_2014 = ifelse(year == 2014 & yearly_prod == highest_2014, 1, 0),
    lowest_2014 = min(yearly_prod),
    lowest_2014 = ifelse(year == 2014 & yearly_prod == lowest_2014, 1, 0),
  ) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(
    highest_2014 = ifelse(max(highest_2014) == 1, 1, 0),
    lowest_2014 = ifelse(max(lowest_2014) == 1, 1, 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    label = case_when(
      (region == "Europe" | highest_2014 == 1 | lowest_2014 == 1)
      & year == 2014 ~ paste0(region, " (", round(yearly_prod/1000000, 1), "M)"),
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(region) %>% 
  mutate(
    val_1961 = ifelse(year == 1961, yearly_prod, 0),
    val_2014 = ifelse(year == 2014, yearly_prod, 0),
    growth = (max(val_2014) - max(val_1961))/max(val_1961)*100,
    growth_label = case_when(
      region == "Asia" ~ paste0("+", round(growth, 1), "% between 1961 and 2014"),
      TRUE ~ paste0("+", round(growth, 1), "%")
    )
  ) %>% 
  ungroup()
  
  

###########################
## Make the plot ##
###########################

eu_color = "darkblue"
low_color = "red"
high_color = "darkgreen"
other_color = "#808080"
font = "DejaVu Serif"

clean_data %>% 
  filter(region == "Europe" | highest_2014 == 1 | lowest_2014 == 1) %>% 
  ggplot(aes(x = year, y = yearly_prod)) +
  geom_line(
    data = clean_data %>% 
      filter(region != "Europe" | highest_2014 == 0 | lowest_2014 == 0),
    aes(group = region),
    color = other_color
  ) +
  geom_line(
    data = clean_data %>% 
      filter(region == "Europe"),
    color = eu_color
  ) +
  geom_line(
    data = clean_data %>% 
      filter(highest_2014 == 1),
    color = high_color
  ) +
  geom_line(
    data = clean_data %>% 
      filter(lowest_2014 == 1),
    color = low_color
  ) +
  
  ### Labels on the right
  annotate(
    "text",
    x = 2016.6, y = 9.7*10^7,
    label = clean_data %>% 
      filter(highest_2014 == 1, year == 2014) %>% 
      pull(label),
    color = high_color,
    family = font
  ) +
  annotate(
    "text",
    x = 2017, y = 3*10^7,
    label = clean_data %>% 
      filter(region == "Europe", year == 2014) %>% 
      pull(label),
    color = eu_color,
    family = font
  ) +
  annotate(
    "text",
    x = 2017, y = 1.5*10^6,
    label = clean_data %>% 
      filter(lowest_2014 == 1, year == 2014) %>% 
      pull(label),
    color = low_color,
    family = font
  ) +
  
  ### Labels percentage
  annotate(
    "text",
    x = 1992, y = 3.3*10^7,
    label = clean_data %>% 
      filter(region == "Asia") %>% 
      pull(growth_label) %>% 
      unique,
    color = high_color,
    angle = 28,
    family = font
  ) +
  annotate(
    "text",
    x = 1997, y = 1.6*10^7,
    label = clean_data %>% 
      filter(region == "Europe") %>% 
      pull(growth_label) %>% 
      unique,
    color = eu_color,
    angle = 5,
    family = font
  ) +
  annotate(
    "text",
    x = 1995, y = 2.1*10^6,
    label = clean_data %>% 
      filter(region == "Oceania") %>% 
      pull(growth_label) %>% 
      unique,
    color = low_color,
    angle = 0,
    family = font
  ) +
  
  ### Title and caption
  geom_richtext(
    data = tibble(year = 1980, yearly_prod = 8*10^7),
    label = paste0("<span style = 'font-family:", '"', font, '"', "'>Evolution of the production of vegetable oil in <p style='color: ", high_color, "'>Asia</p>,<br> <p style='color: ", eu_color, "'>Europe</p>, <p style='color: ", low_color, "'>Oceania</p>, and <p style='color: ", other_color, "'>other continents</p>.</span>"),
    size = 9,
    label.colour = NA,
    fill = NA,
    lineheight = 1.6
  ) +
  labs(
    caption = "\n\nMade by Etienne Bacher | Data from ourworldindata.org"
  ) + 
  ylab("Production (in tons)") +
  
  ### Axis and theme
  scale_x_continuous(
    breaks = c(seq(1961, 2011, 10), 2014),
    limits = c(1961, 2018)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100000000, 25000000),
    labels = paste0(seq(0, 100, 25), "M"),
    limits = c(0, 110000000)
  ) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#f2f2f2"),
    plot.background = element_rect(fill = "#f2f2f2"),
    panel.grid.major = element_line(color = "#e6e6e6"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(family = font)
  )


###########################
## Export ##
###########################

ggsave("R/2021/W15-deforestation/deforestation.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W15-deforestation/deforestation.pdf", 
            filenames = "R/2021/W15-deforestation/deforestation.png",
            format = "png", dpi = 350)  
