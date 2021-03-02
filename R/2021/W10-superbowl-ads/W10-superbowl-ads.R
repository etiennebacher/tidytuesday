library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytuesdayR)
library(patchwork)
library(pdftools)


tuesdata <- tidytuesdayR::tt_load(2021, week = 10)

### Play with the data

tuesdata_cleaned <- tuesdata$youtube %>% 
  select(
    -all_of(contains("url")), 
    -c(id, etag, kind, published_at, thumbnail, channel_title, category_id)
  ) %>% 
  mutate(brand = ifelse(brand == "Hynudai", "Hyundai", brand),
         across(where(is.logical),
                as.numeric)) %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  summarise(
    across(c(patriotic, animals, danger, use_sex, celebrity),
           .fns = list(sum), .names = "{.col}_number")
    
  ) %>% 
  pivot_longer(
    cols = ends_with("number"),
    names_to = "type",
    values_to = "value"
  )


### Create theme

my_theme <- function() {
  theme(
    ### Background
    plot.background = element_rect(fill = "#1f3d7a", color = "#1f3d7a"),
    panel.background = element_rect(fill = "#1f3d7a", color = "#1f3d7a"),
    
    ### Text
    plot.title = element_text(size = rel(1.2), hjust = 0.5, color = "#ffffff"),
    plot.caption = element_text(color = "#ffffff"),
    axis.text = element_text(color = "#ffffff"),
    axis.title = element_text(color = "#ffffff"),
    
    
    ### Graduations
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank()
  )
}


### Produce the plots

make_plot <- function(type_name) {
  
  tuesdata_cleaned %>% 
    filter(grepl(type_name, type)) %>% 
    ggplot(aes(x = year, y = value)) +
    geom_line(color = "#ffffff") +
    scale_y_continuous(
      breaks = seq(0, 10, by = 2), 
      limits = c(0, 10)
    ) +
    labs(x = "", y = "",
         title = str_to_sentence(type_name)) +
    my_theme()
  
}

plot_animals <-  make_plot("animals")
plot_sex <-  make_plot("sex")
plot_patriotic <-  make_plot("patriotic")
plot_danger <-  make_plot("danger")
plot_celebrity <-  make_plot("celebrity")
  


### Arrange with patchwork

layout <- "
#AABB#
CCDDEE
"

plot_animals + 
  plot_celebrity + 
  plot_danger +
  plot_patriotic + 
  plot_sex + 
  plot_layout(design = layout) &
  plot_annotation(
    title = "How many SuperBowl ads contain / are...\n",
    caption = "Made by Etienne Bacher, with FiveThirtyEight data",
    theme = theme(
      plot.title = element_text(size = rel(1.5), 
                                hjust = 0.5, color = "#ffffff"),
      plot.caption = element_text(color = "#ffffff"),
      plot.background = element_rect(fill = "#1f3d7a", color = "#1f3d7a"),
      panel.background = element_rect(fill = "#1f3d7a", color = "#1f3d7a")
    )
  ) 


### Export

ggsave("R/2021/W10-superbowl-ads/superbowl-ads.pdf", 
       width = 13.3, height = 13.5, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W10-superbowl-ads/superbowl-ads.pdf", 
            filenames = "R/2021/W10-superbowl-ads/superbowl-ads.png",
            format = "png", dpi = 350)
