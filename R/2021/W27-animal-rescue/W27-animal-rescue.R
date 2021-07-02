library(data.table)
library(ggplot2)
library(ggtext)
library(patchwork)
library(stringr)
library(dplyr)
library(tidytuesdayR)
library(extrafont)
library(pdftools)
library(rgdal)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################


### Data for animal rescue

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)

cleaned_data <- tuesdata$animal_rescues %>% 
  as.data.table() %>% 
  .[, .N, by = .(animal_group_parent, borough)] 

# Fix borough name
cleaned_data[, borough := str_to_title(borough)] 
cleaned_data[, borough := gsub("Upon", "upon", borough)]
cleaned_data[, borough := gsub("And", "and", borough)]

# Aggregate by borough and animal, then remove duplicates
cleaned_data <- cleaned_data[, N := sum(N), 
                             by = .(borough, animal_group_parent)] %>% 
  unique()

most_rescued_animals <- cleaned_data %>% 
  .[, .(N = sum(N)), by = animal_group_parent] %>% 
  .[order(-N)] %>% 
  head(n = 5)

cleaned_data <- dcast(cleaned_data, 
                      borough ~ animal_group_parent, 
                      value.var = "N")

### Data for London map
### Code taken here: 
### https://towardsdatascience.com/visualising-crime-in-london-using-r-part-i-de7853c92ba8

london_boroughs <- readOGR(dsn = "R/2021/W27-animal-rescue/LondonBoroughs.shp")
london_boroughs@data <- left_join(london_boroughs@data, cleaned_data, by = c('name' = 'borough'))
london_boroughs_f <- fortify(london_boroughs)
london_boroughs$id <- row.names(london_boroughs)
london_boroughs_f <- left_join(london_boroughs_f, london_boroughs@data) 



###########################
## Make the plot ##
###########################


map_rescue <- function(animal) {
  ggplot(london_boroughs_f, aes_string("long", "lat", 
                                       group = "group", fill = animal)) +
    geom_polygon() + 
    geom_path(colour = "white", lwd = 0.05) + 
    coord_equal() +
    labs(x = "lat", y = "lon") +
    scale_fill_gradient2(
      low = "#ffffcc",
      high = "#666600",
      na.value = "white",
      name = ""
    ) + 
    theme_void() +
    theme(
      axis.text = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      legend.title = element_text(hjust = 0.5),
      legend.key.height = unit(0.3, 'cm'),
      plot.title = element_markdown(hjust = 0.5),
      text = element_text(color = "white", family = "Ubuntu Mono", size = 15)
    ) +
    ggtitle(paste0("<br>", animal)) 
}

for (i in most_rescued_animals$animal_group_parent) {
  assign(
    paste0("map_", i),
    map_rescue(i)
  )
}


layout <- 
"
AAAAABBDD
AAAAABBDD
AAAAACCEE
AAAAACCEE
"

map_Cat + map_Dog + map_Bird + map_Fox + map_Horse +
  plot_layout(design = layout) +
  plot_annotation(
    title = 'Number of animals rescued in London since 2009',
    caption = '<br>Made by Etienne Bacher | Data from London.gov',
    theme = theme(
      plot.title = element_markdown(hjust = 0.5, size = 22),
      plot.caption = element_markdown(hjust = 0.5),
      plot.background = element_rect(fill = "#476b6b"),
      text = element_text(color = "white", family = "Ubuntu Mono")
    )
  ) 



###########################
## Export ##
###########################

ggsave("R/2021/W27-animal-rescue/animal-rescue.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W27-animal-rescue/animal-rescue.pdf", 
            filenames = "R/2021/W27-animal-rescue/animal-rescue.png",
            format = "png", dpi = 350)   


