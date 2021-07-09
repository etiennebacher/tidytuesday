library(data.table) 
library(ggplot2)
library(ggtext)
library(patchwork)
library(maps)
library(magrittr)
library(geosphere)
library(extrafont)
library(pdftools)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################

holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv') %>% 
  as.data.table() %>% 
  .[, .(country, year, independence_from)]

holidays[, independence_from := fifelse(independence_from == "Spanish Empire", 
                                        "Spain",
                                        independence_from)]
holidays[, independence_from := fifelse(independence_from == "Spanish Empire[72]",
                                        "Spain",
                                        independence_from)]

holidays <- holidays[independence_from %in% c("Spain", "United Kingdom", "France")]

world <- map_data("world") %>% 
  as.data.table()

capitals <- maps::world.cities %>% 
  as.data.table() %>% 
  .[capital == 1, .(country = country.etc, lat, long)] 

capitals[, country := ifelse(country == "UK", "United Kingdom", country)] 
  
capitals <- capitals[country %in% unique(c(holidays$country, 
                                           holidays$independence_from))]


data_cleaned <- merge(
  holidays, capitals,
  by = "country"
)


##########
## Make plots ##
##########

maps_colonial <- list()

# x = country, y = year
map_country <- function(x, y) {
  tmp <- data_cleaned[independence_from == x & year > y]
  coord_x <- capitals[country == x, .(lat, long)]
  
  world2 <- world
  world2[, colony := fifelse(region %in% unique(tmp$country), 1, 0)]
  
  fill_color <- if (x == "France") {
    "#0055a4"
  } else if (x == "United Kingdom") {
    "#012169"
  } else {
    "red"
  }
    
  tmp_p <- ggplot() +
    geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region, fill = colony)
    ) +
    geom_richtext(
      aes(x = 0, y = -55),
      label = as.character(y),
      size = 5
    ) +
    scale_fill_gradientn(colours = c("white", fill_color), values = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#85adad"),
      legend.position = "none"
    )
  
  for (i in unique(tmp$country)) {
    coord_y <- tmp[country == i]
    coords <- gcIntermediate(c(coord_y$long[1], coord_y$lat[1]), 
                   c(coord_x$long[1], coord_x$lat[1]), 
                   n=25, addStartEnd=TRUE) %>% 
      as.data.frame()
    
    tmp_p <- tmp_p +
      geom_line(
        data = coords,
        aes(x = lon, y = lat),
        color = "black",
        alpha = 0.4
      )
  }
   
  return(tmp_p)
}

for (i in c("France", "Spain", "United Kingdom")) {
  for (j in c(1800, 1900, 1975)) {
    maps_colonial[[paste0(i, "_", j)]] <- map_country(i, j)
  }
}


layout <- 
"
ABC
DEF
GHI
"

uk_1800 + uk_1900 + uk_1975 +
  fr_1800 + fr_1900 + fr_1975 +
  es_1800 + es_1900 + es_1975 +
  plot_layout(design = layout) +
  plot_annotation(
    title = 'When did countries get their independence?',
    subtitle = "The United Kingdom, France, and Spain were the three main European colonial empires. The lines link the colonies with the country that controls them.",
    caption = 'Made by Etienne Bacher | Data from Wikipedia',
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 22),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#334d4d"),
      text = element_text(color = "white")
    )
  ) 

cowplot::plot_grid(
  uk_1800, uk_1900, uk_1975,
    fr_1800, fr_1900, fr_1975,
    es_1800, es_1900, es_1975, ncol = 3
)
