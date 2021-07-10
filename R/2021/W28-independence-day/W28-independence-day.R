library(data.table) 
library(ggplot2)
library(ggtext)
library(patchwork)
library(maps)
library(magrittr)
library(png)
library(grid)
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

world <- map_data("world") 

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

# Get flags 
img_fr <- readPNG("R/2021/W28-independence-day/FR.png")
g_fr <- rasterGrob(img_fr, interpolate=TRUE)
img_uk <- readPNG("R/2021/W28-independence-day/GB.png")
g_uk <- rasterGrob(img_uk, interpolate=TRUE)
img_es <- readPNG("R/2021/W28-independence-day/ES.png")
g_es <- rasterGrob(img_es, interpolate=TRUE)

# x = country, y = year
map_country <- function(x, y) {
  tmp <- data_cleaned[independence_from == x & year > y]
  coord_x <- capitals[country == x, .(lat, long)]
  
  # Detect colonial link (to color countries)
  world2 <- as.data.table(world)
  world2[, colony := as.factor(
    fifelse(region %in% unique(tmp$country) | region == x, 1, 0)
  )]
  
  fill_color <- if (x == "France") {
    "#0055a4"
  } else if (x == "United Kingdom") {
    "#012169"
  } else {
    "red"
  }
  
  flag <- if (x == "France") {
    g_fr
  } else if (x == "United Kingdom") {
    g_uk
  } else {
    g_es
  }
    
  tmp_p <- ggplot() +
    geom_map(
      data = world2, map = world2,
      aes(long, lat, map_id = region, fill = colony)
    ) +
    geom_richtext(
      aes(x = 0, y = -53),
      label = as.character(y),
      size = 7,
      family = "Tangerine",
      fontface = "bold"
    ) +
    scale_fill_discrete(type = c("white", fill_color)) +
    annotation_custom(flag, xmin=-200, xmax=-150, ymin=70, ymax=90) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#85adad"),
      legend.position = "none"
    )
  
  for (i in unique(tmp$country)) {
    coord_y <- tmp[country == i, .(lat, long)]
    coords <- rbind(coord_x, coord_y)
    
    tmp_p <- tmp_p +
      geom_line(
        data = coords,
        aes(x = long, y = lat),
        color = "black",
        alpha = 0.4
      )
  }
   
  return(tmp_p)
}


fr_1800 <- map_country("France", 1800)
fr_1900 <- map_country("France", 1900)
fr_1975 <- map_country("France", 1975)

es_1800 <- map_country("Spain", 1800)
es_1900 <- map_country("Spain", 1900)
es_1975 <- map_country("Spain", 1975)

uk_1800 <- map_country("United Kingdom", 1800)
uk_1900 <- map_country("United Kingdom", 1900)
uk_1975 <- map_country("United Kingdom", 1975)

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
    title = 'The loss of influence of empires through time',
    subtitle = "The United Kingdom, France, and Spain were the three main European colonial empires. The lines link the colonies with the country that controls them.",
    caption = 'Made by Etienne Bacher | Data from Wikipedia',
    theme = theme(
      plot.title = element_markdown(hjust = 0.5, size = 45),
      plot.subtitle = element_text(hjust = 0.5, size = 25),
      plot.caption = element_text(hjust = 0.5, size = 15),
      plot.background = element_rect(fill = "#334d4d"),
      text = element_text(color = "white", family = "Tangerine",
                          face = "bold")
    )
  ) 


###########################
## Export ##
###########################

ggsave("R/2021/W28-independence-day/independence-day.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W28-independence-day/independence-day.pdf", 
            filenames = "R/2021/W28-independence-day/independence-day.png",
            format = "png", dpi = 350)   
