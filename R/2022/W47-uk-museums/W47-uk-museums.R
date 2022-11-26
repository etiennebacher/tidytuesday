library(dplyr)
library(tidyr)
library(ggtext) 
library(ggplot2)
library(pdftools)
library(rnaturalearth)
library(sf)
library(biscale)
library(cowplot)

FONT <- "Alegreya"   

showtext::showtext_auto()
sysfonts::font_add_google(FONT)


##############
## Treat data ##
##############

museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') |> 
  filter(Latitude < 80) |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) 

eng <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') |>
  filter(admin == "United Kingdom") %>% 
  st_transform(crs = 5643)


# Create the hexagonal grid
# Comes from https://github.com/jvieroe/dataviz/blob/main/2021/plague_british/code_plague_british.R
grid_sf <- eng %>% 
  st_make_grid(.,
               cellsize = c(50000, 50000),
               square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number()) %>% 
  st_intersection(eng)

eng <- eng %>% 
  st_transform(crs = 4326)

grid_sf <- grid_sf %>% 
  st_transform(crs = 4326) 


# For each hexagon, count the number of museums and compute the mean of the income
# index
x <- st_intersects(grid_sf, museums)
test <- lapply(x, function(y) {
  if (length(y) == 0) data.frame(n = NA, mean_inc = NA)
  
  tmp <- museums[y, ]
  tmp |> 
    summarise(
      n = n(),
      mean_inc = mean(Area_Deprivation_index_income)
    )
})

out <- do.call(rbind, test) |> 
  bi_class(x = n, y = mean_inc, style = "quantile", dim = 3) |> 
  select(-geometry)


# Unite this data with the grid data
grid_sf <- bind_cols(grid_sf, out)




##############
## Make the map ##
##############

bi_map <-
  ggplot() +
  geom_sf(data = grid_sf, aes(fill = bi_class), color = "white", size = 0.3) +
  bi_scale_fill(pal = "BlueOr", dim = 3, guide = F) +
  xlim(c(-13, 4)) +
  geom_richtext(
    data = data.frame(x = -12.3, y = 59.5),
    aes(x, y),
    label = "<span style='color: #7ebbd2'>Income index</span> and <span style='color: #d8a386'>number <br> of museums </span> in <br>the UK",
    size = 8,
    family = FONT,
    hjust = 0,
    fill = NA, label.color = NA
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#e6e6e6")
  )  

legend <- 
  bi_legend(pal = "BlueOr",
            dim = 3,
            xlab = "# of museums ",
            ylab = "Income index") +
  bi_theme(base_family = FONT) +
  theme(
    plot.background = element_rect("#e6e6e6", colour = "#e6e6e6"),
    panel.background = element_rect("#e6e6e6", colour = "#e6e6e6"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_text(size = 12, color = "#d8a386"),
    axis.title.y = element_text(size = 12, color = "#7ebbd2")
  )

ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.25, 0.25)




###########################
## Export ##
###########################

ggsave("R/2022/W47-uk-museums/uk-museums.pdf", 
       width = 7.15, height = 8, device = cairo_pdf)

pdf_convert(pdf = "R/2022/W47-uk-museums/uk-museums.pdf", 
            filenames = "R/2022/W47-uk-museums/uk-museums.png",
            format = "png", dpi = 350)   

