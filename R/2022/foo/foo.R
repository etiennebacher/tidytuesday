library(dplyr)
library(tidyr)
library(ggtext) 
library(ggplot2)
library(pdftools)
library(rnaturalearth)
library(sf)
library(biscale)

showtext::showtext_auto()
sysfonts::font_add_google("Cormorant Garamond")

museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') |> 
  filter(Latitude < 80) |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) 

eng <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') |>
  filter(admin == "United Kingdom")


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
  bi_class(x = n, y = mean_inc, style = "quantile", dim = 3)



eng <- eng %>% 
  st_transform(crs = 5643)

grid_sf <- eng %>% 
  st_make_grid(.,
               cellsize = c(25000, 25000),
               square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number())

grid_sf <- grid_sf %>% 
  st_intersection(eng)

eng <- eng %>% 
  st_transform(crs = 4326)

grid_sf <- grid_sf %>% 
  st_transform(crs = 4326) 


grid_sf <- grid_sf |> 
  mutate(
    n_museums = lengths(st_intersects(grid_sf, out))
  )



ggplot() +
  geom_sf(data = grid_sf, aes(fill = bi_class), color = "white", size = 0.3) +
  # scale_fill_viridis_c(option = "rocket", direction = -1) +
  bi_scale_fill(pal = "DkViolet", dim = 3, guide = F) 
