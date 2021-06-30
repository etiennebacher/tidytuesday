library(data.table)
library(ggplot2)
library(forcats)
library(dplyr)
library(tibble)
library(tidytuesdayR)
library(extrafont)
library(pdftools)
library(rgdal)


# Takes a few minutes
# extrafont::font_import()


##########
## Treat the data ##
##########

tuesdata <- tidytuesdayR::tt_load(2021, week = 27)

london_boroughs <- readOGR(dsn = here::here("R/2021/W27-animal-rescue/LondonBoroughs.shp")) 


london_boroughs@data <- left_join(london_boroughs@data, tuesdata$animal_rescues, by = c('name' = 'borough'))


london_boroughs_f <- fortify(london_boroughs)
london_boroughs_f <- left_join(london_boroughs_f, london_boroughs@data) 



tuesdata$animal_rescues %>% 
  as.data.table() 
  
