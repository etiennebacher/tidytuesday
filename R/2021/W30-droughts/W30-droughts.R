library(data.table)
library(ggplot2)
library(ggtext)
library(patchwork)
library(tidytuesdayR)
library(extrafont)
library(pdftools)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################


### Data for animal rescue

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)

tuesdata$drought %>% 
  View
