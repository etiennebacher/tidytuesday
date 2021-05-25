library(data.table)
library(ggplot2)
library(tidytuesdayR)
library(extrafont)
library(pdftools)

# Takes a few minutes
# extrafont::font_import()

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)