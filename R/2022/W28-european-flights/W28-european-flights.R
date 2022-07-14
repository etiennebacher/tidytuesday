library(dplyr)
library(tidyr)
library(lubridate)
library(circlize)
library(ggtext)
library(geojsonio)
library(sf)
library(raster)
library(broom)
library(luciole) # dreamRs/luciole
library(rgeos)
library(pdftools)
library(patchwork)
library(paletteer)

luciole::add_luciole()

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')



total <- flights %>%
  group_by(STATE_NAME) %>% 
  filter(min(YEAR) == 2016) %>% 
  ungroup() %>% 
  group_by(STATE_NAME, YEAR, MONTH_NUM) %>% 
  summarize(
    total_flights = sum(FLT_TOT_1 )
  ) %>% 
  ungroup() %>% 
  unite(col = "yearmonth", YEAR, MONTH_NUM) %>% 
  distinct() %>% 
  group_by(STATE_NAME) %>% 
  mutate(
    total_flights_index = total_flights[which(yearmonth == "2018_01")],
    total_flights_rel = total_flights - total_flights_index
  ) %>% 
  ungroup() %>% 
  group_by(STATE_NAME) %>% 
  mutate(share = total_flights/sum(total_flights, na.rm = TRUE)*100) %>% 
  ungroup() 


ggplot(data = total %>% filter(STATE_NAME != "France"), aes(x = yearmonth, y = share, group = STATE_NAME)) +
  geom_line(color = "grey") +
  geom_line(data = total %>% filter(STATE_NAME == "France"), color = "red") +
  geom_vline(xintercept = "2020_03", linetype = "dotted") +
  scale_x_discrete(
    breaks = paste0(2016:2022, "_01")
  ) +
  theme()



###########################
## Export ##
###########################

ggsave("R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.pdf", 
       width = 16, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.pdf", 
            filenames = "R/2022/W09-alternative-fuel-stations/alternative-fuel-stations.png",
            format = "png", dpi = 350)   
