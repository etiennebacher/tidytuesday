###########################
## Tidytuesday 2021, Week 05 ##
###########################

library(tidyverse)
library(ggrepel)
library(ggtext)
library(countrycode)
library(WDI)

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)$plastics

# Get ISO codes and regions
some_wdi <- WDI(country = "all", indicator = "TM.TAX.TCOM.BC.ZS",
                start = 1999, end = 2000, extra = TRUE) %>% 
  select(iso3c, region)

# Get ISO codes of plastic data and merge with some_wdi to have the regions
tuesdata_2 <- tuesdata %>% 
  mutate(iso3c = countrycode(sourcevar = country, 
                             origin = "country.name",
                             destination = "iso3c")) %>% 
  left_join(some_wdi, by = "iso3c") %>%
  relocate(iso3c, region)


# Get the number of plastic from Coca-Cola per continent
coca_plastic_per_region <- tuesdata_2 %>% 
  distinct() %>% 
  # Get Coca companies
  filter(grepl("Coca", parent_company) == TRUE) %>% 
  select(region, parent_company, grand_total) %>% 
  # Total number of plastics from Coca
  mutate(total_coca = sum(grand_total, na.rm = T)) %>% 
  # Number of plastics from Coca per region
  group_by(region) %>% 
  mutate(all_plastic = sum(grand_total, na.rm = T)) %>% 
  ungroup() %>% 
  select(- grand_total, - parent_company) %>% 
  distinct() %>% 
  # Repartition of plastics from Coca between regions
  mutate(coca_repartition = round(all_plastic/total_coca*100, 2),
         id = 1,
         region = fct_explicit_na(region, na_level = "Other regions"),
         region = fct_reorder(region, coca_repartition)
  ) 


# Get bottle image
# img_b <- png::readPNG("2021-W05/coca-contour.png") 
# bottle <- grid::rasterGrob(img_b, interpolate = T) 


ggplot(data = coca_plastic_per_region, 
       aes(x = id, y = coca_repartition, fill = region)) +
  geom_bar(position = "stack", stat = "identity", width = 0.1) +
  theme_void() + 
  theme(legend.position = "none",
        legend.margin = margin(c(10, 70, 5, 5))) + 
  geom_text_repel(
    aes(label = region),
    position = position_stack(vjust = 1),
    size = 4,
    hjust = 40,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  )



  ylab("Total number of plastics") +
  xlab("") +
  coord_flip() +
  labs(title = "Number of plastics per country",
       caption = "Made by Etienne Bacher. Data from Sarah Sauve.") +
  geom_text(aes(label = grand_total_2)) 
