###########################
## Tidytuesday 2021, Week 05 ##
###########################

library(tidyverse)
library(ggrepel)
library(ggtext)
library(countrycode)
library(WDI)


##########
## Get the data and play with it ##
##########

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
  ) %>%
  
  # Add a fake row so that there's a fake column on the graph (useful to 
  # get space for the labels)
  add_row(
    region = NULL, total_coca = 0, all_plastic = 0,
    coca_repartition = 0, id = 2
  )



ggplot(data = coca_plastic_per_region, 
       aes(x = id, y = coca_repartition, fill = region)) +
  geom_bar(position = "stack", stat = "identity", color = "black") +
  theme_void() +
  theme(
    legend.position = "none",
    rect = element_rect(fill = "#999999", colour = "#999999", size = 0.4, linetype = 1),
    panel.background = element_rect(fill = NA, colour = NA), 
    panel.border = element_rect(colour = "#999999", fill = NA, size = rel(1)),
    plot.background = element_rect(colour = NA),
    plot.title = element_markdown(color = "#d9d9d9", size = 18, 
                                  face = "bold", lineheight = 1.2,
                                  hjust = 0.4),
    plot.caption = element_markdown(color = "#d9d9d9", size = 8, 
                                    face = "bold", hjust = 0.4),
    plot.margin = unit(c(1,-3,1,1), "cm")
  ) + 
  # Get labels outside of the stacked bar
  geom_text_repel(
    aes(
      label = glue::glue("{region} ({coca_repartition}%)"),
      segment.linetype = "dotted",
      color = region
    ),
    # Many tries to have the right alignment
    hjust = c(70, 70, 70, 65, 70, 70, 80, 90, 130),
    position = position_stack(vjust = 0.5)
  ) +
  # Hide the line of ggrepel going on the second column (which is NULL by purpose)
  # adjust the coordinates before printing the plot
  geom_rect(aes(xmin = 1.5, xmax = 2.5, ymin = -5, ymax = 10), fill = "#999999") +
  # Add colors
  scale_fill_manual(
    values = c("#E61E2B", "#1E1E1E", "#E61E2B", "#1E1E1E", 
               "#E61E2B", "#1E1E1E", "#E61E2B", "#1E1E1E")
  ) +
  scale_color_manual(
    values = c("#E61E2B", "#1E1E1E", "#E61E2B", "#1E1E1E", 
               "#E61E2B", "#1E1E1E", "#E61E2B", "#1E1E1E")
  ) +
  labs(
    title = "Where does the plastic produced by  
    <b style='color:#E61E2B;'>Coca-</b><b style='color:#1E1E1E;'>Cola</b> go?",
    subtitle = "",
    caption = "Graph by <b style='color:#E61E2B;'> Etienne Bacher </b> &bull; 
    Data from <b style='color:#1E1E1E;'>Break Free from Plastic and Sarah Sauve</b>"
  )


ggsave("R/2021-W05-plastic-pollution/coca_plastic.png", width = 12, height = 7.5, dpi = 500)

