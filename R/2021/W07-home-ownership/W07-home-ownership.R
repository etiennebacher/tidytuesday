library(dplyr)
library(tidyr)
library(tidytuesdayR)
library(hrbrthemes)
# devtools::install_github("hrbrmstr/waffle")
library(waffle)
library(ggrepel)
library(glue)
library(ggtext)
library(ggpubr)
library(pdftools)

tuesdata <- tidytuesdayR::tt_load(2021, week = 7)


###########################
## FIRST CHOICE ##
###########################


# Function to plot the percentage of home owner per ethnic group
plot_owner_perc <- function(ethnic) {

  tuesdata$home_owner %>%
    filter(race == ethnic) %>%
    mutate(owner = round(mean(home_owner_pct, na.rm = T)*100, 0),
           non_owner = 100-owner) %>%
    ungroup() %>%
    pivot_longer(
      cols = c("owner", "non_owner"),
      names_to = "is_owner"
    ) %>%
    select(is_owner, value) %>%
    distinct() %>%
    ggplot(aes(fill = is_owner, values = value)) +
    geom_waffle(n_rows = 5) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() +
    scale_fill_manual(
      name = NULL,
      values = c("#cc9966", "#660000"),
      labels = c("Non-owner", "Owner"),
      guide = guide_legend(reverse=TRUE)
    ) +
    xlim(-1.5, 21.5) +
    ylim(-1, 7) +
    theme(legend.position = "none") +
    ggtitle(ethnic) +
    theme(
      plot.title = element_text(hjust = 0.55, size = 15),
      text = element_text(family = "Montserrat"),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 15),
      plot.margin = unit(c(-5, 1, -5, 1), "lines")
    )

}

# Apply function to the three groups and add percentage with segments

plot_white <- plot_owner_perc("White") +
  geom_curve(
    aes(x = -1, xend = 0.5, y = 4, yend = 3),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = -1, y = 4.4, label = "69%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "31%", colour = "#cc9966", size = 5) +
  theme(legend.position = "bottom")

plot_hispanics <- plot_owner_perc("Hispanic") +
  geom_curve(
    aes(x = 3, xend = 5, y = -1, yend = 0.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = 2.2, y = -1, label = "44%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 21.5, xend = 20.5, y = 0.5, yend = 2.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 21.4, y = -0.1, label = "56%", colour = "#cc9966", size = 5)

plot_black <- plot_owner_perc("Black") +
  geom_curve(
    aes(x = 3, xend = 4, y = 6.5, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = -0.4
  ) +
  annotate("text", x = 2.2, y = 6.5, label = "45%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "55%", colour = "#cc9966", size = 5)


# Create an empty plot containing only text

df <- data.frame(
  x = 0,
  y = 5
)

text <-
  ggplot(df) +
  aes(
    x, y, label = "<p style = 'font-size:16pt;'>Although the share of Americans that own their home has been quite constant between 1976 and 2016, we observe significant disparities between the three main ethnic groups (White, Black, and Hispanic).</p>",
    hjust = -0.2, vjust = 1.1, fill = "#f9f5d2"
  ) +
  geom_textbox(
    aes(valign = TRUE, box.color = "#f9f5d2"), 
    width = unit(0.67, "npc"), 
    height = unit(0.43, "npc")
  ) +
  xlim(0, 20) + 
  ylim(0, 7) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
  theme_void()
  

# Arrange plots

all_plots <- ggpubr::ggarrange(text, plot_hispanics, plot_white, plot_black, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom") 

# Last modifications of the composition

annotate_figure(
  all_plots, 
  top = text_grob("\nHome ownership in the US\n", 
                  face = "bold",
                  size = 22,
                  family = "Montserrat"),
  bottom = text_grob(paste0("\nVisualization by Etienne Bacher. Data from the",
                            " Urban Institute and the US Census.\n"), size = 15)
  ) +
  bgcolor("#f9f5d2")

# Export

ggsave("R/2021/W07-wealth-income/home-ownership.pdf", 
       width = 16.3, height = 9.5, device = cairo_pdf)


pdf_convert(pdf = "R/2021/W07-wealth-income/home-ownership.pdf", 
            filenames = "R/2021/W07-wealth-income/home-ownership.png",
            format = "png", dpi = 350)







###########################
## SECOND CHOICE (not exported) ##
###########################


# Function to plot the percentage of home owner per ethnic group
plot_owner_perc <- function(ethnic) {
  
  tuesdata$home_owner %>%
    filter(race == ethnic) %>%
    mutate(owner = round(mean(home_owner_pct, na.rm = T)*100, 0),
           non_owner = 100-owner) %>%
    ungroup() %>%
    pivot_longer(
      cols = c("owner", "non_owner"),
      names_to = "is_owner"
    ) %>%
    select(is_owner, value) %>%
    distinct() %>%
    ggplot(aes(fill = is_owner, values = value)) +
    geom_waffle(n_rows = 5) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() +
    scale_fill_manual(
      name = NULL,
      values = c("#cc9966", "#660000"),
      labels = c("Non-owner", "Owner"),
      guide = guide_legend(reverse=TRUE)
    ) +
    xlim(-1.5, 21.5) +
    ylim(-1, 7) +
    theme(legend.position = "none") +
    ggtitle(ethnic) +
    theme(
      plot.title = element_text(hjust = 0.55, size = 15),
      text = element_text(family = "Montserrat"),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 15)
    )
  
}

# Apply function to the three groups and add percentage with segments

plot_white <- plot_owner_perc("White") +
  geom_curve(
    aes(x = -1, xend = 0.5, y = 4, yend = 3),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = -1, y = 4.4, label = "69%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "31%", colour = "#cc9966", size = 5) +
  theme(legend.position = "bottom")

plot_hispanics <- plot_owner_perc("Hispanic") +
  geom_curve(
    aes(x = 3, xend = 5, y = -1, yend = 0.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = 0.4
  ) +
  annotate("text", x = 2.2, y = -1, label = "44%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 21.5, xend = 20.5, y = 0.5, yend = 2.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 21.4, y = -0.1, label = "56%", colour = "#cc9966", size = 5)

plot_black <- plot_owner_perc("Black") +
  geom_curve(
    aes(x = 3, xend = 4, y = 6.5, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#660000",
    curvature = -0.4
  ) +
  annotate("text", x = 2.2, y = 6.5, label = "45%", colour = "#660000", size = 5) +
  geom_curve(
    aes(x = 18, xend = 17, y = 6.4, yend = 5.5),
    arrow = arrow(length = unit(2, "mm")),
    colour = "#cc9966",
    curvature = 0.4
  ) +
  annotate("text", x = 18.9, y = 6.4, label = "55%", colour = "#cc9966", size = 5)


# Show the evolution across time

plot_evol <-
  tuesdata$home_owner %>% 
  mutate(home_owner_pct = round(home_owner_pct*100, 1),
         label = if_else(year == max(year), as.character(race), NA_character_)) %>% 
  ggplot(aes(x = year, y = home_owner_pct, linetype = race)) +
  geom_line() +
  theme_pubclean() +
  ylim(0, 100) +
  labs(x = "Year", y = "Share of home ownership") +
  scale_linetype_discrete("Ethnic group") +
  theme(plot.background = element_rect(fill = "#f9f5d2", color = "#f9f5d2"),
        panel.background = element_rect(fill = "#f9f5d2"),
        legend.background = element_rect(fill = "#f9f5d2"),
        legend.position = "none",
        plot.margin = unit(c(0, 3.5, 0, 3.5),"cm")) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) 



# Arrange plots

all_plots <- ggpubr::ggarrange(plot_evol, plot_hispanics, 
                               plot_white, plot_black, 
                               ncol = 2, nrow = 2, common.legend = TRUE, 
                               legend = "bottom", 
                               legend.grob = get_legend(plot_white)) 

# Last modifications of the composition

annotate_figure(
  all_plots, 
  top = text_grob("\nHome ownership in the US\n", 
                  face = "bold",
                  size = 22,
                  family = "Montserrat"),
  bottom = text_grob(paste0("\nVisualization by Etienne Bacher. Data from the",
                            " Urban Institute and the US Census.\n"), size = 15)
) +
  bgcolor("#f9f5d2")

# Export

ggsave("R/2021/W07-wealth-income/home-ownership.pdf", 
       width = 16.3, height = 9.5, device = cairo_pdf)


pdf_convert(pdf = "R/2021/W07-wealth-income/home-ownership.pdf", 
            filenames = "R/2021/W07-wealth-income/home-ownership.png",
            format = "png", dpi = 350)

