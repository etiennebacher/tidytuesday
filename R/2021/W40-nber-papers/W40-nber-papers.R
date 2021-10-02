library(dplyr) 
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(patchwork)
library(extrafont)
library(pdftools)
library(gameofthrones)


# Takes a few minutes
# extrafont::font_import()


###########################
## Treat the data ##
###########################


papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')


joined_df <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 


#######################################
##### Make plots #####
#######################################

custom_theme <- theme(
  plot.background = element_rect(fill = got_palettes$daenerys[5],
                                 color = got_palettes$daenerys[5]),
  panel.background = element_rect(fill = got_palettes$daenerys[5]),
  legend.background = element_rect(fill = got_palettes$daenerys[5]),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_text(size = 9),
  plot.title = element_text(hjust = 0.5, size = 14),
  legend.position = "bottom",
  legend.key = element_rect(fill = got_palettes$daenerys[5], 
                            color = got_palettes$daenerys[5]),
  text = element_text(color = "#404040", family = "Sawasdee")
)

share_plot <-
  joined_df |> 
  filter(year >= 1980, year != 2021) |> 
  select(year, paper, program_category) |> 
  distinct() |> 
  group_by(year, program_category) |> 
  count() |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(share = n / sum(n) * 100) |> 
  ungroup() |> 
  ggplot(aes(year, share, fill = program_category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = got_palettes$daenerys[2]) +
  geom_hline(yintercept = c(25, 75), linetype = "dashed", alpha = 0.5,
             color = got_palettes$daenerys[2]) +
  labs(
    title = "Share of new papers by category",
    x = "",
    y = ""
  ) +
  scale_fill_manual(values = got_palettes$daenerys[c(1, 4, 3, 2)]) +
  guides(fill = guide_legend(
    title = "Category",
    title.position = "top",
    title.theme = element_text(size = 13, color = "#404040", family = "Sawasdee"),
    title.hjust = 0.5,
    label.theme = element_text(size = 10, color = "#404040", family = "Sawasdee")
  )) +
  custom_theme


evol_plot <-
  joined_df |> 
  filter(year >= 1980, year != 2021) |> 
  select(year, paper, program_category) |> 
  distinct() |> 
  group_by(year, program_category) |> 
  count() |> 
  ungroup() |> 
  drop_na() |> 
  ggplot(aes(year, n, color = program_category)) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = 400, alpha = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 800, alpha = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 1200, alpha = 0.3, linetype = "dashed") +
  labs(
    title = "Number of new papers by category",
    color = "Category",
    y = ""
  ) +
    scale_color_manual(values = got_palettes$daenerys[c(1, 4, 3)]) +
    guides(color = guide_legend(
      title = "Category",
      title.position = "top",
      title.theme = element_text(size = 13, color = "#404040", family = "Sawasdee"),
      title.hjust = 0.5,
      label.theme = element_text(size = 10, color = "#404040", family = "Sawasdee")
    )) +
    custom_theme
  

share_plot + evol_plot +
  plot_annotation(
    title = 'Macro or Micro: which one is the NBER favorite?<br>',
    caption = '<br>Made by Etienne Bacher | Data from National Bureau of Economic Research (NBER)',
    theme = theme(
      plot.title = element_markdown(hjust = 0.5, size = 22),
      plot.caption = element_markdown(hjust = 0.5),
      plot.background = element_rect(fill = got_palettes$daenerys[5]),
      panel.background = element_rect(fill = got_palettes$daenerys[5]),
      text = element_text(color = "#404040", family = "Sawasdee")
    )
  ) 




###########################
## Export ##
###########################

ggsave("R/2021/W40-nber-papers/nber-papers.pdf", 
       width = 15, height = 9, device = cairo_pdf)

pdf_convert(pdf = "R/2021/W40-nber-papers/nber-papers.pdf", 
            filenames = "R/2021/W40-nber-papers/nber-papers.png",
            format = "png", dpi = 350)   

