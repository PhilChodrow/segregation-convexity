library(sf)
library(tidyverse)
library(patchwork)

source("scripts/style.R")

geo <- st_read("throughput/checkerboard/shapefile/shapefile.shp")
demographics <- read_csv("throughput/checkerboard/demographics.csv") 
    
geo <- geo %>%
    left_join(demographics, by = "GEOID")

geo <- geo |>
    mutate(type = case_when(
        type == "all_white" ~ "(a)",
        type == "all_grey" ~ "(b)",
        type == "checker" ~ "(c)",
        type == "seg" ~ "(d)",
        TRUE ~ type
    )) %>%
    mutate(type = factor(type, levels = c("(a)", "(b)", "(c)", "(d)")))



r <- ggplot(geo) + 
    geom_sf(aes(fill = n_1), color = 'black', size = 0.02) + 
    facet_wrap(~type, nrow = 2) + 
    scale_fill_continuous(
        low = 'white', 
        high = darkgrey, 
        limits=c(0,1), 
        breaks = c(0, .5, 1),
        labels = scales::percent) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    # theme_void() + 
    coord_sf() +
    theme(legend.position = 'bottom', 
          panel.spacing = unit(1.2, 'lines'), 
        #   plot.margin=grid::unit(c(0,0,0,0), "mm"), 
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
          strip.text =  element_text(vjust = 1.5, size = 15), 
          strip.background = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5)) + 
    labs(fill = 'Density of Group A') + 
    font_theme

ggsave("fig/checkerboard.png", r, width = 5, height = 6.5, dpi = 300,   bg = "#FFFFFF")