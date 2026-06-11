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
    checkerboard_config +
    checkerboard_theme +
    font_theme +
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5)) + 
    labs(fill = 'Density of Group A') + 
    font_theme

ggsave("fig/checkerboard.png", r, width = 5, height = 6.5, dpi = 300,   bg = "#FFFFFF")