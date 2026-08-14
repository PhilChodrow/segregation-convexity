library(sf)
library(tidyverse)


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

rect <- geo |>
    filter(type == "(c)") |>
    mutate(in_box = ifelse(x_idx >= 16 & y_idx < 16, TRUE, FALSE)) |>
    filter(in_box) |>
    st_union() |>
    st_as_sf() |>
    mutate(type = "(c)")





r <- ggplot(geo) + 
    geom_sf(aes(fill = n_1), color = 'black', size = 0.02) + 
    facet_wrap(~type, nrow = 1) + 
    checkerboard_config +
    checkerboard_theme +
    font_theme +
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5)) + 
    labs(fill = 'Density of Group A') + 
    font_theme + 
    geom_sf(data = rect, fill = NA, color = darkgrey, size = 1) 

ggsave("fig/checkerboard.png", r, width = 9, height = 3.5, dpi = 300,   bg = "#FFFFFF")