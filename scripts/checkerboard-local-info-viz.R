library(tidyverse)
library(sf)
source("src/local-info.R")
source("scripts/style.R")


geo          <- st_read("throughput/checkerboard/shapefile/shapefile.shp")
demographics <- read_csv("throughput/checkerboard/demographics.csv")

all_geos <- list()

smoothing_bandwidth <- 2

tex_macros <- paste0("\\newcommand{\\smoothingBandwidth}{", smoothing_bandwidth, "}\n")

cat(tex_macros, file = "params/local-info-illustration.tex")


for(current_type in c("seg", "checker")) {

    new_demographics <- demographics |>
        filter(type == current_type) |>
        pivot_longer(cols = -c(GEOID, type), names_to = "group", values_to = "count") |>
        arrange(group, GEOID) |>
        select(-type) |>
        group_by(GEOID) |>
        nest() |>
        mutate(n = map(data, ~setNames(as.numeric(.x$count), .x$group))) |>
        select(-data) 

    smoothed_demographics <- spatial_rbf_smoother(new_demographics, geo, sigma = smoothing_bandwidth) 

    geo_with_demo <- geo |>
        left_join(smoothed_demographics, by = c("GEOID" = "GEOID")) |>
        mutate(p_scalar = map_dbl(p, ~.x[1]), 
               type = current_type) 

    all_geos[[current_type]] <- geo_with_demo
}


geo_with_demos <- bind_rows(all_geos)


r <- ggplot(geo_with_demos) +
    geom_sf(aes(fill = p_scalar), size = 0.02) + 
    checkerboard_config +
    checkerboard_theme + 
    facet_wrap(~type) +
    font_theme +
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5)) + 
    labs(fill = 'Density of Group A')

ggsave("fig/checkerboard-smoothed.png", r, width = 5, height = 5, bg = "white")

