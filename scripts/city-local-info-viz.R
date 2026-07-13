library(tidyverse)
library(patchwork)

args <- commandArgs(trailingOnly=TRUE)
city <- args[1]
geo_info <- readRDS(paste0("throughput/local-info/", city, ".rds")) 

# just for the demographics
geo <- readRDS(paste0("throughput/geo/", city, ".rds"))

geo <- geo |>
    filter(variable %in% c("White", "Black", "Hispanic")) |>
    group_by(GEOID) |>
    mutate(p = estimate / sum(estimate)) |>
    ungroup() |>
    select(-estimate)


demographic_viz <- function(geo, variable_name, color) {
    geo |>
        filter(variable == variable_name) |>
        ggplot() + 
        geom_sf(aes(fill = p), size = 0.02) +
        font_theme + 
        theme_void() + 
        scale_fill_gradient(low = "white", high = color, limits = c(0, 1), na.value = "white") +
        labs(title = paste("Proportion", variable_name)) + 
        guides(fill = "none")
}

local_info_viz <- geo_info |>
    ggplot() + 
    geom_sf(aes(fill = trace), size = 0.02) +
    font_theme + 
    theme_void() + 
    scale_fill_viridis_c(option = "inferno") +
    labs(title = "Local Information") + 
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, title = "J(x)"))



r <- demographic_viz(geo, "White", "#ED6A5A") + 
     demographic_viz(geo, "Black", "#d8d154") + 
     demographic_viz(geo, "Hispanic", "#5b8781") + 
     local_info_viz +
    plot_layout(guides = "collect", nrow = 1) 


ggsave(paste0("fig/", city, "-local-info-overview.png"), r, width = 12, height = 3, bg = "white")