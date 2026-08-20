library(tidyverse)
library(patchwork)
source("scripts/style.R")
source("src/maps.R")
args <- commandArgs(trailingOnly=TRUE)
cities <- args[1:length(args)]


people_per_dot <- 100
max_j <- NA


plot_list <- list()

cities <- c("Milwaukee")
cities <- gsub("_", " ", cities)

for(j in 1:length(cities)){
    
    city <- cities[j]
    geo_info <- readRDS(paste0("throughput/local-info/", city, ".rds")) 
    geo <- readRDS(paste0("throughput/geo/", city, ".rds")) |>
        pivot_wider(names_from = variable, values_from = estimate)

    local_info_viz <- geo_info |>
        mutate(local_info = 1/(8*pi) * (trace)) |>
        ggplot() + 
        geom_sf(aes(fill = local_info), size = 0.02) +
        font_theme + 
        theme_void() + 
        scale_fill_viridis_c(option = "inferno", limits = c(0, 0.02), breaks = c(0, 0.01, 0.02)) +
        guides(fill = guide_colorbar(title = "Local\ninformation\n(nats/km²)"), title.position = "top") +
        font_theme +
        theme(legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "mm")) 
        # + 
        # guides(fill = guide_colorbar(breaks = seq(0, max_j, length.out = 3), title.position = "top", title.hjust = 0.5)) 

    dots <- make_dots(geo, people_per_dot)
    dot_viz <- ggplot() + 
        geom_sf(data = geo, size = 0.1, fill = map_fill, color = map_color) + 
        theme_void() + 
        geom_sf(data = dots, aes(color = variable), size = 0.2) + 
        scale_color_manual(values = palette) + 
        guides(color = guide_legend(override.aes = list(size = 4), nrow = 2)) + 
        theme(legend.title = element_blank(), legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "mm")) + 
        font_theme

    if(j > 1){
        local_info_viz <- local_info_viz + 
            guides(fill = "none")
        dot_viz <- dot_viz + 
            guides(color = "none")
    }

    plot_list[[2*j-1]] <- dot_viz
    plot_list[[2*j]] <- local_info_viz
}

p <- patchwork::wrap_plots(plot_list, ncol = 2) & theme(plot.margin=unit(c(0,0,0,0),"mm"))

ggsave("fig/local-info.png", p, width = 4.5, height = 5, bg = "white")





