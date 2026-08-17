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

for(j in 1:length(cities)){
    
    city <- cities[j]
    geo_info <- readRDS(paste0("throughput/local-info/", city, ".rds")) 
    geo <- readRDS(paste0("throughput/geo/", city, ".rds")) |>
        pivot_wider(names_from = variable, values_from = estimate)

    local_info_viz <- geo_info |>
        ggplot() + 
        geom_sf(aes(fill = trace), size = 0.02) +
        font_theme + 
        theme_void() + 
        scale_fill_viridis_c(option = "inferno", limits = c(0, max_j)) +
        guides(fill = guide_colorbar(title.hjust = 0.5, title = "J(x)")) +
        font_theme +
        theme(legend.position = "bottom")

    
    
    dots <- make_dots(geo, people_per_dot)
    dot_viz <- ggplot() + 
        geom_sf(data = geo, fill = "#eee2e2", color = "#eee2e2") + 
        theme_void() + 
        geom_sf(data = dots, aes(color = variable), size = 0.5) + 
        scale_color_manual(values = palette) + 
        guides(color = guide_legend(override.aes = list(size = 4), nrow = 2)) + 
        theme(legend.title = element_blank(), legend.position = "bottom") + 
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







# 

# # just for the demographics
# 

# geo <- geo |>
#     filter(variable %in% c("White", "Black", "Hispanic")) |>
#     group_by(GEOID) |>
#     mutate(p = estimate / sum(estimate)) |>
#     ungroup() |>
#     select(-estimate)


# demographic_viz <- function(geo, variable_name, color) {
#     geo |>
#         filter(variable == variable_name) |>
#         ggplot() + 
#         geom_sf(aes(fill = p), size = 0.02) +
#         font_theme + 
#         theme_void() + 
#         scale_fill_gradient(low = "white", high = color, limits = c(0, 1), na.value = "white") +
#         labs(title = paste("Proportion", variable_name)) + 
#         guides(fill = "none")
# }





# r <- demographic_viz(geo, "White", "#ED6A5A") + 
#      demographic_viz(geo, "Black", "#d8d154") + 
#      demographic_viz(geo, "Hispanic", "#5b8781") + 
#      local_info_viz +
#     plot_layout(guides = "collect", nrow = 1) 


# ggsave(paste0("fig/", city, "-local-info-overview.png"), r, width = 12, height = 3, bg = "white")