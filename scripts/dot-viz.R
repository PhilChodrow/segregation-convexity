library(tidycensus, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tigris, quietly = TRUE)
library(sf, quietly = TRUE)
library(patchwork, quietly = TRUE)
source("src/local-info.R")
source("src/get-data.R")
source("scripts/style.R")
source("src/hclust.R")
source("src/maps.R")
source("src/utils.R")

args <- commandArgs(trailingOnly=TRUE)
cities <- args[1:length(args)]

city_df <- read_csv("assumptions/cities.csv") 



plot_list <- list()
DF <- tibble()

desired_dots <- 10000

cities <- c("Detroit", "Atlanta")


for(i in 1:length(cities)){
    city <- cities[i]
    geo <- readRDS(paste0("throughput/geo/", city, ".rds")) |>
    pivot_wider(names_from = variable, values_from = estimate)

    city <- gsub("_", " ", city)

    # dotmap under the hierarchical clustering viz
    print("- Making dots")

    # sum up all the estimates in geo
    total_people <- geo |>
        select(White, Black, Asian, Hispanic, Other) |>
        st_drop_geometry() |>
        summarise(across(everything(), sum)) |>
        rowSums()

    dots <- make_dots(geo, people_per_dot = total_people / desired_dots) # people represented per dot
    print("   ...done")

    print("- Retrieving administrative geographic overlays")
    city_info <- city_df |>
        filter(name == city) 

    state <- city_info$state[1]
    counties <- city_info$county 

    geo <- get_acs(
        geography = "county subdivision",
        variables = fields,
        state = state,
        county = counties,
        geometry = TRUE,
        year = 2024
    ) |>
      st_transform(4326)


    r <- ggplot() + 
        geom_sf(data = geo, fill = "#eee2e2", color = "#eee2e2") + 
        theme_void() + 
        geom_sf(data = dots, aes(color = variable), size = 0.1) + 
        scale_color_manual(values = palette) + 
        guides(color = guide_legend(override.aes = list(size = 5), ncol = 5, title = element_blank())) + 
        theme(legend.position = "bottom", 
            legend.text = element_text(size = 11), 
            plot.margin=unit(c(0,0,0,0),"mm"), 
            plot.title = element_text(hjust = 0.5)) + 
        font_theme + 
        ggtitle(city) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) + 
        geom_sf(data = geo, fill = NA, color = "#000000", size = 0.2)

    plot_list[[i]] <- r
}

q <- plot_list[[1]] + plot_list[[2]] +
    plot_layout(ncol = 2, guides = "collect") & theme(legend.position = 'bottom')

ggsave("fig/dot-viz.png", q, width = 4.1, height = 3, dpi = 600)
