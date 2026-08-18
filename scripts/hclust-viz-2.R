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
city <- args[1]

cities <- c("Atlanta", "Detroit", "Washington DC")

plot_list <- list()
DF <- tibble()

desired_dots <- 10000


for(i in 1:length(cities)){
    city <- cities[i]
    geo <- readRDS(paste0("throughput/geo/", city, ".rds")) |>
    pivot_wider(names_from = variable, values_from = estimate)

    print(city)
    print("---------")
    print("- Clustering")
    h <- hclust_sf(geo, verbose = F)
    print("  ...done")

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

    r <- ggplot() + 
        geom_sf(data = geo, fill = "#eee2e2", color = "#eee2e2") + 
        theme_void() + 
        geom_sf(data = dots, aes(color = variable), size = 0.1) + 
        scale_color_manual(values = palette) + 
        guides(color = guide_legend(override.aes = list(size = 5), ncol = 5, title = element_blank())) + 
        theme(legend.position = "bottom", 
            legend.text = element_text(size = 11), 
            plot.margin=unit(c(0,0,0,0),"mm")) + 
        font_theme + 
        ggtitle(paste0(city, "\n(", nrow(geo), " blockgroups)")) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) 

    r <- hclust_map(geo, h, k = 3, r, size_factor = 0.5) 
    plot_list[[i]] <- r

    height_df <- tibble(height = rev(h$height)) |>
    mutate(k = row_number() + 1, 
           cumulative_info = cumsum(height)) |> 
    rbind(tibble(height = 0, k = 1, cumulative_info = 0)) |>
    mutate(city = city)

    DF <- rbind(DF, height_df)
}


curves <- DF |>
    filter(k <= 20) |>    
    ggplot() + 
    aes(x = k, y = cumulative_info, shape = city) + 
    geom_line(color = darkgrey) + 
    geom_point(color = darkgrey, size = 1.5, fill = "white") + 
    theme_minimal() + 
    font_theme + 
    theme(axis.line = element_line(color = darkgrey), legend.position = c(0.8, 0.5)) + 
    xlab("Number of splits") + 
    ylab("Shannon mutual information (nats)") + 
    guides(linetype = guide_legend(title = element_blank()), shape = guide_legend(title = element_blank())) + 
    scale_shape_manual(values = c(21, 22, 23)) 


layout_boxes <- 
    c(
        area(t = 1, l = 1, b = 1, r = 1),
        area(t = 1, l = 2, b = 1, r = 2),
        area(t = 1, l = 3, b = 1, r = 3),
        area(t = 2, l = 1, b = 2, r = 3), 
        area(t = 1, l = 4, b = 1, r = 4)
    )

big_plot <- ((((plot_list[[1]] + plot_list[[2]] + plot_list[[3]] ) + plot_layout(guides = "collect")) + guide_area() & theme(legend.position = c(0.5, 10))) + (curves + plot_layout(guides = "keep"))  + plot_layout(design = layout_boxes, widths = c(1, 1, 1, 1.5), heights = c(1, 0.01))) & theme(plot.margin=unit(c(0,1.5,0,1.5),"mm")) 

ggsave("fig/aggregation-viz.png", big_plot, width = 10, height = 3.5)
