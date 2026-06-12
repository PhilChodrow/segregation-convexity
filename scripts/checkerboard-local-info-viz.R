library(tidyverse)
library(sf)
library(patchwork)
source("src/local-info.R")
source("scripts/style.R")


geo          <- st_read("throughput/checkerboard/shapefile/shapefile.shp")
demographics <- read_csv("throughput/checkerboard/demographics.csv")

geo <- geo |>
    mutate(GEOID = as.character(GEOID))

demographics <- demographics |>
    mutate(GEOID = as.character(GEOID))

# test scaling

demographics <- demographics |>
    mutate(n_1 = n_1 * 100 + 1, 
           n_2 = n_2 * 100 + 1)




all_geos <- list()

smoothing_bandwidth <- 1


# let's redo the RBF smoother so that it gives us the same kind of thing: a data frame with a GEOID and columns for each group. 

for(current_type in c("seg", "checker")) {

    new_demographics <- demographics |>
        filter(type == current_type) |>
        select(-type)

    smoothed_demographics <- spatial_rbf_smoother(new_demographics, geo, sigma = smoothing_bandwidth) 

    geo_with_demo <- geo |>
        left_join(smoothed_demographics, by = c("GEOID" = "GEOID")) |>
        mutate(type = current_type) 

    all_geos[[current_type]] <- geo_with_demo
}

geo_with_demos <- bind_rows(all_geos)
geo_with_demos <- geo_with_demos |>
    mutate(label = ifelse(type == "seg", "(b)", "(a)"))

r <- ggplot(geo_with_demos) +
    geom_sf(aes(fill = n_1 / (n_1 + n_2)), size = 0.02) + 
    checkerboard_config +
    checkerboard_theme + 
    facet_wrap(~label) +
    font_theme +
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5)) + 
    labs(fill = 'Density of Group A')

ggsave("fig/checkerboard-smoothed.png", r, width = 5, height = 5, bg = "white")

for(current_type in c("seg", "checker")) {

    new_demographics <- demographics |>
        filter(type == current_type) |>
        select(-type)

    smoothed_demographics <- spatial_rbf_smoother(new_demographics, geo, sigma = smoothing_bandwidth) 

    g <- compute_metric_tensor(geo, smoothed_demographics, sigma = 1)

    geo_with_info <- geo |>
        left_join(g, by = c("GEOID" = "GEOID")) |>
        mutate(type = current_type) 

    all_geos[[current_type]] <- geo_with_info
}

geo_with_info <- bind_rows(all_geos)

geo_with_info <- geo_with_info |>
    mutate(label = ifelse(type == "seg", "(d)", "(c)"))

p <- geo_with_info |>
    ggplot() + 
    geom_sf(aes(fill = trace), size = 0.02) + 
    checkerboard_config +
    facet_wrap(~label) +
    scale_fill_viridis_c(option = "inferno") + 
    checkerboard_theme + 
    font_theme + 
    guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, title = "Mean Local Information")) 
    
ggsave("fig/checkerboard-metric-tensor-trace.png", p, width = 4.5, height = 5, bg = "white")

q <- r / p + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("fig/checkerboard-smoothed-and-trace.png", q, width = 5, height = 6.5, bg = "white")

mean_traces <- geo_with_info |>
    group_by(type) |>
    summarize(mean_trace = mean(trace)) |>
    pull(mean_trace)


tex_macros <- paste0("\\newcommand{\\smoothingBandwidth}{", smoothing_bandwidth, "}\n")

tex_macros <- paste0(tex_macros, "\\newcommand{\\meanTraceSeg}{", round(mean_traces[2], 3), "}\n")

tex_macros <- paste0(tex_macros, "\\newcommand{\\meanTraceChecker}{", round(mean_traces[1], 3), "}\n")

cat(tex_macros, file = "params/local-info-illustration.tex")