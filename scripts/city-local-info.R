library(tidyverse)
library(sf)

source("src/local-info.R")
source("scripts/style.R")


args <- commandArgs(trailingOnly=TRUE)
city <- args[1]

geo <- readRDS(paste0("throughput/geo/", city, ".rds"))

# meters
smoother_bandwidth <- 1000

# filter out empty geometries

geo <- geo |>
    filter(!st_is_empty(geometry))

geo <- geo |>
    group_by(GEOID) |>
    filter(sum(estimate) > 0)
 
# data prep: widen and smooth
geo <- geo |>
    pivot_wider(names_from = variable, values_from = estimate)

demos <- geo |>
    st_drop_geometry()

smoothed_demos <- spatial_rbf_smoother(demos, geo, sigma = smoother_bandwidth)

geo <- geo |>
    select(-c(White, Black, Asian, Hispanic, Other)) |>
    left_join(smoothed_demos, by = c("GEOID" = "GEOID")) 

g <- compute_metric_tensor(geo, smoothed_demos, sigma = 1)

geo_with_info <- geo |>
    left_join(g, by = c("GEOID" = "GEOID"))



tex_macros <- paste0(
    "\\newcommand{\\citySmootherBandwidth}{", round(smoother_bandwidth/1000, 0), "}\n" # km
)
cat(tex_macros, file = "params/local-info-viz.tex")

saveRDS(geo_with_info, paste0("throughput/local-info/", city, ".rds"))
