library(tidyverse)
library(sf)
library(patchwork)
library(ggmagnify)
library(units)
source("src/local-info.R")
source("scripts/style.R")

args <- commandArgs(trailingOnly=TRUE)
city <- args[1]

city <- "Detroit"

geo <- readRDS(paste0("throughput/geo/", city, ".rds"))

# create overlaid a hexagonal grid

hex_grid <- st_make_grid(geo, cellsize = 0.1, square = FALSE) |>
    st_as_sf() |>
    mutate(hex_id = row_number()) |>
    st_intersection(st_union(geo))

# compute the area of the intersection between each hex and each CBG, and then compute the weighted average of the demographics in each hex

test <- geo |>
    st_intersection(hex_grid)

test <- st_make_valid(test)
test$area <- st_area(test$geometry)


new_grid <- test |>
    group_by(hex_id, variable) |>
    summarize(weighted_estimate = sum(estimate * area) / sum(area), .groups = "drop") 


new_grid |>
    ggplot() +
    geom_sf(aes(fill = as.numeric(weighted_estimate)), color = "black", size = 0.1)