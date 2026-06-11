# this script creates the four synthetic checkerboard files and saves them as sf objects to throughput

# make the throughput/checkerboards directory if it doesn't exist already

# setup
library(tidyverse)
library(sf)

checkerboard_dir <- "throughput/checkerboard"
if (!dir.exists(checkerboard_dir)) {
    dir.create(checkerboard_dir)
}

# params
num_blocks <- 8
cells_per_block <- 8
total_cells_per_side <- num_blocks * cells_per_block

# write each of these to the file params.tex

tex_macros <- paste0("\\newcommand{\\numBlocks}{", num_blocks, "}\n",
                    "\\newcommand{\\cellsPerBlock}{", cells_per_block, "}\n",
                    "\\newcommand{\\totalCellsPerSide}{", total_cells_per_side, "}\n")


cat(tex_macros, file = "params/checkerboard_params.tex")


# shapefile

poly_sfc <- st_sfc(st_polygon(list(rbind(c(0,0), c(total_cells_per_side,0), c(total_cells_per_side,total_cells_per_side), c(0,total_cells_per_side), c(0,0)))))
sf_poly <- st_sf(geometry = poly_sfc)
grid <- st_make_grid(sf_poly, cellsize = 1, square = TRUE)

grid_sf <- st_sf(geometry = grid) 
centroids <- st_centroid(grid_sf)
coords <- st_coordinates(centroids)
grid_sf <- grid_sf |>
    mutate(
        GEOID = 1:nrow(grid_sf),
        x_idx = as.integer(coords[,"X"]),
        y_idx = as.integer(coords[,"Y"])
    )


grid_sf <- sf::st_set_crs(grid_sf, 32618) # set the coordinate reference system to WGS 84 (EPSG:4326)

write_sf(
    grid_sf, paste0(checkerboard_dir, "/shapefile", sep = "/"), 
    , driver = "ESRI Shapefile"
)

# demographics
grid_sf |>
    as_tibble() |>
    select(GEOID, x_idx, y_idx) |>
    mutate(
        all_white = 0, 
        all_grey = 0.5,
        checker = as.integer((x_idx %/% cells_per_block + y_idx %/% cells_per_block) %% 2 == 0), 
        seg = as.integer(x_idx <= cells_per_block * (num_blocks / 2))) |>
    pivot_longer(cols = c(all_white, all_grey, checker, seg), names_to = "type", values_to = "n_1") |>
    mutate(n_2 = 1 - n_1) |> 
    select(-x_idx, -y_idx) |>
    write_csv(paste0(checkerboard_dir, "/demographics.csv"))
