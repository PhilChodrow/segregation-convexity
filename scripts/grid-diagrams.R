library(tidyverse)
library(sf)
source("scripts/style.R")

num_blocks <- 3
cells_per_block <- 3
total_cells_per_side <- num_blocks * cells_per_block


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

blocks <- grid_sf |>
    mutate(block = as.integer(x_idx/ 3)  + as.integer(y_idx/3)) |>
    group_by(block) |>
    summarise(geometry = st_union(geometry))
    



grid_sf <- sf::st_set_crs(grid_sf, 32618) # set the coordinate reference system to WGS 84 (EPSG:4326)
blocks <- sf::st_set_crs(blocks, 32618) # set the coordinate reference system to WGS 84 (EPSG:4326)


demographics <- grid_sf |>
    mutate(`(a)` = 1, 
           `(b)` = (x_idx + y_idx) %% 3,
           `(d)` = as.integer((x_idx) / 3),
           `(c)` = (as.integer(x_idx/ 3)  + as.integer(y_idx/3))  %% 3) |>
           pivot_longer(cols = c(`(a)`, `(b)`, `(c)`, `(d)`), names_to = "type") |>
           mutate(value = factor(value))
           
# create multiline labels for the facets to be used with label_parsed
demographics <- demographics |>
           mutate(label = case_when(
               type == "(a)" ~ "atop(City~(a), phi(italic(P))==0~~~~italic(I)[phi](italic(P)) == 0)",
               type == "(b)" ~ "atop(City~(b), phi(italic(P))==log(3)~~~~italic(I)[phi](italic(P)) == 0)",
               type == "(c)" ~ "atop(City~(c), {phi(italic(P))==italic(I)[phi](italic(P))}==log(3))",
               type == "(d)" ~ "atop(City~(d), {phi(italic(P))==italic(I)[phi](italic(P))} == log(3))"
           ))
           

# compute the centroids
demographics  <- demographics |>
    st_centroid()

r <- ggplot() + 
    geom_sf(data = demographics, aes(shape = value, color = value), size = 2.2) + 
    theme_void() + 
    theme(legend.position = "none") + 
    coord_sf(expand = FALSE) + 
    facet_wrap(~label, nrow = 1, labeller = label_parsed) + 
    scale_color_manual(values = c(palette[[1]], palette[[2]], palette[[4]])) +
    scale_fill_manual(values = c(palette[[1]], palette[[2]], palette[[4]])) +
    geom_sf(data = blocks, fill = NA, color = darkgrey, size = 0.5) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) + 
    font_theme

ggsave("fig/dot-diagram.png",r, width = 7, height = 2.0, dpi = 600, bg = "white")