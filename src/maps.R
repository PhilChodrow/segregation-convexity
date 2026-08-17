library(tidyverse)
library(sf)
source("scripts/style.R")
make_dots <- function(geo, people_per_dot){

    vars <- colnames(geo)[!(colnames(geo) %in% c("GEOID", "geometry"))]
    points <- data.frame(geometry = st_sfc())
    for(i in 1:nrow(geo)){
        for (j in 1:length(vars)){
            var <- vars[j]
            n_people <- geo[[var]][i]
            to_place <- n_people / people_per_dot
            if(to_place < 1){
                to_place <- ifelse(runif(1) < to_place, 1, 0)
            }
            if(to_place > 0){
                new_points <- st_as_sf(st_sample(geo[i,], size = to_place)) |> 
                    mutate(variable = var)
                points <- rbind(points, new_points)
            }
        }
    }
    points
}


hclust_map <- function(geo, h, k, gg, size_factor = 1){
    
    all <- st_union(geo)

    all_bd <- st_boundary(all)
    
    for(j in 2:k){
        print(j)
        plot_geo <- geo |>
            mutate(new_cluster = cutree(h, k = j)) |>
            group_by(new_cluster) |>
            summarise(geometry = st_union(geometry))
        
        bd <- st_boundary(plot_geo) |>
            st_difference(all_bd)

        size <- rev(h$height)[j-1]

        gg <- gg + geom_sf(data = bd, alpha = 1, color = darkgrey, size = 10^(2*size)*size_factor)
    }
    gg
}
