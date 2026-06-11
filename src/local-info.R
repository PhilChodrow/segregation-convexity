library(tidyverse)
library(sf)

compute_centroid_df <- function(tracts, km = FALSE, ...){

	centroids <- st_centroid(tracts) %>%
		mutate(x = map_dbl(geometry, ~.[1]),
			   y = map_dbl(geometry, ~.[2])) %>%
		tbl_df() %>%
		select(GEOID, x, y) %>%
		rename(geoid = GEOID)

	if(km){
		centroids <- centroids %>%
			mutate(x = x * cos(y / 360) * 111,
				   y = y * 111)
	}
	return(centroids)
}

id_lookup <- function(tracts, key_col = 'GEOID'){
	tracts[[key_col]] %>%
		data_frame(row = as.character(1:length(.)), geoid = .)
}

make_adjacency <- function(tracts){
	lookup  <- id_lookup(tracts)
	adj_mat <- st_relate(tracts, pattern = '****T****', sparse = TRUE) # as sparse list
	1:length(adj_mat) %>%
		map(~data_frame(from = as.character(.),
						to = as.character(adj_mat[[.]]))) %>%
		reduce(rbind) %>%
		left_join(lookup, by = c('from' = 'row')) %>%
		left_join(lookup, by = c('to' = 'row'), suffix = c('_1', '_2')) %>%
		select(-from, -to)
}

add_coords_to_adj <- function(adj, tracts, km = FALSE){

	coords <- compute_centroid_df(tracts, km)

	adj <- adj %>%
		left_join(coords, by = c('geoid_1' = 'geoid')) %>%
		left_join(coords, by = c('geoid_2' = 'geoid'), suffix = c('_1', '_2'))

	if('t_1' %in% names(adj)){
		adj <- adj %>%
			mutate(coords_1 = pmap(list(x_1, y_1, t_1), c),
				   coords_2 = pmap(list(x_2, y_2, t_2), c))
	}else{
		adj <- adj %>%
			mutate(coords_1 = pmap(list(x_1, y_1), c),
				   coords_2 = pmap(list(x_2, y_2), c))
	}
	return(adj)
}

# alternative take on the RBF smoother that doesn't use adjacency structure or data frame nonsense, just vectorized computations like a not-sociopath

spatial_rbf_smoother <- function(demographics, geo, sigma = 10) {
	
	dist_matrix <- geo |>
		arrange(GEOID) |>
		st_distance() |> 
		as.matrix() |>
		clean_units()
	weight_matrix <- exp(-dist_matrix*dist_matrix / (2 * sigma^2))
	weight_matrix <- weight_matrix / rowSums(weight_matrix)

	p_matrix <- demographics |>
		arrange(GEOID) |>
		select(n) |>
		pull() |>
		map(~.x / sum(.x)) |>
		reduce(cbind) |>
		t()

	smoothed_p_matrix <- weight_matrix %*% p_matrix

	rownames(smoothed_p_matrix) <- demographics$GEOID
	colnames(smoothed_p_matrix) <- paste0("group_", 1:ncol(smoothed_p_matrix))
	

	new_demos <- demographics |>
		cbind(smoothed_p_matrix)

	new_demos <- new_demos |>
		pivot_longer(cols = starts_with("group_"), names_to = "group", values_to = "p_smoothed") |>
		select(-n) |>
		group_by(GEOID) |>
		nest() |>
		mutate(p = map(data, ~setNames(as.numeric(.x$p_smoothed), .x$group))) |>
		select(-data)
		

	return(new_demos)
}


# https://stackoverflow.com/questions/46935207/removing-units-from-an-r-vector
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}