# many functions in this file lightly modified from compx 
# https://github.com/PhilChodrow/compx

library(tidyverse)
library(sf)
source("src/constructors.R")

compute_centroid_df <- function(tracts, km = FALSE, ...){

	centroids <- st_centroid(tracts) |>
		mutate(x = map_dbl(geometry, ~.[1]),
			   y = map_dbl(geometry, ~.[2])) |>
		tibble() |>
		select(GEOID, x, y) 

	if(km){
		centroids <- centroids |>
			mutate(x = x * cos(y / 360) * 111,
				   y = y * 111)
	}
	return(centroids)
}

id_lookup <- function(tracts, key_col = 'GEOID'){
	tracts[[key_col]] %>%
		data_frame(row = as.character(1:length(.)), GEOID = .)
}

make_adjacency <- function(tracts){
	lookup  <- id_lookup(tracts)
	adj_mat <- st_relate(tracts, pattern = '****T****', sparse = TRUE) # as sparse list
	1:length(adj_mat) |>
		map(~data_frame(from = as.character(.),
						to = as.character(adj_mat[[.]]))) |>
		reduce(rbind) |>
		left_join(lookup, by = c('from' = 'row')) |>
		left_join(lookup, by = c('to' = 'row'), suffix = c('_1', '_2')) |>
		select(-from, -to)
}

add_coords_to_adj <- function(adj, tracts, km = FALSE){

	coords <- compute_centroid_df(tracts, km)
	new_adj <- adj |>
		left_join(coords, by = c('GEOID_1' = 'GEOID')) |>
		left_join(coords, by = c('GEOID_2' = 'GEOID'), suffix = c('_1', '_2'))

	new_adj <- new_adj |>
		mutate(coords_1 = pmap(list(x_1, y_1), c),
			   coords_2 = pmap(list(x_2, y_2), c)) |>
		select(-x_1, -y_1, -x_2, -y_2)
	return(new_adj)
}

# alternative take on the RBF smoother that doesn't use adjacency structure or data frame nonsense, just vectorized computations like a not-sociopath

spatial_rbf_smoother <- function(demographics, geo, sigma = 10) {
	
	demographic_column_names <- demographics |>
		select(-GEOID) |>
		colnames()

	GEOID_order <- geo |>
		arrange(GEOID) |>
		pull(GEOID)

	dist_matrix <- geo |>
		arrange(GEOID) |>
		st_centroid() |>
		st_distance() |> # by default in units of meters
		as.matrix() |>
		clean_units() 

	weight_matrix <- exp(-dist_matrix/sigma * dist_matrix/sigma / 2)
	weight_matrix <- weight_matrix / rowSums(weight_matrix)

	totals <- demographics |>
		arrange(GEOID) |>
		select(-GEOID) |>
		rowSums()

	p_matrix <- demographics |> 
		arrange(GEOID) |>
		select(-GEOID) |>
		as.matrix() 
	
	p_matrix <- p_matrix / rowSums(p_matrix)

	smoothed_p_matrix <- weight_matrix %*% p_matrix

	rownames(smoothed_p_matrix) <- GEOID_order
	colnames(smoothed_p_matrix) <- demographic_column_names
	
	new_demos <- (smoothed_p_matrix * totals) |>
		as.data.frame() |>
		rownames_to_column("GEOID") |>
		as_tibble() 

	return(new_demos)
}


# https://stackoverflow.com/questions/46935207/removing-units-from-an-r-vector
clean_units <- function(x){
  attr(x,"units") <- NULL
  class(x) <- setdiff(class(x),"units")
  x
}

# METRIC COMPUTATIONS

normalize <- function(n){
	n / sum(n)
}

# X, the design matrix, should be a matrix of local spatial neighborhoods
# Y the dependent demographic distributions, should be a matrix of demographic distributions in the local neighborhoods
# check on whether we should diff these from the focal distribution or not?
# do we need to center either X or Y in order for the analytic formula to be correct?

do_regression <- function(X, Y, W = diag(dim(X)[1])){
		tryCatch({
			(solve((t(X) %*% W) %*% X) %*% t(X)) %*% (W %*% Y)
			},
			error = function(e) matrix(NA, dim(X)[1], dim(Y)[2]) )
}

# basic data structure: geo with GEOID and geometry, demographics with GEOID and demographic counts. 

DKL_ <- function(p, eps = 0.0001){
	p <- (p + eps) / sum(p + eps)
	diag(1 / p)
}


compute_metric_tensor <- function(geo, demographics, sigma, hessian = DKL_) { 

	# normalized demographic distributions within each geoid
	proportions <- demographics |>
		pivot_longer(cols = -GEOID, names_to = "group", values_to = "count") |>
		group_by(GEOID) |>
		mutate(p = count / sum(count)) |>
		select(-count) |>
		group_by(GEOID) |>
		nest() |>
		mutate(p = map(data, ~setNames(as.numeric(.x$p), .x$group))) |>
		select(-data)

	# now we need to figure out, for each geoid, which other geoids we want to include in the local neighborhood

	adj <- make_adjacency(geo)
	adj <- adj |>
		left_join(proportions, by = c("GEOID_1" = "GEOID")) |>
		left_join(proportions, by = c("GEOID_2" = "GEOID"), suffix = c("_1", "_2"))

	derivs <- add_coords_to_adj(adj, geo, km = TRUE) |>
		mutate(
			x_diff = map2(coords_2, coords_1, ~.x - .y), 
			p_diff = map2(p_2, p_1, ~.x - .y)
		) |>
		select(GEOID_1, GEOID_2, x_diff, p_diff) |>
		mutate(
			distance = map_dbl(x_diff, ~sqrt(sum(.x^2))), 
			weight   = exp(-distance^2 / (2 * sigma^2))
		) |>
		filter(GEOID_1 != GEOID_2) |>
		group_by(GEOID_2) |>
		mutate(weight = weight / sum(weight)) |>
		group_by(GEOID_1) |>
		# filter(n() > 2) |>
		do(X = reduce(.$x_diff, rbind), 
		   P = reduce(.$p_diff, rbind), 
		   w = reduce(.$weight, c)) |>
		ungroup() |>
		mutate(size = map_dbl(w, length)) |>
		mutate(W = map(w, ~diag(.x))) |>
		# corrections for neighborhoods containing only one neighbor
		mutate(W = ifelse(size == 1, map(w, ~matrix(.x)), W)) |>
		mutate(X = ifelse(size == 1, map(X, ~matrix(.x, nrow = 1)), X)) |>
		mutate(P = ifelse(size == 1, map(P, ~matrix(.x, nrow = 1)), P)) |>
		select(GEOID_1, X, P, W) |>
		mutate(D = pmap(list(X, P, W), do_regression)) |>
		select(GEOID_1, D, X, P)
		
	hessians <- proportions |>
		mutate(H = map(p, hessian))

	derivs |>
		left_join(hessians, by = c("GEOID_1" = "GEOID")) |> 
		rename(GEOID = GEOID_1) |>
		mutate(g = map2(D, H, ~ .x %*% .y %*% t(.x))) |>
		mutate(det = map_dbl(g, ~det(.x)), 
			   trace = map_dbl(g, ~sum(diag(.x))))
}
