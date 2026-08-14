library(tidyverse)
library(sf)
source("src/constructors.R")

DKL <- function(p, q) {
	sum(p * log(p / q), na.rm = TRUE)
}

# custom information loss measurement
information_loss <- function(n, m, M) {
		p     <- n / sum(n)
		q     <- m / sum(m)
		p_bar <- (m + n) / sum(m + n)
		r     <- M / sum(M)
		sum(n) / sum(M) * DKL(p, r) +
		sum(m) / sum(M) * DKL(q, r) -
		sum(m + n) / sum(M) * DKL(p_bar, r)
}


hclust_sf <- function(geo){

	demographic_lookup <- geo |>
		mutate(id = 1:n()) |>
		st_set_geometry(NULL) |> 
		nest(-GEOID, -id) |>
		mutate(n = map(data, as.integer)) |>
		select(GEOID, id, n)

    adj <- st_relate(geo, pattern = '****1****', sparse = TRUE) 
	
	adj <- 1:length(adj) |>
		map(~data_frame(from = .,
						to = adj[[.]])) |>
		reduce(rbind) |>
		filter(from != to) |>
		left_join(demographic_lookup, by = c('from' = 'id')) |>
		left_join(demographic_lookup, by = c('to' = 'id'), suffix = c('_from', '_to'))

	n <- nrow(geo)
	names <- geo$GEOID

	M <- geo |> 
		st_set_geometry(NULL) |> 
		select(-GEOID) |> 
		colSums()

	adj <- adj |> 
		mutate(info_loss = map2_dbl(n_from, n_to, ~information_loss(.x, .y, M))) 

	# main loop

	merge <- matrix(nrow = n-1, ncol = 2)
	height <- numeric(length = n-1)
	cluster_stage <- 1

	for (i in 1:(n-1)) { 
		print(paste("Iteration:", i, "Length of adjacency:", nrow(adj)))
		# choose merge to perform
		new_cluster_name <- n+i
		ix <- which.min(adj$info_loss)
		loss <- min(adj$info_loss)
		

		# integer ids of the best merge
		i <- adj$from[ix]
		j <- adj$to[ix]

		if(i <= n){
			eye <- -i
		}else{
			eye <- i - n
		}

		if(j <= n){
			jay <- -j
		}else{
			jay <- j - n
		}

		# record the merge in the hclust data structure
		height[cluster_stage] <- loss
		merge[cluster_stage, c(1, 2)] <- c(eye, jay)

		# now need to update the adjacency matrix
		op <- adj |>
			filter(from %in% c(i, j) | to %in% c(i, j))
		
		ij_n <- adj |>
			filter(from == i, to == j) |>
			select(n_from, n_to)
		
		ij_n <- ij_n$n_from[[1]] + ij_n$n_to[[1]]

		adj <- adj |>
			anti_join(op, by = c("from", "to"))

		op <- op |>
			filter(!(from == i & to == j) & !(from == j & to == i)) |>
			select(-info_loss)

		test <- op |> 
			mutate(new = rep(list(ij_n), nrow(op)))
		
		test$n_from <- ifelse(test$from %in% c(i, j), test$new, test$n_from)
		test$n_to   <- ifelse(test$to   %in% c(i, j), test$new, test$n_to)

		test <- test |>
			mutate(from = ifelse(from %in% c(i,j), new_cluster_name, from),
				   to = ifelse(to %in% c(i,j), new_cluster_name, to))

		test <- test |> 
			select(-new) |>
			mutate(info_loss = map2_dbl(n_from, n_to, ~information_loss(.x, .y, M)))

		adj <- adj |>
			rbind(test) |>
			distinct(from, to, .keep_all = TRUE)
		cluster_stage <- cluster_stage + 1

			
	}

	# return the data structure
	a <- list()
	a$merges <- merge
	a$height <- height
	a$labels <- names
	a$order  <- names
	class(a) <- "hclust"
	a
}