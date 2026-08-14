id_lookup <- function(tracts, key_col = 'GEOID'){
	tracts[[key_col]] %>%
		data_frame(row = as.character(1:length(.)), GEOID = .)
}
