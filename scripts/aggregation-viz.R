library(tidycensus, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tigris, quietly = TRUE)
library(sf, quietly = TRUE)
library(patchwork, quietly = TRUE)
source("scripts/style.R")
source("src/get-data.R")


args <- commandArgs(trailingOnly=TRUE)
city_to_retrieve <- args[1]


geo_cbg      <- get_census_data(city_to_retrieve, "cbg")
geo_tracts   <- get_census_data(city_to_retrieve, "tract")
geo_counties <- get_census_data(city_to_retrieve, "county subdivision")

binarize <- function(geo, category = "Black"){
    geo |>
        mutate(bin = if_else(variable == category, 1, 0)) |>
        group_by(GEOID, bin) |>
        summarize(estimate = sum(estimate), .groups = "drop") |>
        pivot_wider(names_from = bin, values_from = estimate) |>
        select(-GEOID) |>
        st_drop_geometry()
}

# Shannon mutual information for a data frame representing a contingency table
mutual_information <- function(df){
    # Compute the joint probability distribution
    joint_prob <- df / sum(df)
    
    # Compute the marginal distributions
    row_prob <- rowSums(joint_prob)
    col_prob <- colSums(joint_prob)

    # Compute the mutual information
    mi <- 0
    for(i in seq_len(nrow(joint_prob))){
        for(j in seq_len(ncol(joint_prob))){
            if(joint_prob[i, j] > 0){
                mi <- mi + joint_prob[i, j] * log(joint_prob[i, j] / (row_prob[i] * col_prob[j]))
            }
        }
    }
    return(mi)
}

infos <- c(
            mutual_information(binarize(geo_cbg)), 
            mutual_information(binarize(geo_tracts)),
            mutual_information(binarize(geo_counties)), 
            0
    )

num_blockgroups <- geo_cbg |>
    distinct(GEOID) |>
    nrow()
num_tracts <- geo_tracts |>
    distinct(GEOID) |>
    nrow()

num_counties <- geo_counties |>
    distinct(GEOID) |>
    nrow()


names(infos) <- c(
    paste("Blockgroups", "(", num_blockgroups, ")", sep = " "), 
    paste("Tracts", "(", num_tracts, ")", sep = " "), 
    paste("County\nSubdivisions", "(", num_counties, ")", sep = " "), 
    paste("City", "(", 1, ")", sep = " ")
)

info_df <- tibble(
    level = names(infos),
    mutual_information = unname(infos)
)

info_df <- info_df |>
    mutate(level = factor(level, levels = names(infos)))



p_black <- geo_cbg |>
    group_by(GEOID) |>
    mutate(p = estimate / sum(estimate)) |>
    filter(variable == "Black")
    
r <- ggplot() + 
    geom_sf(data = p_black, aes(fill = p),  color = darkgrey, linewidth = 0.1) +
    scale_fill_gradient(low = "white", high = "#d8d154", limits = c(0, 1), na.value = "white") + 
    geom_sf(data = geo_tracts, alpha = 0, color = darkgrey, linewidth = 0.3) +
    geom_sf(data = geo_counties, alpha = 0, color = darkgrey, linewidth = 1.2) +
    theme_void() + 
    font_theme + 
    guides(fill = guide_colorbar(title = "Proportion Black")) + 
    theme(legend.position = c(0.85, 0.4))

p <- info_df |>
    ggplot() + 
    aes(x = level, y = mutual_information, group = 1) + 
    geom_line() +
    geom_point() + 
    theme_minimal() + 
    font_theme + 
    labs(x = "Geographic Level", y = "Shannon Mutual Information\n(Black vs. Nonblack)") + 
    theme(axis.line = element_line(color = darkgrey))
    

q <- free(r) / p + plot_layout(nrow = 2, heights = c(5, 1))
ggsave("fig/aggregation-viz.png", plot = q, width = 5, height = 6)

tex_macros <- paste0(
    "\\newcommand{\\aggBlockgroupsInfo}{", round(infos["Blockgroups"], 2), "}\n",
    "\\newcommand{\\aggTractsInfo}{", round(infos["Tracts"], 2), "}\n",
    "\\newcommand{\\aggCountySubdivisionsInfo}{", round(infos["County\nSubdivisions"], 2), "}\n",
    "\\newcommand{\\aggCityInfo}{", round(infos["City"], 2), "}\n", 
    "\\newcommand{\\aggCity}{", city_to_retrieve, "}\n"
)
cat(tex_macros, file = "params/aggregation-viz.tex")