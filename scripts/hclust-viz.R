library(tidycensus, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tigris, quietly = TRUE)
library(sf, quietly = TRUE)
library(patchwork, quietly = TRUE)
source("src/local-info.R")
source("src/get-data.R")
source("scripts/style.R")
source("src/hclust.R")
source("src/maps.R")

args <- commandArgs(trailingOnly=TRUE)
city <- args[1]

city <- "Milwaukee"

geo <- readRDS(paste0("throughput/geo/", city, ".rds")) |>
    pivot_wider(names_from = variable, values_from = estimate)

# for hierarchical clustering viz
h <- hclust_sf(geo)

# dotmap under the hierarchical clustering viz
dots <- make_dots(geo, people_per_dot = 100) # people represented per dot

# aggregation levels from administrative divisions

geo_cbg      <- get_census_data(city, "cbg")
geo_tracts   <- get_census_data(city, "tract")
geo_counties <- get_census_data(city, "county subdivision")

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
            0,
            mutual_information(geo_counties |> select(-GEOID) |> st_drop_geometry()),
            mutual_information(geo_tracts |> select(-GEOID) |> st_drop_geometry()), 
            mutual_information(geo_cbg |> select(-GEOID) |>
        st_drop_geometry())
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
    paste("City\n", "(", 1, ")", sep = " "),
    paste("County\nSubdivisions\n", "(", num_counties, ")", sep = " "), 
    paste("Tracts\n", "(", num_tracts, ")", sep = " "), 
    paste("Blockgroups\n", "(", num_blockgroups, ")", sep = " ")
)

info_df <- tibble(
    level = names(infos),
    mutual_information = unname(infos), 
    k = c(1, num_counties, num_tracts, num_blockgroups)
)

info_df <- info_df |>
    arrange(mutual_information)

info_df <- info_df |>
    mutate(level = factor(level, levels = names(infos)))




# figure construction

## first the dotmap/hierarchical

# basemap plus dots
r <- ggplot() + 
    geom_sf(data = geo, fill = "#eee2e2", color = "#eee2e2") + 
    theme_void() + 
    geom_sf(data = dots, aes(color = variable), size = 0.5) + 
    scale_color_manual(values = palette) + 
    guides(color = guide_legend(override.aes = list(size = 5), ncol = 3, title = element_blank())) + 
    theme(legend.position = c(0.5,-0.05), 
          legend.text = element_text(size = 11)) + 
    font_theme

# add the hierarchical boundaries
r <- hclust_map(geo, h, k = 4, r) 


# information loss associated with hierarchical clustering
height_df <- tibble(height = rev(h$height)) |>
    mutate(k = row_number() + 1, 
           cumulative_info = cumsum(height)) |> 
    rbind(tibble(height = 0, k = 1, cumulative_info = 0))

q <- height_df |>
    filter(k <= 30) |>
    ggplot() + 
    aes(x = k, y = cumulative_info) + 
    geom_line() + 
    geom_point() +
    theme_minimal() +
    geom_hline(yintercept = sum(height_df$height), linetype = "dashed") + 
    font_theme + 
    labs(x = "Number of Algorithmic Clusters", y = "Shannon Mutual Information") + 
    theme(axis.line = element_line(color = darkgrey))

# information loss associated with city-level aggregation

p <- info_df |>
    ggplot() + 
    aes(x = level, y = mutual_information, group = 1) + 
    geom_line() +
    geom_point() + 
    theme_minimal() + 
    font_theme + 
    labs(x = "Administrative Subdivision Level", y = "Shannon Mutual Information") + 
    theme(axis.line = element_line(color = darkgrey)) + 
    geom_hline(yintercept = sum(height_df$height), linetype = "dashed") 

## bring it all together

layout <- "
AB
AC
"

full_plot <- r + q + p + plot_layout(design = layout, widths = c(1, 1))
ggsave("fig/hclust-viz.png", full_plot, width = 7, height = 6)






# # dotmap 




# points <- make_dots(geo, scale = 500)

# q <- ggplot() + 
#     geom_sf(data = geo, fill = "#CCCCCC", color = "#CCCCCC") +
#     geom_sf(data = points, aes(color = variable)) +
#     theme_void()

# ggsave("fig/dotmap-experiment.png", q, width = 16, height = 6)



