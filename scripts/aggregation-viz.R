library(tidycensus, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tigris, quietly = TRUE)
library(sf, quietly = TRUE)
library(patchwork, quietly = TRUE)
source("scripts/style.R")


args <- commandArgs(trailingOnly=TRUE)
city_to_retrieve <- args[1]

cities <- read_csv("assumptions/cities.csv") 


race_variable_name_map <- c(
          "Estimate!!Total:!!Not Hispanic or Latino:!!White alone" = "White","Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone" = "Black", 
          "Estimate!!Total:!!Not Hispanic or Latino:!!American Indian and Alaska Native alone" = "Other",
           "Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone" = "Asian",
          "Estimate!!Total:!!Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone" = "Other",
          "Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:" = "Other", 
          "Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:!!Two races including Some other race" = "Other", 
          "Estimate!!Total:!!Not Hispanic or Latino:!!Two or more races:!!Two races excluding Some other race, and three or more races" = "Other", 
          "Estimate!!Total:!!Hispanic or Latino:!!White alone" = "Hispanic",
          "Estimate!!Total:!!Hispanic or Latino:!!Black or African American alone" = "Hispanic",
          "Estimate!!Total:!!Hispanic or Latino:!!American Indian and Alaska Native alone" = "Hispanic",                             
          "Estimate!!Total:!!Hispanic or Latino:!!Asian alone" = "Hispanic",
          "Estimate!!Total:!!Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone" = "Hispanic", 
          "Estimate!!Total:!!Hispanic or Latino:!!Some other race alone" = "Hispanic",  
          "Estimate!!Total:!!Hispanic or Latino:!!Two or more races:" = "Hispanic", 
          "Estimate!!Total:!!Hispanic or Latino:!!Two or more races:!!Two races including Some other race" = "Hispanic",
          "Estimate!!Total:!!Hispanic or Latino:!!Two or more races:!!Two races excluding Some other race, and three or more races" = "Hispanic"
)

race_variables <- load_variables(2024, "acs5", cache = TRUE) |>
    filter(str_detect(name, "B03002")) |>
    select(name, label) 

fields <- c(race_variables$name)
names(fields) <- race_variables$label

get_census_data <- function(chosen_city, geography){
    city_info <- cities |>
        filter(name == chosen_city) 

    state <- city_info$state[1]
    counties <- city_info$county 

    geo <- get_acs(
        geography = geography,
        variables = fields,
        state = state,
        county = counties,
        geometry = TRUE,
        year = 2024
    ) |>
      st_transform(4326)

    geo <- geo |>
      filter(variable %in% names(race_variable_name_map)) |>
      mutate(variable = recode(variable, !!!race_variable_name_map)) |>
      group_by(GEOID, variable) |>
      summarize(estimate = sum(estimate), .groups = "drop") 

    # filter empty geometries
    geo <- geo |>
      filter(!st_is_empty(geometry))

    # filter geometries with zero population
    geo <- geo |>
        group_by(GEOID) |>
        filter(sum(estimate) > 0)

    return(geo)    
}

geo_cbg <- get_census_data(city_to_retrieve, "cbg")
geo_tracts <- get_census_data(city_to_retrieve, "tract")
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