library(tidycensus, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(tigris, quietly = TRUE)
library(sf, quietly = TRUE)

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
        filter(sum(estimate) > 0) |>
        pivot_wider(names_from = variable, values_from = estimate)

    return(geo)    
}
