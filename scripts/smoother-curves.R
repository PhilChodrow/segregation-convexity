library(tidyverse)
library(patchwork)
source("scripts/style.R")
source("src/maps.R")
source("src/utils.R")
source("src/local-info.R")
args <- commandArgs(trailingOnly=TRUE)
cities <- args[1:length(args)]

SIGMAS <- seq(1, 5001, 500)

cities <- c("Detroit", "Boston")

DF <- tibble()

for(j in 1:length(cities)){
    city <- cities[j]
    geo <- geo <- readRDS(paste0("throughput/geo/", city, ".rds"))

    geo <- geo |>
    group_by(GEOID) |>
    filter(sum(estimate) > 0)
 
    # data prep: widen and smooth
    geo <- geo |>
        pivot_wider(names_from = variable, values_from = estimate)

    demos <- geo |>
        st_drop_geometry()

    for(sigma in SIGMAS){

        smoothed_demos <- spatial_rbf_smoother(demos, geo, sigma = sigma)
        MI <- smoothed_demos |>
            select(-GEOID) |>
            mutual_information()
        
        print(paste("Processing", city, "with bandwidth ", sigma, "MI = ", MI))

        df <- tibble(
            city = city, 
            sigma = sigma, 
            MI = MI
        )

        DF <- rbind(DF, df)
    }

}

r <- DF |>
    ggplot() + 
    aes(x = sigma, y = MI, linetype = city) + 
    geom_line(color = darkgrey) + 
    geom_point(pch = 21, fill = "white", color = darkgrey, size = 2) + 
    theme_minimal() + 
    font_theme + 
    theme(axis.line = element_line(color = darkgrey), legend.position = c(0.8, 0.7)) + 
    xlab("Kernel bandwidth (m)") + 
    ylab("Shannon mutual information (nats)") + 
    guides(linetype = guide_legend(title = element_blank()))


ggsave("fig/smoother_curves.png", width = 4, height = 3.2)