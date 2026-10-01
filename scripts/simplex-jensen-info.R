library(tidyverse)
library(ggtern)
library(nleqslv)
library(patchwork)
source("scripts/style.R")



# visualization of the Jensen information

n_points <- 5

# google search AI
rdirichlet_base <- function(n, alpha) {
  gammas <- matrix(rgamma(length(alpha) * n, shape = alpha, rate = 1), 
                   nrow = n, byrow = TRUE)
  gammas / rowSums(gammas)
}
alpha <- c(1, 1, 1)

set.seed(123)
data <- rdirichlet_base(n_points, alpha) |>
    as_tibble() |>
    rename(p1 = V1, p2 = V2, p3 = V3) |>
    mutate(weight = runif(n_points, 0, 1)) |>
    mutate(mean_p1 = weighted.mean(p1, weight),
           mean_p2 = weighted.mean(p2, weight),
           mean_p3 = weighted.mean(p3, weight)) 

mean_point <- data |>
    summarise(mean_p1 = weighted.mean(p1, weight),
              mean_p2 = weighted.mean(p2, weight),
              mean_p3 = weighted.mean(p3, weight))


sub_digits <- c("\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089")

labels <- paste0("y", sub_digits[1:n_points])

# make a vector of ggplot2 expressions giving the divergence labels

divergence_labels <- paste0("D[phi] * (italic(y)[", 1:n_points, "] ~ \"||\" ~ bar(italic(y)))")

r <- data |>
    ggtern() + 
    geom_segment(aes(x = mean_p1, xend = p1, y = mean_p2, yend = p2, z = mean_p3, zend = p3), size = 0.5, color = darkgrey) + 
    geom_point(aes(x = mean_p1, y = mean_p2, z = mean_p3), size = 6, data = mean_point, pch = 21, fill = "white") +
    geom_point(aes(x = p1, y = p2, z = p3, size  = weight*0.5), color = darkgrey) +
    scale_color_viridis_c() + 
    theme_bw() + 
    font_theme +
    theme(legend.position = "bottom",
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          plot.margin = margin(0, 0, 0, 0),
          tern.axis.line = element_line(color = darkgrey, size = 0.1), 
          tern.axis.title.L = element_text(hjust = -.05, vjust = 1),
          tern.axis.title.R = element_text(hjust = 1, vjust = 1)) +
    theme_hidelabels() + 
    theme_hideticks() + 
    annotate(geom = "text", x = mean_point$mean_p1, y = mean_point$mean_p2, z = mean_point$mean_p3, label = "ȳ", size = 3, color = darkgrey, vjust = 0.4, hjust = 0.5, fontface = "italic") + 
    annotate(
        geom = "text", 
        x = data$p1, 
        y = data$p2, 
        z = data$p3, 
        label = labels, 
        size = 2, 
        color = darkgrey, 
        hjust = c(2, 3.7, 2, 4, -0.5)*0.5 ,
        vjust = c(-3, -3, -2, -1, -2.5)*0.5,
        family = "sans", 
        fontface = "italic") + 
    annotate(geom = "text", 
        x = (data$p1 + mean_point$mean_p1)/2, 
        y = (data$p2 + mean_point$mean_p2)/2, 
        z = (data$p3 + mean_point$mean_p3)/2, 
        label = divergence_labels, 
        size = 1.7, 
        color = darkgrey, 
        hjust = c(0.0, 0, 1.0, 2.2, -0.6)*0.5, 
        vjust = c(2.0, 2.5, -2.0, 1.5, 1.5)*0.5 , 
        parse = TRUE) + 
    guides(size = "none") + 
    labs(x = "", y = "", z = "") +
    scale_size_continuous(range = c(1, 3)) 

ggsave("fig/simplex-jensen-info.png", r, width = 3.0, height = 3.0, dpi = 600)

# Now we are going to try the cumulative distribution story for an economic equality measure

