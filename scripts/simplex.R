library(tidyverse)
library(ggtern)
library(nleqslv)
source("scripts/style.R")


H_isocontour <- function(p_1, H, init = 0.1){
  
    f <- function(p_2){
        p_3 <- 1 - p_1 - p_2
        if(p_3 < 0) return(NA)
        H_calc <- -p_1*log(p_1) - p_2*log(p_2) - p_3*log(p_3)
        return(H_calc - H)
    }

    p_2 <- nleqslv(init, f)$x
    p_3 <- 1 - p_1 - p_2
    return(c(p_1, p_2, p_3))
}

safe_isocontour <- function(p_1, H, init = 0.1){
    tryCatch({
        return(H_isocontour(p_1, H, init))
    }, error = function(e){
        return(c(NA, NA, NA))
    })
}


DF <- expand.grid(
    p_1 = seq(0, 1, 0.01), 
    H = seq(0.1, log(3) - 0.00001, log(3) / 7), 
    init = seq(0, 1, 0.05)
) |>
    tibble() |>
    filter(p_1 + init <= 1) |>
    rowwise() |>
    mutate(p = list(safe_isocontour(p_1, H, init)),
           p_2 = p[[2]], 
           p_3 = p[[3]]) |>
    filter(!is.na(p_2), !is.na(p_3)) |>
    filter(p_2 >= 0, p_3 >= 0) |>
    select(p_1, p_2, p_3, H) |>
    mutate(H_test = -p_1*log(p_1) - p_2*log(p_2) - p_3*log(p_3)) |>
    filter(abs(H - H_test) < 1e-6) |>
    select(-H_test) 


DF <- DF |>
    mutate(H_test = -p_1*log(p_1) - p_2*log(p_2) - p_3*log(p_3)) |>
    filter(abs(H - H_test) < 1e-6) |>
    select(-H_test) 

DF_2 <- DF |>
    rename(p_1 = p_2, p_2 = p_3, p_3 = p_1)

DF_3 <- DF |>
    rename(p_1 = p_3, p_2 = p_1, p_3 = p_2)

DF_all <- rbind(DF, DF_2, DF_3)


# filter to unique values of p_1, p_2, p_3, H
DF_all <- DF_all |>
    distinct(p_1, p_2, p_3, H) 


r <- DF_all |>
    ggtern(aes(x = p_1, y = p_2, z = p_3, color = H, group = H)) + 
    geom_point(size = 0.5) + 
    scale_color_viridis_c() + 
    theme_bw() + 
    theme(legend.position = "bottom", 
          panel.grid.major = element_line(color = "white"), 
          panel.grid.minor = element_line(color = "white"), 
        #   text = element_text(face = "italic"), 
          plot.margin = margin(0, 0, 0, 0), 
          tern.axis.line = element_line(color = darkgrey, size = 0.1)) + 
    labs(color = "Shannon entropy (nats)") + 
    # ggtitle("Isocontours of Shannon Entropy on the 3-Simplex") + 
    font_theme +
    labs(x = "A", y = "B", z = "C") +
    theme_hidelabels() + 
    theme_hideticks() + 
    guides(color = "none")


ggsave("fig/simplex-entropy.png", r, width = 3.5, height = 3)

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

divergence_labels <- paste0("D[phi] * (y[", 1:n_points, "] ~ \"||\" ~ bar(y))")

r <- data |>
    ggtern() + 
    geom_segment(aes(x = mean_p1, xend = p1, y = mean_p2, yend = p2, z = mean_p3, zend = p3), size = 0.5, color = darkgrey) + 
    geom_point(aes(x = mean_p1, y = mean_p2, z = mean_p3), size = 6, data = mean_point, pch = 21, fill = "white") +
    geom_point(aes(x = p1, y = p2, z = p3, size = weight*0.5), color = darkgrey) +
    scale_color_viridis_c() + 
    theme_bw() + 
    font_theme +
    theme(legend.position = "bottom",
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          plot.margin = margin(0, 0, 0, 0),
          tern.axis.line = element_line(color = darkgrey, size = 0.1)) +
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
        size = 1.5, 
        color = darkgrey, 
        hjust = c(0.0, 0, 1.0, 2.2, -0.6)*0.5, 
        vjust = c(2.0, 2.5, -2.0, 1.5, 1.5)*0.5 , 
        parse = TRUE) + 
    guides(size = "none") + 
    labs(x = "A", y = "B", z = "C") +
    scale_size_continuous(range = c(1, 3)) 

ggsave("fig/simplex-information.png", r, width = 3.0, height = 3, dpi = 600)

# lower triangular matrix

num_bins <- 100
alpha <- rep(1, num_bins)
w <- 1:num_bins
p <- rdirichlet_base(1, alpha) 



LT <- matrix(0, nrow = num_bins, ncol = num_bins)
for(i in 1:num_bins){
    for(j in 1:num_bins){
        if(i >= j){
            LT[i, j] <- 1
        }
    }
}

test <- LT %*% t(LT) 
solve(test,w)