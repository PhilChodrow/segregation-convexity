library(tidyverse)
library(ggtern)
library(nleqslv)
library(patchwork)
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
          tern.axis.line = element_line(color = darkgrey, size = 0.1), 
          tern.axis.title = element_text(size = 20), 
          tern.axis.title.L = element_text(hjust = -.05, vjust = 1),
          tern.axis.title.R = element_text(hjust = 1, vjust = 1)) + 
    labs(color = "Shannon entropy (nats)") + 
    # ggtitle("Isocontours of Shannon Entropy on the 3-Simplex") + 
    font_theme +
    labs(x = "A", y = "B", z = "C") +
    theme_hidelabels() + 
    theme_hideticks() + 
    guides(color = "none")


ggsave("fig/simplex-entropy.png", r, width = 3.5, height = 3)


num_bins <- 3
e     <- rep(1, num_bins)

LT <- matrix(0, nrow = num_bins, ncol = num_bins)
for(i in 1:num_bins){
    for(j in 1:num_bins){
        if(i >= j){
            LT[i, j] <- 1
        }
    }
}

isocontour <- function(p_1, mean_w, contour, init = 0.1){
    upper_index <- ceiling(mean_w)
    lower_index <- floor(mean_w)
    target_e <- rep(0, num_bins)
    target_e[upper_index] <- mean_w - lower_index
    target_e[lower_index] <- upper_index - mean_w

    f <- function(p_2){
        p_3 <- 1 - p_1 - p_2
        p <- c(p_1, p_2, p_3)
        LTp <- LT %*% p
        out <- sum((LTp - target_e)^2 - contour)
        # print(paste(p_2, out))
        return(out)
    }

    p_2 <- nleqslv(init, f)$x
    p_3 <- 1 - p_1 - p_2
    return(c(p_1, p_2, p_3))
}

isocontour_safe <- function(p_1, mean_w, contour, init = 0.1){
    tryCatch({
        return(isocontour(p_1, mean_w, contour, init))
    }, error = function(e){
        return(c(NA, NA, NA))
    })
}

DF <- expand.grid(
    p_1 = seq(0, 1, 0.001), 
    mean_w = seq(1.5, 2.5, 0.5), 
    contour = 2^(-seq(0, 5, 0.1))
) |>
    tibble() |>
    filter(p_1 <= 1) |>
    rowwise() |>
    mutate(p = list(isocontour_safe(p_1, mean_w, contour)),
           p_2 = p[[2]], 
           p_3 = p[[3]]) |>
    filter(!is.na(p_2), !is.na(p_3)) |>
    filter(p_2 >= 0, p_3 >= 0) |>
    select(p_1, p_2, p_3, mean_w, contour) |>
    filter(abs(p_1 + p_2 + p_3 - 1) < 1e-6) 

subspace_df <- expand.grid(
    p_1 = seq(0, 1, 0.01), 
    p_2 = seq(0, 1, 0.01)) |>
    tibble() |>
    mutate(p_3 = 1 - p_1 - p_2) |>
    mutate(mean_w = (1*p_1 + 2*p_2 + 3*p_3) / (p_1 + p_2 + p_3)) |>
    filter(mean_w == 2.0)
    


q <- DF |> 
    filter(mean_w == 2) |>
    filter(p_3 < 0.99) |>
    arrange(contour) |>
    mutate(p = p_1 + p_2 + p_3) |>
    ggtern(aes(x = p_1, z = p_2, y = p_3)) + 
    geom_point(aes(color = -contour), size = 0.5) + 
    scale_color_viridis_c() +
    # geom_line(data = subspace_df, aes(x = p_1, y = p_2, z = p_3), color = "black", size = 0.5, linetype = "dashed") +  
    theme_bw() + 
    theme(legend.position = "bottom", 
          panel.grid.major = element_line(color = "white"), 
          panel.grid.minor = element_line(color = "white"), 
          plot.margin = margin(0, 0.2, 0, 0.2), 
          tern.axis.line = element_line(color = darkgrey, size = 0.1), 
          tern.axis.title = element_text(face = "italic", size = 20), 
          tern.axis.title.L = element_text(hjust = -.05, vjust = 1),
          tern.axis.title.R = element_text(hjust = 1, vjust = 1)) + 
    labs(color = "Squared distance from target cumulative distribution") + 
    font_theme +
    labs(x = "w = 0", y = "w = 1", z = "w = 2") +
    theme_hidelabels() + 
    theme_hideticks() + 
    guides(color = "none") 
    
ggsave("fig/simplex-cumulative.png", r, width = 3.5, height = 3)

patch_tern <- function(tern) {
  tern |> ggplot_build() |> ggplot_gtable() |> wrap_elements()
}


p <- patch_tern(r) + patch_tern(q)

ggsave("fig/simplex-isocontours.png", p, width = 6.2, height = 3)