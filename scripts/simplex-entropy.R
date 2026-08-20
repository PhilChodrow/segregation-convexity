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
          tern.axis.line = element_line(color = darkgrey, size = 0.1)) + 
    labs(color = "Shannon entropy (nats)") + 
    # ggtitle("Isocontours of Shannon Entropy on the 3-Simplex") + 
    font_theme +
    labs(x = "A", y = "B", z = "C") +
    theme_hidelabels() + 
    theme_hideticks() + 
    guides(color = "none")


ggsave("fig/simplex-entropy.png", r, width = 3.5, height = 3)