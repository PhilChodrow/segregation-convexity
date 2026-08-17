library(tidyverse)
library(ggtern)


resolution = 1/10

df <- expand.grid(
    x = seq(0, 1, resolution), 
    y = seq(0, 1, resolution), 
    z = seq(0, 1, resolution)
) |>
    filter(x + y + z == 1.0) |>
    tibble()


df <- df |>
    mutate(euc = x^2 + y^2 + z^2, 
           ent = x*log(x) + y*log(y) + z*log(z)) |>
    mutate(ent = ifelse(is.na(ent), 0, ent)) |>
    pivot_longer(-c(x, y, z)) |>
    mutate(name = ifelse(name == "euc", "Euclidean Norm", "Shannon Entropy")) 
    
    # |>
    # group_by(name) |>
    # mutate(value = value / max(value, na.rm = T))



r <- ggtern(data = df, aes(x = x, y = y, z = z, value = value)) +
#   geom_point(alpha = 0.5) +
  geom_interpolate_tern(
        stat = "InterpolateTern",
        method = "auto",
        na.rm = TRUE,
        # formula = value ~ x + y + z,
        expand = c(1, 1, 1),
        base = "identity",
        aes(
            colour = after_stat(level)
        ),
        size = 0.5
    ) + 
    facet_wrap(~name) + 
    theme_minimal() +
    scale_color_viridis_c()


ggsave("fig/simplex.png", r, width = 10, height = 5)