library(systemfonts)

systemfonts::register_variant(
  name = "Avenir-Light",
  family = "Avenir",
  weight = "Regular",
)

system_fonts() |>
    filter(str_detect(family, "Avenir")) |>
    View()





plot_theme <- theme_minimal() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.background = element_rect(),
              panel.grid.major = element_line(size = 0),
              panel.grid.minor = element_line(size = 0),
              plot.margin=unit(c(0,0,0,0),"mm"),
              text = element_text(family = "Avenir Next Medium", color = "black"))