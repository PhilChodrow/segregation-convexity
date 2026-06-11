library(systemfonts)

# system_fonts() |>
#   filter(grepl("Avenir", family)) |>
#   select(name, family, style, weight, width) |>
#   print(n = Inf)

systemfonts::register_variant(
  name = "Avenir-Medium",
  family = "Avenir",
  weight = "medium",
)

font_theme <- theme(
  text = element_text(family = "Avenir"),
)


plot_theme <- theme_minimal() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.background = element_rect(),
              panel.grid.major = element_line(size = 0),
              panel.grid.minor = element_line(size = 0),
              plot.margin=unit(c(0,0,0,0),"mm")) + 
              font_theme

darkgrey <- "#1d1d1d"