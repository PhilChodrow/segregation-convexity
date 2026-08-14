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

darkgrey <- "#1d1d1d"


palette <- c(
  "White" = "#ED6A5A",
  "Black" = "#5b8781", 
  "Asian" = "#4ECDC4", 
  "Hispanic" = "#d8d154", 
  "Other" = "#37505C"
)



checkerboard_config <- list(
                        scale_fill_continuous(
                              low = 'white', 
                              high = palette["Other"], 
                              breaks = c(0, .5, 1),
                              labels = scales::percent, 
                              limits = c(0, 1)),
                        scale_x_continuous(expand = c(0,0)),
                        scale_y_continuous(expand = c(0,0)),
                        coord_sf()
)
           
checkerboard_theme <- theme(
                        legend.position = 'bottom', 
                        panel.spacing = unit(1.2, 'lines'), 
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
                        strip.text =  element_text(vjust = 1.5, size = 15), 
                        strip.background = element_blank(), 
                        axis.text = element_blank(), 
                        axis.ticks = element_blank()
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

