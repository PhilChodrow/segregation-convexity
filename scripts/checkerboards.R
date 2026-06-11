library(tidyverse)
source("scripts/style.R")


plot_theme <- theme_minimal() +
		theme(axis.ticks = element_blank(),
			  axis.text.x = element_blank(),
			  axis.text.y = element_blank(),
			  panel.background = element_rect(),
			  panel.grid.major = element_line(size = 0),
			  panel.grid.minor = element_line(size = 0),
			  plot.margin=unit(c(0,0,0,0),"mm"))

df <- expand.grid(x = 1:8, y = 1:8) %>%
	mutate(`(a)` = 0,
		   `(b)` = .5,
		   `(c)` = (x + y) %% 2, 
		   `(d)` = (x > 4)*1)

r <- df %>% 
	gather(key = model, value = p, -x, -y) %>%
	ggplot(aes(x = x, y = y)) +
	plot_theme + 
	geom_tile(aes(fill = p)) +
	scale_fill_continuous(
		low = 'white', 
		high = '#1d1d1d', 
		limits=c(0,1), 
		breaks = c(0, .5, 1),
		labels = scales::percent) +
	facet_wrap(~model, nrow = 2) +
	scale_y_continuous(expand = c(0,0)) +
	scale_x_continuous(expand = c(0,0)) +	
	xlab('') +
	ylab('') +
	# guides(fill=FALSE) + 
	coord_fixed() + 
	theme(panel.border = element_rect(color = 'black', fill = NA, size = 0.5),
		  strip.text = element_text(size = 16), 
		  legend.position = 'bottom', 
		  panel.spacing.x = unit(1.2, 'lines'), 
		  plot.margin=grid::unit(c(0,0,0,0), "mm"), 
		  text = element_text(family = "Avenir", color = "black")) + 
	guides(fill = guide_colorbar(title.position = 'top', title.hjust = 0.5, nrow = 1)) + 
	labs(fill = 'Density of Group A') 

# retitle the legend


# make the fig directory if it doesn't exist already
if (!dir.exists("fig")) {
	dir.create("fig")
}

ggsave("fig/checkerboards.png", r, width = 5, height = 6, dpi = 300,   bg = "#FFFFFF"
)

system2(command = "pdfcrop", 
        args    = c("fig/checkerboards.pdf", 
                    "fig/checkerboards.pdf") 
        )


