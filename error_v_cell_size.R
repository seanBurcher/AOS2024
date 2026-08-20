library(dplyr)
library(ggplot2)
source("R/defs/plot_themes.R")

error_data <- read.csv("R/error_cell_size.csv")

error_data <- error_data %>%
  filter(grid_cell_size <= 50)

ggplot(error_data, aes(x = grid_cell_size)) +
  geom_point(mapping = aes(y = gs_error), size = 3) +
  geom_errorbar(mapping = aes(ymin = gs_error - gs_sd / sqrt(n_points), 
                    ymax = gs_error + gs_sd/ sqrt(n_points)), 
                width = 2) +
  labs(x = "Grid Cell Size (m)", y = "Mean Position Error (m)") +
  publication_plot_theme


ggplot(error_data, aes(x = grid_cell_size)) +
  geom_point(mapping = aes(y = run_time), size = 3) +
  labs(x = "Grid Cell Size (m)", y = "Run Time (s)") +
  publication_plot_theme
