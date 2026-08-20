# Function to create a single grid plot for one time step
plot_grid_values <- function(
    grid_values,
    track_df,
    time_step = NULL,
    node_locs = NULL,
    title = "Grid Values") {
    # Create the base plot
    p <- ggplot() +
        geom_tile(
            data = grid_values,
            mapping = aes(x = center_x, y = center_y, fill = value)
        ) + # Creates colored grid cells
        scale_fill_viridis_c(name = "Grid\nValue", guide = "none") + # Color scale
        labs(x = "X (m)", y = "Y (m)", title = title) +
        theme_minimal() +
        coord_equal() # Equal aspect ratio


    # current_input_data <- input_track %>%
    #     filter(t <= time_step)
    # # Input Point and Track
    # p <- p + geom_path(
    #     data = current_input_data,
    #     mapping = aes(x = x, y = y),
    #     color = "black"
    # )
    # p <- p + geom_point(
    #     data = current_input_data[nrow(current_input_data), ],
    #     mapping = aes(x = x, y = y),
    #     color = "white", size = 2, shape = 21,
    #     fill = "black", stroke = 1.5, inherit.aes = FALSE
    # )

    current_solution_data <- track_df %>%
        filter(time <= time_step)
    # Solution Point and Track
    p <- p + geom_path(
        data = current_solution_data,
        mapping = aes(x = x, y = y),
        color = "red"
    )
    p <- p + geom_point(
        data = current_solution_data[nrow(current_solution_data), ],
        mapping = aes(x = x, y = y),
        color = "white", size = 2, shape = 21,
        fill = "red", stroke = 1.5, inherit.aes = FALSE
    )

    # Add node locations if provided
    if (!is.null(node_locs)) {
        p <- p + geom_point(
            data = node_locs, aes(x = x, y = y),
            color = "black", shape = 15, size = 3,
            inherit.aes = FALSE
        )
    }

    # Add time step to title if provided
    if (!is.null(time_step)) {
        p <- p + labs(title = paste(title, "- Time Step:", time_step))
    }

    p <- p + guides(
        colour = guide_legend(show = FALSE)
    )

    p <- p + classic_plot_theme

    return(p)
}
