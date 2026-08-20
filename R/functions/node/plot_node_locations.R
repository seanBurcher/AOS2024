library(ggplot2)

plot_node_locations <- function(health_df, locs, theme = NULL) {
    plot <- ggplot() +
        geom_point(
            data = locs,
            aes(
                x = avg_lon,
                y = avg_lat,
                colour = node_id
            ),
            shape = 15,
            size = 3
        ) +
        geom_point(
            data = health_df,
            aes(
                x = longitude,
                y = latitude,
                colour = node_id
            ),
            shape = 1,
            size = 1
        ) +
        xlab("Longitude") +
        ylab("Latitude")

    return(plot + theme)
}
