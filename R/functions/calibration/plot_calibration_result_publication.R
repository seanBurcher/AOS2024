library(ggplot2)

plot_calibration_result_publication <- function(rssi_v_dist, theme = NULL) {
    plot <- ggplot() +
        geom_point(
            data = rssi_v_dist,
            aes(
                x = distance,
                y = rssi
            )
        ) +
        geom_line(
            data = rssi_v_dist,
            aes(
                x = distance,
                y = pred
            ),
            linewidth = 2.0,
            color = "red"
        ) +
        labs(title = "", x = "Distance (m)", y = "RSSI (dBm)") +
        xlim(0, 600) +
        ylim(-125, -25)

    return(plot + theme)
}
