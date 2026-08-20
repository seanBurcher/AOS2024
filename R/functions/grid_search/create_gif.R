library(ggplot2)
library(gganimate)
library(magick)

create_tracking_gif <- function(
    track_df,
    grid_data_list,
    node_locs = NULL,
    plot_width = 8, plot_height = 10,
    fps = 2) {
    # Vector to store plot filenames
    plot_files <- character(nrow(track_df))

    for (i in 1:nrow(track_df)) {
        time <- track_df$time[i]
        p <- plot_grid_values(
            grid_values = grid_data_list[[i]],
            track_df = track_df,
            time_step = time,
            node_locs = node_locs
        )
        filename <- sprintf("outputs/gif_frames_tmp/plot_%03d.png", i)
        ggsave(filename, p, width = plot_width, height = plot_height, unit = "in", dpi = 500)
        plot_files[i] <- filename

        # Progress indicator
        if (i %% 10 == 0) cat("Created plot", i, "of", nrow(track_df), "\n")
    }

    # Create GIF using magick
    # cat("Creating GIF...\n")
    # images <- image_read(plot_files)
    # animation <- image_animate(images, fps = fps)
    # image_write(animation, path = file.path(output_dir, gif_filename))
    # cat("GIF saved as:", file.path(output_dir, gif_filename), "\n")

    # Optionally clean up individual plot files
    # file.remove(plot_files)

    return(file.path(output_dir, gif_filename))
}
