load_sidekick_data <- function(sidekick_file_path) {
    df <- read.csv(sidekick_file_path)

    num_cols <- ncol(df)
    print(num_cols)

    if (num_cols == 8) {
        colnames(df) <- c("tag_type", "tag_id", "time_utc", "rssi", "lat", "lon", "heading", "antenna_angle")
    } else if (num_cols == 11) {
        colnames(df) <- c("tag_type", "tag_id", "time_utc", "rssi", "lat", "lon", "sync", "tag_family", "payload_type", "solar_mV", "temperature_C")
    }
    df$time_utc <- as.POSIXct(df$time_utc, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")


    return(df)
}
