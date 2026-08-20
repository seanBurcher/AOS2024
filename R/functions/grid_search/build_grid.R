build_grid <- function(node_locs, center_lat, center_lon, x_size_meters, y_size_meters, bin_size) {
    get_delta_lat <- function(lat, dist) {
        pi <- 3.1415926535897932
        earth_radius <- 6371.0e3 # meters
        lat1_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lat2_rad <- asin(sin(lat1_rad) * cos(angular_dist) + cos(lat1_rad) * sin(angular_dist))
        lat2_deg <- lat2_rad * 180 / pi
        return(lat2_deg - center_lat)
    }

    get_delta_lon <- function(lat, dist) {
        pi <- 3.1415926535897932
        earth_radius <- 6371.0e3 # meters
        lat1_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lon2_rad <- atan2(sin(angular_dist) * cos(lat1_rad), cos(angular_dist) - sin(lat1_rad) * sin(lat1_rad))
        lon2_deg <- lon2_rad * 180 / pi
        return(lon2_deg)
    }

    delta_lon <- get_delta_lon(center_lat, x_size_meters / 2)
    delta_lat <- get_delta_lat(center_lat, y_size_meters / 2)

    num_bins_x <- x_size_meters / bin_size
    num_bins_y <- y_size_meters / bin_size

    grid_data_frame <- data.frame(
        i = integer(),
        lat1 = double(),
        lon1 = double(),
        lat2 = double(),
        lon2 = double(),
        center_lat = double(),
        center_lon = double()
    )

    starting_lat <- center_lat - delta_lat
    starting_lon <- center_lon - delta_lon
    d_lat <- get_delta_lat(center_lat, bin_size)
    d_lon <- get_delta_lon(center_lat, bin_size)

    bin_count <- 1
    for (iy in 1:num_bins_y) {
        for (ix in 1:num_bins_x) {
            lat1 <- starting_lat + (iy - 1) * d_lat
            lon1 <- starting_lon + (ix - 1) * d_lon

            grid_bin <- data.frame(
                i = bin_count,
                lat1 = lat1,
                lon1 = lon1,
                lat2 = lat1 + d_lat,
                lon2 = lon1 + d_lon,
                center_lat = lat1 + d_lat / 2,
                center_lon = lon1 + d_lon / 2
            )
            grid_data_frame <- rbind(grid_data_frame, grid_bin)
            bin_count <- bin_count + 1
        }
    }
    return(grid_data_frame)
}


build_grid_optimized <- function(node_locs, center_lat, center_lon, x_size_meters, y_size_meters, bin_size) {
    # Pre-calculate constants
    pi <- 3.1415926535897932
    earth_radius <- 6371.0e3 # meters
    center_lat_rad <- center_lat * pi / 180.0

    # Optimized delta calculation functions (vectorized where possible)
    get_delta_lat_fast <- function(lat, dist) {
        lat_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lat2_rad <- asin(sin(lat_rad) * cos(angular_dist) + cos(lat_rad) * sin(angular_dist))
        return((lat2_rad * 180 / pi) - lat)
    }

    get_delta_lon_fast <- function(lat, dist) {
        lat_rad <- lat * pi / 180.0
        angular_dist <- dist / earth_radius
        lon2_rad <- atan2(sin(angular_dist) * cos(lat_rad), cos(angular_dist) - sin(lat_rad) * sin(lat_rad))
        return(lon2_rad * 180 / pi)
    }

    # Calculate grid parameters
    delta_lon <- get_delta_lon_fast(center_lat, x_size_meters / 2)
    delta_lat <- get_delta_lat_fast(center_lat, y_size_meters / 2)

    num_bins_x <- as.integer(x_size_meters / bin_size)
    num_bins_y <- as.integer(y_size_meters / bin_size)
    total_bins <- num_bins_x * num_bins_y

    # Pre-calculate starting positions and deltas
    starting_lat <- center_lat - delta_lat
    starting_lon <- center_lon - delta_lon
    d_lat <- get_delta_lat_fast(center_lat, bin_size)
    d_lon <- get_delta_lon_fast(center_lat, bin_size)

    # Pre-allocate vectors (much faster than rbind)
    grid_data_frame <- data.frame(
        i = 1:total_bins,
        lat1 = numeric(total_bins),
        lon1 = numeric(total_bins),
        lat2 = numeric(total_bins),
        lon2 = numeric(total_bins),
        center_lat = numeric(total_bins),
        center_lon = numeric(total_bins)
    )

    # Vectorized grid generation
    bin_count <- 1
    for (iy in 1:num_bins_y) {
        # Calculate row indices
        row_start <- (iy - 1) * num_bins_x + 1
        row_end <- iy * num_bins_x

        # Vectorized calculations for entire row
        lat1 <- starting_lat + (iy - 1) * d_lat
        lon1_vec <- starting_lon + (0:(num_bins_x - 1)) * d_lon

        # Fill in the data frame row by row
        grid_data_frame$lat1[row_start:row_end] <- lat1
        grid_data_frame$lon1[row_start:row_end] <- lon1_vec
        grid_data_frame$lat2[row_start:row_end] <- lat1 + d_lat
        grid_data_frame$lon2[row_start:row_end] <- lon1_vec + d_lon
        grid_data_frame$center_lat[row_start:row_end] <- lat1 + d_lat / 2
        grid_data_frame$center_lon[row_start:row_end] <- lon1_vec + d_lon / 2 # Fixed: was d_lat/2
    }

    return(grid_data_frame)
}
