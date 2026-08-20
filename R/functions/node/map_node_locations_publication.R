library(dplyr)
library(leaflet)

map_node_locations_publication <- function(
    node_locs,
    track_error_df,
    sidekick_df,
    tile_url = "https://tile.openstreetmap.org/{z}/{x}/{y}.png") {
    error_lines <- data.frame(
        id = integer(),
        lat = double(),
        lon = double()
    )
    for (i in 1:nrow(track_error_df)) {
        track_solution <- track_error_df[i, ]
        line_start <- data.frame(
            id = track_solution$i,
            lat = track_solution$act_lat,
            lon = track_solution$act_lon
        )
        line_end <- data.frame(
            id = track_solution$i,
            lat = track_solution$sol_lat,
            lon = track_solution$sol_lon
        )
        error_lines <- rbind(error_lines, line_start)
        error_lines <- rbind(error_lines, line_end)
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

    left_lon <- -74.9515
    right_lon <- -74.940500
    bottom_lat <- 38.932
    top_lat <- 38.940482

    scale_start_coords <- c(bottom_lat, left_lon)
    tick_top_lat <- scale_start_coords[1] + 0.0002
    scale_color <- "black"
    scale_weight <- 3

    lon50 <- get_delta_lon(scale_start_coords[1], 50) + scale_start_coords[2]
    lon100 <- get_delta_lon(scale_start_coords[1], 100) + scale_start_coords[2]
    lon150 <- get_delta_lon(scale_start_coords[1], 150) + scale_start_coords[2]
    lon200 <- get_delta_lon(scale_start_coords[1], 200) + scale_start_coords[2]
    lon250 <- get_delta_lon(scale_start_coords[1], 250) + scale_start_coords[2]
    lon300 <- get_delta_lon(scale_start_coords[1], 300) + scale_start_coords[2]

    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles(
            urlTemplate = tile_url,
            options = tileOptions(maxZoom = 20)
        ) %>%
        addCircles(
            data = node_locs,
            lat = node_locs$avg_lat,
            lng = node_locs$avg_lon,
            radius = 5.0,
            stroke = FALSE,
            fillOpacity = 1.0,
            fillColor = "cyan"
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], scale_start_coords[1]),
            lng = c(scale_start_coords[2], lon300),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat),
            lng = c(scale_start_coords[2], scale_start_coords[2]),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat - (tick_top_lat - scale_start_coords[1]) / 2),
            lng = c(lon50, lon50),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat),
            lng = c(lon100, lon100),
            weight = scale_weight,
            color = scale_color,
            opacity = 100.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat - (tick_top_lat - scale_start_coords[1]) / 2),
            lng = c(lon150, lon150),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat),
            lng = c(lon200, lon200),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat - (tick_top_lat - scale_start_coords[1]) / 2),
            lng = c(lon250, lon250),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        addPolylines(
            lat = c(scale_start_coords[1], tick_top_lat),
            lng = c(lon300, lon300),
            weight = scale_weight,
            color = scale_color,
            opacity = 1.0
        ) %>%
        fitBounds(
            lng1 = left_lon, lat1 = top_lat,
            lng2 = right_lon, lat2 = bottom_lat,
        ) %>%
        addPolylines(
            data = sidekick_df,
            lat = sidekick_df$lat,
            lng = sidekick_df$lon,
            color = "blue",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = sidekick_df,
            lat = sidekick_df$lat,
            lng = sidekick_df$lon,
            radius = 1,
            color = "blue",
            fillColor = "blue",
            fillOpacity = 1.0,
            label = as_datetime(sidekick_df$time_value)
        ) %>%
        addPolylines(
            data = track_error_df,
            lat = track_error_df$sol_lat,
            lng = track_error_df$sol_lon,
            color = "red",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = track_error_df,
            lat = track_error_df$sol_lat,
            lng = track_error_df$sol_lon,
            radius = 1,
            color = "red",
            fillColor = "red",
            fillOpacity = 1.0,
            label = paste(track_error_df$i, ":", as_datetime(track_error_df$time), " : ", track_error_df$error)
        ) %>%
        # Multilat Solutions
        addPolylines(
            data = track_df,
            lat = track_df$ml_lat,
            lng = track_df$ml_lon,
            color = "orange",
            weight = 2
        ) %>%
        addCircleMarkers(
            data = track_df,
            lat = track_df$ml_lat,
            lng = track_df$ml_lon,
            radius = 1,
            color = "orange",
            fillColor = "orange",
            fillOpacity = 1.0,
            label = as_datetime(track_df$time)
        )
    return(map)
}
