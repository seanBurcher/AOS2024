library(dplyr)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# UTILS
source("R/functions/utils/get_time_value.R")
# NDOE
source("R/functions/node/node_functions.R")
# TAG
source("R/functions/tag/tag_functions.R")
# SIDEKICK
source("R/functions/sidekick/load_sidekick_data.R")
# CALIBRATION
source("R/functions/calibration/calibration_functions.R")

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
# Specify the path to the sidekick data file you recorded for calibration
sidekick_file_path <- "data/meadows/sidekick/calibration_2023_8_3_t2.csv"
# Specify the path to your database file
database_file <- "~/Desktop/full_data/meadows.duckdb"

# Specify the tag ID that you used in your calibration
my_tag_id <- "072A6633"

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where you nodes were in
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
start_time <- as.POSIXct("2023-08-01 00:00:00", tz = "GMT")
stop_time <- as.POSIXct("2023-08-07 00:00:00", tz = "GMT")

# Specify a list of node Ids if you only want to include a subset in calibration
# IF you want to use all nodes, ignore this line and SKIP the step below
# where the data frame is trimmed to only nodes in this list
# my_nodes <- c("B25AC19E", "44F8E426", "FAB6E12", "1EE02113", "565AA5B9", "EE799439", "1E762CF3", "A837A3F4", "484ED33B")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
node_health_df <- tbl(con, "node_health") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)
node_health_df <- node_health_df %>% distinct(node_id, time, recorded_at, .keep_all = TRUE)
## -----------------------------------------------------------------------------
##  2.) GET NODE LOCATIONS
## -----------------------------------------------------------------------------
# Calculate the average node locations
node_locs <- calculate_node_locations(node_health_df)
# Plot the average node locations
node_loc_plot <- plot_node_locations(node_health_df, theme = classic_plot_theme)
node_loc_plot
# Write the node locations to a file
export_node_locations("examples/meadows/output/node_locations.csv", node_locs)
# Draw a map with the node locations
node_map <- map_node_locations(node_locs, tile_url = my_tile_url)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA FROM FILES
## -----------------------------------------------------------------------------
# Load from DB
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = database_file, read_only = TRUE)
detection_df <- tbl(con, "raw") |> 
  filter(time >= start_time & time <= stop_time) |>
  collect()
DBI::dbDisconnect(con)

# Load from File
# detection_df <- load_node_detection_data(tag_data_directory, start_time = start_time, stop_time = stop_time)

# Get beeps from test tag only
detection_df <- subset.data.frame(detection_df, tag_id == my_tag_id)

## -----------------------------------------------------------------------------
##  4.) LOAD SIDEKICK CALIBRATION DATA FROM FILE
## -----------------------------------------------------------------------------
# Get Sidekick data from CSV (note I modified the headers for cleaner names in here)
sidekick_all_df <- load_sidekick_data(sidekick_file_path)
# Get beeps from test tag only
sidekick_tag_df <- subset.data.frame(sidekick_all_df, tag_id == my_tag_id)
# Show location of all beeps in relation to node locations
calibration_map <- map_calibration_track(node_locs, sidekick_tag_df, tile_url = my_tile_url)
calibration_map


## -----------------------------------------------------------------------------
##  CONVERT TO X-Y COORDINATES
## -----------------------------------------------------------------------------
origin_lat <- 38.93393539
origin_lon <- -74.94797955

lat_to_y <- function(lat, origin_lat, origin_lon) {
  dist <- haversine(origin_lat,origin_lon,lat,origin_lon)
  if(lat < origin_lat) {
    return(-dist)
  } else {
    return(dist)
  }
}

lon_to_x <- function(lon, origin_lat, origin_lon) {
  dist <- haversine(origin_lat,origin_lon,origin_lat,lon)
  if(lon < origin_lon) {
    return(-dist)
  } else {
    return(dist)
  }
}

node_locs <- node_locs %>%
  rowwise() %>%
  mutate(x = lon_to_x(avg_lon,origin_lat,origin_lon)) %>%
  ungroup()
node_locs <- node_locs %>%
  rowwise() %>%
  mutate(y = lat_to_y(avg_lat,origin_lat,origin_lon)) %>%
  ungroup()
ggplot() +
  geom_point(data = node_locs, mapping = aes(x = x, y = y, label = node_id), color = "steelblue", shape = 15, size = 3) +
  geom_text(data = node_locs, mapping = aes(x = x, y = y, label = node_id), vjust = 2) +
  labs(x = "X (m)", y = "Y (m)", title = "Study Area") +
  classic_plot_theme

write.csv(node_locs,"~/Desktop/meadows_node_locs.csv")

sidekick_all_df <- sidekick_all_df %>%
  mutate(t_epoch = as.numeric(time_utc))

sidekick_all_df <- sidekick_all_df %>%
  mutate(t = t_epoch - sidekick_all_df$t_epoch[1])

sidekick_all_df <- sidekick_all_df %>%
  filter(t <= 1060)

calibration_map <- map_calibration_track(node_locs, sidekick_all_df, tile_url = my_tile_url)
calibration_map


sidekick_all_df <- sidekick_all_df %>%
  rowwise() %>%
  mutate(x = lon_to_x(lon,origin_lat,origin_lon)) %>%
  ungroup()
sidekick_all_df <- sidekick_all_df %>%
  rowwise() %>%
  mutate(y = lat_to_y(lat,origin_lat,origin_lon)) %>%
  ungroup()

sidekick_track <- sidekick_all_df %>%
  select(t,x,y)

interpolate_track <- function(actual_data) {
  # Sort data by time to ensure proper interpolation
  actual_data <- actual_data[order(actual_data$t), ]
  
  # Create a complete sequence of integer seconds from min to max time
  min_time <- floor(min(actual_data$t))
  max_time <- ceiling(max(actual_data$t))
  complete_times <- seq(min_time, max_time, by = 1)
  
  # Interpolate x and y coordinates
  interpolated_x <- approx(x = actual_data$t, y = actual_data$x, 
                           xout = complete_times, method = "linear")$y
  interpolated_y <- approx(x = actual_data$t, y = actual_data$y, 
                           xout = complete_times, method = "linear")$y
  
  # Create the interpolated dataframe
  interpolated_data <- data.frame(
    t = complete_times,
    x = interpolated_x,
    y = interpolated_y
  )
  
  return(interpolated_data)
}

track <- interpolate_track(sidekick_track)

ggplot() +
  geom_point(data = node_locs, mapping = aes(x = x, y = y, label = node_id), color = "steelblue", shape = 15, size = 3) +
  geom_text(data = node_locs, mapping = aes(x = x, y = y, label = node_id), vjust = 2) +
  geom_point(data = track, mapping = aes(x = x, y = y), color = "red", shape = 20, size = 2) +
  geom_path(data = track, mapping = aes(x = x, y = y), color = "red") +
  labs(x = "X (m)", y = "Y (m)", title = "Study Area") +
  classic_plot_theme

write.csv(track,"~/Desktop/meadows_sidekick_track.csv")
