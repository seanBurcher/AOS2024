library(dplyr)
library(duckdb)
library(ggplot2)

options(digits = 10)

# DEFS
source("R/defs/plot_themes.R")
# UTILS
source("R/functions/utils/get_time_value.R")
# NDOE
source("R/functions/node/node_functions.R")
# TAG
source("R/functions/tag/load_node_detection_data.R")
# SIDEKICK
source("R/functions/sidekick/load_sidekick_data.R")
# GRID SEARCH
source("R/functions/grid_search/grid_search_functions.R")

my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
node_data_file <- "data/stefan/Nodes.csv"
test_point_data_file <- "data/stefan/test_points.csv"
beep_data_file <- "data/stefan/BeepData.csv"
# Specify the tag ID that you used in your calibration
my_tag_id <- "1E78191E"

# Specify the RSSI vs Distance fit coefficients from calibration
a <- -106.17107083262
b <- -35.43355448807
c <- 0.01421921366
rssi_coefs <- c(a, b, c)

## -----------------------------------------------------------------------------
##  2.) GET NODE LOCATIONS
## -----------------------------------------------------------------------------
# Read Node data file
node_data_df <- read.csv(node_data_file)

source("R/functions/utils/convert_utm_to_latlon.R")
converted_data <- convert_utm_to_latlon(node_data_df, 16, "N", "NodeUTMx", "NodeUTMy")

node_locs <- converted_data %>%
  rename(avg_lat = latitude) %>%
  rename(avg_lon = longitude) %>%
  rename(node_id = NodeId)

# Draw a map with the node locations
node_map <- map_node_locations(node_locs)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA
## -----------------------------------------------------------------------------
detection_df <- read.csv(beep_data_file)

detection_df$time_utc <- as.POSIXct(detection_df$Time, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")

# Get beeps from test tag only
detection_df <- subset.data.frame(detection_df, TagId == my_tag_id)

detection_df <- detection_df %>%
  rename(tag_id = TagId) %>%
  rename(node_id = NodeId) %>%
  rename(tag_rssi = TagRSSI) %>%
  rename(time = time_utc)

detection_df$time <- as.POSIXct(detection_df$time, tz = "UTC")
detection_df <- detection_df %>% mutate(time_value = as.integer(time))

## -----------------------------------------------------------------------------
##  4.) BUILD A GRID
## -----------------------------------------------------------------------------
grid_center_lat <- 36.539537521479645
grid_center_lon <- -87.36046106364083
grid_size_x <- 600 # meters
grid_size_y <- 600 # meters
grid_bin_size <- 2 # meters
# Create a data frame with the details about the grid
grid_df <- build_grid(
  node_locs = node_locs,
  center_lat = grid_center_lat,
  center_lon = grid_center_lon,
  x_size_meters = grid_size_x,
  y_size_meters = grid_size_y,
  bin_size = grid_bin_size
)
# Draw all of the grid bins on a map
grid_map <- draw_grid(node_locs, grid_df)
grid_map

## -----------------------------------------------------------------------------
##  6.) CALCULATE TRACK
## -----------------------------------------------------------------------------
# track_frame_output_path <- "output/track_frames/"
track_results <- calculate_track(
  start_time = "2025-02-26 19:55:00",
  length_seconds = 3600+1800,
  step_size_seconds = 5,
  det_time_window = 15, # Must have detection within this window to be included in position calculation
  filter_alpha = 0.7,
  filter_time_range = 60, # Time range to include detections in filtered value
  grid_df = grid_df,
  detection_df = detection_df,
  node_locs = node_locs,
  rssi_coefs = rssi_coefs,
  track_frame_output_path = NULL # If NULL no individual frames will be saved
)
print(track_df)
track_map <- map_track(node_locs, track_results$track_df, my_tile_url)
track_map

##########################################
# Make GIF!!!!
##########################################
source("R/functions/grid_search/create_gif.R")
source("R/functions/grid_search/plot_grid_values.R")
create_tracking_gif(
  track_df = track_results$track_df,
  grid_data_list = track_results$grid_data_list,
  node_locs = node_locs
)

## -----------------------------------------------------------------------------
## LOAD TEST POINT DATA
## -----------------------------------------------------------------------------
test_point_data <- read.csv(test_point_data_file)
test_point_data_converted <- convert_utm_to_latlon(test_point_data, 16, "N", "TestUTMx", "TestUTMy")

# create start_time_utc and stop_time_utc columns
test_point_data_converted <- test_point_data_converted %>%
  mutate(
    start_time_utc = as.POSIXct(paste(Date, paste0(hour_UTC, ":", Min)),
                                format = "%m/%d/%y %H:%M",
                                tz = "UTC"),
    stop_time_utc = start_time_utc + 60  # Assuming 3-minute duration
  )