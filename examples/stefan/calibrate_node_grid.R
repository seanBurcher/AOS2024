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
node_data_file <- "data/stefan/Nodes.csv"
test_point_data_file <- "data/stefan/test_points.csv"
beep_data_file <- "data/stefan/BeepData.csv"

# Specify the tag ID that you used in your calibration
#my_tag_id <- "1E66192A"
#my_tag_id <- "2D4B5519"
my_tag_id <- "1E78191E"

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##  1.) LOAD NODE HEALTH DATA FROM FILES
## -----------------------------------------------------------------------------
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
##  3.) LOAD STATION DETECTION DATA FROM FILES
## -----------------------------------------------------------------------------
detection_data <- read.csv(beep_data_file)

detection_data$time_utc <- as.POSIXct(detection_data$Time, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")

# Get beeps from test tag only
detection_data <- subset.data.frame(detection_data, TagId == my_tag_id)

detection_data <- detection_data %>%
  rename(tag_id = TagId) %>%
  rename(node_id = NodeId) %>%
  rename(tag_rssi = TagRSSI) %>%
  rename(time = time_utc)

## -----------------------------------------------------------------------------
##  4.) LOAD TEST POINT DATA
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

map <- leaflet() %>%
  addTiles(
    urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    options = tileOptions(maxZoom = 20)
  ) %>%
  addCircles(
    data = node_locs,
    lat = node_locs$avg_lat,
    lng = node_locs$avg_lon,
    label = node_locs$node_id,
    stroke = FALSE,
    radius = 5.0,
    fillOpacity = 1.0,
    fillColor = "blue"
  ) %>%
  addCircles(
    data = test_point_data_converted,
    lat = test_point_data_converted$latitude,
    lng = test_point_data_converted$longitude,
    radius = 2.0,
    label = test_point_data_converted$time_utc,
    stroke = FALSE,
    fillOpacity = 1.0,
    fillColor = "red"
  )
map

# For each test point
#  get all detections with in start_time_utc and stop_time_utc
#  save a row with tag_id, time_utc, lat, lon

# Initialize empty dataframe for sidekick tag data
sidekick_tag_df <- data.frame(
  tag_id = character(),
  time_utc = as.POSIXct(character()),
  lat = numeric(),
  lon = numeric(),
  stringsAsFactors = FALSE
)

# For each test point, find matching detections within the time window
for (i in 1:nrow(test_point_data_converted)) {
  test_point <- test_point_data_converted[i, ]
  
  # Find detections within the time window for this test point
#  matching_detections <- detection_data[
#    detection_data$time >= test_point$start_time_utc & 
#    detection_data$time <= test_point$stop_time_utc, 
#  ]
#  print("Matching detections in time window")
  matching_detections <- subset.data.frame(detection_data, time >= test_point$start_time_utc & time <= test_point$stop_time_utc)
  
  # If we have matching detections, add them to sidekick_tag_df
  if (nrow(matching_detections) > 0) {
    for (j in 1:nrow(matching_detections)) {
      sidekick_tag_df <- rbind(sidekick_tag_df, data.frame(
        tag_id = my_tag_id,
        time_utc = matching_detections$time[j],
        lat = test_point$latitude,
        lon = test_point$longitude,
        stringsAsFactors = FALSE
      ))
    }
  }
}

## -----------------------------------------------------------------------------
##  5.) CALCULATE THE RSSI VS DISTANCE RELATIONSHIP
## -----------------------------------------------------------------------------
# This function will match sidekick detections to detections recorded by nodes
# and sent to the station.  Then using the sidekick location, the node locations
# calculated above, and the rssi measured in the node, a list of rssi and
# distance pairs is generated and returned
# For Blu Series tags use_sync=TRUE, for 434 MHz tags use_sync=FALSE
source("R/functions/calibration/calibration_functions.R")
rssi_v_dist <- calc_rssi_v_dist(node_locs, sidekick_tag_df, detection_data, use_sync = FALSE)

# Plot the resulting RSSI and distance data
ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi, colour = node_id)) +
  labs(title="RSSI vs. Distance",x="Distance (m)",y="RSSI (dBm)",colour="Node ID") +
  classic_plot_theme

# Fit the RSSI vs distance data with exponential relationship
nlsfit <- nls(
  rssi ~ a - b * exp(-c * distance),
  rssi_v_dist,
  start = list(a = -105, b = -60, c = 0.05)
)
summary(nlsfit)
# Get the coefficients from the fit result
co <- coef(summary(nlsfit))
rssi_coefs <- c(co[1, 1], co[2, 1], co[3, 1])

# Add a predicted column to the RSSI vs distance data
rssi_v_dist$pred <- predict(nlsfit)
# Plot the RSSI vs distance data with the fit curve
calibration_plot <- plot_calibration_result(rssi_v_dist, classic_plot_theme)
calibration_plot

# Print the coefficients from the fit. You'll need these coefficients later
# for localization.
print(rssi_coefs)

## -----------------------------------------------------------------------------
##  GRID CALIBRATED!!!
## -----------------------------------------------------------------------------

