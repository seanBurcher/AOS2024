library(dplyr)
library(data.table)
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
sidekick_file_path <- "data/ctt_office_v2/sidekick/long_slow_cal.csv"

# ALL THE TAGS!!!
classic_blu_bat <- "870614B1"
castle_blu_bat <- "23D9727E"
caslte_blu_bat_black <- "D172633A"
power_tag <- "1E336107"

# Specify the tag ID that you used in your calibration
my_tag_id <- power_tag

# Specify the time range of node data you want to import for this analysis
#   This range should cover a large time window where you nodes were in
#   a constant location.  All node health records in this time window
#   will be used to accurately determine the position of your nodes
start_time <- as.POSIXct("2025-05-27 19:00:00", tz = "GMT")
stop_time <- as.POSIXct("2023-08-07 21:00:00", tz = "GMT")

# Specify a list of node Ids if you only want to include a subset in calibration
# IF you want to use all nodes, ignore this line and SKIP the step below
# where the data frame is trimmed to only nodes in this list
my_nodes <- c("B995250B", "4D64B5B5", "7378B941", "9CC5DC79", "D46BA6A2", "7D769A01", "AA4358FB", "CB4A1EAE", "520A3C5C")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------



node_health_df <- read.csv("data/ctt_office_v2/station/node_health.csv")
node_health_df <- node_health_df %>%
  distinct(NodeId, RecordedAt, .keep_all = TRUE)
node_health_df <- node_health_df %>%
  filter(NodeId %in% my_nodes) %>%
  filter(!is.na(Latitude))

node_health_df <- node_health_df %>%
  rename(node_id = NodeId) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude)

node_locs_gps <- calculate_node_locations(node_health_df)

# Plot the average node locations
node_loc_plot <- plot_node_locations(node_health_df, node_locs, theme = classic_plot_theme)
node_loc_plot
# Write the node locations to a file
export_node_locations("examples/meadows/output/node_locations.csv", node_locs)


## -----------------------------------------------------------------------------
##  2.) GET NODE LOCATIONS
## -----------------------------------------------------------------------------
node_locs <- read.csv("data/ctt_office_v2/node_locs_adjusted.csv")

# Draw a map with the node locations
node_map <- map_node_locations(node_locs, tile_url = my_tile_url)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA FROM FILES
## -----------------------------------------------------------------------------
detection_df <- read.csv("data/ctt_office_v2/station/434_detection.csv")
# Rename to database naming format
detection_df <- detection_df %>%
  rename(tag_id = TagId) %>%
  rename(tag_rssi = TagRSSI) %>%
  rename(time = Time) %>%
  rename(node_id = NodeId) %>%
  rename(radio_id = RadioId)

# Get beeps from test tag only
detection_df <- subset.data.frame(detection_df, tag_id == my_tag_id)
# Convert date strings ot POSIX dates
detection_df$time <- as.POSIXct(detection_df$time, tz = "UTC")
# Remove all dupiclate rows
# (same detection data but was picked up by multiple radios on the station)
detection_df <- detection_df %>% distinct(tag_id, node_id, time, tag_rssi, .keep_all = TRUE)

source("examples/ctt_office_v2/load_node_files.R")
node_det_df <- load_node_data_434("data/ctt_office_v2/node_files/")
node_det_df <- node_det_df %>%
  rename(tag_rssi = rssi)
node_det_df <- node_det_df %>%
  filter(tag_id == my_tag_id)

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
##  5.) CALCULATE THE RSSI VS DISTANCE RELATIONSHIP
## -----------------------------------------------------------------------------
# This function will match sidekick detections to detections recorded by nodes
# and sent to the station.  Then using the sidekick location, the node locations
# calculated above, and the rssi measured in the node, a list of rssi and
# distance pairs is generated and returned
# For Blu Series tags use_sync=TRUE, for 434 MHz tags use_sync=FALSE
rssi_v_dist <- calc_rssi_v_dist(node_locs, sidekick_tag_df, node_det_df, use_sync = FALSE)

# Plot the resulting RSSI and distance data
ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi, colour = node_id)) +
  labs(title = "RSSI vs. Distance", x = "Distance (m)", y = "RSSI (dBm)", colour = "Node ID") +
  classic_plot_theme

# Fit the RSSI vs distance data with exponential relationship
nlsfit <- nls(
  rssi ~ a - b * exp(-c * distance),
  rssi_v_dist,
  start = list(a = -105, b = -60, c = 0.17)
)
summary(nlsfit)
# Get the coefficients from the fit result
co <- coef(summary(nlsfit))
rssi_coefs <- c(co[1, 1], co[2, 1], co[3, 1])

# Add a predicted column to the RSSI vs distance data
rssi_v_dist$pred <- predict(nlsfit)
# Plot the RSSI vs distance data with the fit curve
calibration_plot <- plot_calibration_result(rssi_v_dist, classic_plot_theme) + ylim(-70, -25)
calibration_plot

# Print the coefficients from the fit. You'll need these coefficients later
# for localization.
print(rssi_coefs)

## -----------------------------------------------------------------------------
##  GRID CALIBRATED!!!
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##  Characterize Measurement Noise
## -----------------------------------------------------------------------------

ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi - pred)) +
  classic_plot_theme

rssi_v_dist$error <- NA

for (i in 1:nrow(rssi_v_dist)) {
  rssi_v_dist$error[i] <- rssi_v_dist$rssi[i] - rssi_v_dist$pred[i]
}

rssi_v_dist <- rssi_v_dist %>%
  filter(error >= -25 & error <= 25)

ggplot() +
  geom_histogram(data = rssi_v_dist, aes(x = error), color = "black", fill = "steelblue", alpha = 0.5, binwidth = 1) +
  labs(x = "Difference from Model (dBm)") +
  publication_plot_theme

mean(rssi_v_dist$error)
sd(rssi_v_dist$error)

#############################################
library(MASS)

binwidth <- 1
# Fit Gaussian distribution using maximum likelihood
fit <- fitdistr(rssi_v_dist$error, "normal")
mu <- fit$estimate["mean"]
sigma <- fit$estimate["sd"]

# Print fitted parameters
cat("Fitted Gaussian Parameters:\n")
cat(sprintf("Mean (μ): %.4f ± %.4f\n", mu, fit$sd["mean"]))
cat(sprintf("Standard Deviation (σ): %.4f ± %.4f\n", sigma, fit$sd["sd"]))
cat(sprintf("Sample size: %d\n", length(rssi_v_dist$error)))

# Create sequence for fitted curve
x_range <- range(rssi_v_dist$error)
x_seq <- seq(-20, 20, length.out = 200)

# Calculate scaling factor to match histogram counts
# We need to scale the normal density to match the histogram bin counts
bin_count <- length(rssi_v_dist$error) * binwidth
fitted_curve <- dnorm(x_seq, mean = mu, sd = sigma) * bin_count

# Create the plot matching your style
p <- ggplot() +
  # Histogram (matching your exact style)
  geom_histogram(
    data = data.frame(error = rssi_v_dist$error),
    aes(x = error),
    color = "black",
    fill = "steelblue",
    alpha = 0.5,
    binwidth = binwidth
  ) +
  # Fitted Gaussian curve
  geom_line(aes(x = x_seq, y = fitted_curve),
    color = "red", size = 1.2
  ) +
  # Labels matching your style
  labs(
    x = "Measured RSSI - Model RSSI (dBm)",
    y = "Counts / dBm",
  ) +
  scale_x_continuous(limits = c(-20, 20)) +
  # Your theme
  publication_plot_theme
p
