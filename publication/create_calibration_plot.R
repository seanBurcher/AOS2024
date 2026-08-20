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
sidekick_file_path <- "data/meadows/sidekick/calibration_2023_8_3_all.csv"
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
node_loc_plot <- plot_node_locations(node_health_df, node_locs, theme = classic_plot_theme)
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
##  5.) CALCULATE THE RSSI VS DISTANCE RELATIONSHIP
## -----------------------------------------------------------------------------
# This function will match sidekick detections to detections recorded by nodes
# and sent to the station.  Then using the sidekick location, the node locations
# calculated above, and the rssi measured in the node, a list of rssi and
# distance pairs is generated and returned
# For Blu Series tags use_sync=TRUE, for 434 MHz tags use_sync=FALSE
rssi_v_dist <- calc_rssi_v_dist(node_locs, sidekick_tag_df, detection_df, use_sync = FALSE)

# Plot the resulting RSSI and distance data
ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi, colour = node_id)) +
  labs(title="RSSI vs. Distance",x="Distance (m)",y="RSSI (dBm)",colour="Node ID") +
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
calibration_plot <- plot_calibration_result(rssi_v_dist, classic_plot_theme)
calibration_plot

# Print the coefficients from the fit. You'll need these coefficients later
# for localization.
print(rssi_coefs)

## -----------------------------------------------------------------------------
##  GRID CALIBRATED!!!
## -----------------------------------------------------------------------------
source("R/defs/plot_themes.R")
source("R/functions/calibration/plot_calibration_result_publication.R")
plot <- plot_calibration_result_publication(rssi_v_dist, publication_plot_theme)
plot


distances <- seq(0, 600, 0.01)
source("R/functions/utils/rssi_v_dist.R")
model <- data.frame(
  dist = distances,
  rssi = predict_rssi(rssi_coefs, distances)
  # sim_rssi = simulate_rssi(rssi_coefs,distances)
)

a <- rssi_coefs[1]
b <- rssi_coefs[2]
ggplot() +
  geom_point(data = rssi_v_dist, mapping = aes(x = distance, y = rssi), alpha = 0.75) +
  geom_line(data = model, mapping = aes(x = dist, y = rssi), color = "red", linewidth = 2) +
  labs(x = "Distance (m)", y = "RSSI (dBm)") +
  ylim(a - 20, (a - b) + 20) +
  xlim(0, 600) +
  publication_plot_theme

## -----------------------------------------------------------------------------
##  Characterize Measurement Noise
## -----------------------------------------------------------------------------

ggplot() +
  geom_point(data = rssi_v_dist, aes(x = distance, y = rssi - pred)) +
  classic_plot_theme

rssi_v_dist$error <- NA

for(i in 1:nrow(rssi_v_dist)){
  rssi_v_dist$error[i] <- rssi_v_dist$rssi[i] - rssi_v_dist$pred[i]
}

ggplot() +
  geom_point(data = rssi_v_dist, mapping = aes(x = distance, y = abs(error))) +
  classic_plot_theme

ggplot() +
  geom_point(data = rssi_v_dist, mapping = aes(x = rssi, y = abs(error))) +
  classic_plot_theme

ggplot() +
  geom_histogram(data = rssi_v_dist, aes( x = error), color = "black", fill ="steelblue", alpha = 0.5, binwidth = 1) +
  labs( x = "Difference from Model (dBm)") +
  publication_plot_theme

mean(rssi_v_dist$error)
sd(rssi_v_dist$error)

# Display the plot
print(result$plot)

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
x_seq <- seq(x_range[1] - 2*sigma, x_range[2] + 2*sigma, length.out = 200)

# Calculate scaling factor to match histogram counts
# We need to scale the normal density to match the histogram bin counts
bin_count <- length(rssi_v_dist$error) * binwidth
fitted_curve <- dnorm(x_seq, mean = mu, sd = sigma) * bin_count

# Create the plot matching your style
p <- ggplot() +
  # Histogram (matching your exact style)
  geom_histogram(data = data.frame(error = rssi_v_dist$error), 
                 aes(x = error), 
                 color = "black", 
                 fill = "steelblue", 
                 alpha = 0.5, 
                 binwidth = binwidth) +
  # Fitted Gaussian curve
  geom_line(aes(x = x_seq, y = fitted_curve), 
            color = "red", size = 1.2) +
  # Labels matching your style
  labs(x = "Measured RSSI - Model RSSI (dBm)",
       y = "Counts / dBm",
      ) +
  # Your theme
  publication_plot_theme
p

## -----------------------------------------------------------------------------
##  Characterize Measurement Noise V2
## -----------------------------------------------------------------------------

# Create distance bins (0-10, 10-20, ..., 590-600)
bin_width <- 50
max_distance <- 600

# Method 1: Using cut() function
rssi_v_dist$distance_bin <- cut(rssi_v_dist$distance, 
                        breaks = seq(0, max_distance, by = bin_width),
                        right = FALSE,  # [0,10), [10,20), etc.
                        labels = paste0(seq(0, max_distance - bin_width, by = bin_width), 
                                       "-", seq(bin_width, max_distance, by = bin_width)))

# Calculate statistics for each bin
error_stats <- rssi_v_dist %>%
  group_by(distance_bin) %>%
  summarise(
    bin_center = mean(c(as.numeric(sub("-.*", "", distance_bin)), 
                       as.numeric(sub(".*-", "", distance_bin)))),
    n_samples = n(),
    avg_error = mean(error, na.rm = TRUE),
    std_error = sd(error, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(avg_error))  # Remove bins with no data

# Display the statistics table
print("Error Statistics by Distance Bin:")
print(error_stats)
print(error_stats$std_error)
# Create plots
# Plot 1: Average error vs distance
p1 <- ggplot(error_stats, aes(x = bin_center, y = avg_error)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Average RSSI Error vs Distance",
       x = "Distance (meters)",
       y = "Average Error (dB)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p1
# Plot 2: Standard deviation vs distance
p2 <- ggplot(error_stats, aes(x = bin_center, y = std_error)) +
  #geom_point(data = rssi_v_dist, mapping = aes(x = distance, y = abs(error))) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  ylim(0,10) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,by=2)) +
  labs(
       x = "Distance (meters)",
       y = "Measurement Noise (dBm)") +
  publication_plot_theme
p2
# Plot 3: Combined plot with error bars
p3 <- ggplot(error_stats, aes(x = bin_center)) +
  geom_errorbar(aes(ymin = avg_error - std_error, ymax = avg_error + std_error),
                width = 5, alpha = 0.7, color = "gray") +
  geom_line(aes(y = avg_error), color = "blue", size = 1) +
  geom_point(aes(y = avg_error), color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "RSSI Error Profile: Mean ± Standard Deviation",
       x = "Distance (meters)",
       y = "Error (dB)",
       subtitle = "Blue line: average error, Gray bars: ±1 standard deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p3
# Display plots
print(p1)
print(p2)
print(p3)

