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

## -----------------------------------------------------------------------------
##  SPECIFY PARAMETERS HERE
## -----------------------------------------------------------------------------
# ALL THE TAGS!!!
classic_blu_bat <- "870614B1"
castle_blu_bat <- "23D9727E"
caslte_blu_bat_black <- "D172633A"
power_tag <-"1E336107"

# Specify the tag ID that you used in your calibration
my_tag_id <- power_tag

# Specify the RSSI vs Distance fit coefficients from calibration
a <- -62.23199786905
b <- -26.94434562050 
c <- 0.02064257308
rssi_coefs <- c(a, b, c)

# Specify time range of detection data you want to pull from the DB
det_start_time <- as.POSIXct("2025-05-27 19:00:00", tz = "GMT")
det_stop_time <- as.POSIXct("2025-05-27 21:00:00", tz = "GMT")

# Specify a list of node Ids if you only want to include a subset in calibration
# IF you want to use all nodes, ignore this line and SKIP the step below
# where the data frame is trimmed to only nodes in this list
my_nodes <- c("B995250B", "4D64B5B5", "7378B941", "9CC5DC79", "D46BA6A2", "7D769A01", "AA4358FB", "CB4A1EAE", "520A3C5C")

# You can specify an alternative map tile URL to use here
my_tile_url <- "https://mt2.google.com/vt/lyrs=y&x={x}&y={y}&z={z}"
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##  2.) GET NODE LOCATIONS
## -----------------------------------------------------------------------------
node_locs <- read.csv("data/ctt_office_v2/node_locs_adjusted.csv")

# Draw a map with the node locations
node_map <- map_node_locations(node_locs, tile_url = my_tile_url)
node_map

## -----------------------------------------------------------------------------
##  3.) LOAD STATION DETECTION DATA
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
detection_df <- detection_df %>% mutate(time_value = as.integer(time))

source("examples/ctt_office_v2/load_node_files.R")
node_det_df <- load_node_data_434("data/ctt_office_v2/node_files/")
node_det_df <- node_det_df %>%
  rename(tag_rssi = rssi)
node_det_df <- node_det_df %>%
  filter(tag_id == my_tag_id)
node_det_df <- node_det_df %>% mutate(time_value = as.integer(time))

## -----------------------------------------------------------------------------
##  4.) BUILD A GRID
## -----------------------------------------------------------------------------
grid_center_lat <- 39.0011804
grid_center_lon <- -74.9145980
grid_size_x <- 60 # meters
grid_size_y <- 60 # meters
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
##  5.) (OPTIONAL) CALCULATE TEST SOLUTION
## -----------------------------------------------------------------------------
test_time <- as.POSIXct("2023-08-03 19:55:50", tz = "GMT")
test_rec_df <- calc_receiver_values(
  current_time = test_time,
  det_window = 60,
  station_tag_df = detection_df,
  node_locs = node_locs,
  node_t_offset = node_toff_df,
  rssi_coefs = rssi_coefs,
  filter_alpha = 0.7,
  filter_time_range = 120
)
print(test_rec_df)
# Find the GridSearch Solution
test_grid_values <- calc_grid_values(grid_df, test_rec_df, rssi_coefs)
solution <- subset(test_grid_values, test_grid_values$value == max(test_grid_values$value))
print(solution)
# Multilateration calculation
reduced_rec_df <- subset.data.frame(test_rec_df, test_rec_df$filtered_rssi >= a)
node_w_max <- reduced_rec_df[reduced_rec_df$filtered_rssi == max(reduced_rec_df$filtered_rssi),]
multilat_fit <- nls(reduced_rec_df$exp_dist ~ haversine(reduced_rec_df$lat,reduced_rec_df$lon,ml_lat,ml_lon),
                      reduced_rec_df,
                      start= list(ml_lat = node_w_max$lat, ml_lon = node_w_max$lon),
                      control=nls.control(warnOnly = T, minFactor=1/65536, maxiter = 100)
                    )
print(multilat_fit)
co <- coef(summary(multilat_fit))
print(paste(co[1,1],co[2,1]))
multilat_result <- c(co[1,1],co[2,1])
source("publication/draw_single_solution_publication.R")
test_map <- draw_single_solution_publication(test_rec_df, test_grid_values, solution, multilat_result, pub_tile_url)
test_map

## -----------------------------------------------------------------------------
##  6.) CALCULATE TRACK
## -----------------------------------------------------------------------------
# track_frame_output_path <- "output/track_frames/"
track_df <- calculate_track(
  start_time = "2025-05-27 19:41:41",
  length_seconds = 80,
  step_size_seconds = 5,
  det_time_window = 15, # Must have detection within this window to be included in position calculation
  filter_alpha = 0.5,
  filter_time_range = 60, # Time range to include detections in filtered value
  grid_df = grid_df,
  detection_df = node_det_df,
  node_locs = node_locs,
  rssi_coefs = rssi_coefs,
  track_frame_output_path = NULL # If NULL no individual frames will be saved
)
print(track_df)
track_map <- map_track(node_locs, track_df, my_tile_url)
track_map

## -----------------------------------------------------------------------------
##  7.) (OPTIONAL) COMPARE WITH KNOWN TRACK?
## -----------------------------------------------------------------------------
# If you've recorded a test track with the sidekick and want to see how well you
# are able to recreate it you can use the commands below.
sidekick_file_path <- "data/ctt_office_v2/sidekick/long_slow_cal.csv"
# Get Sidekick data from CSV (note I modified the headers for cleaner names in here)
sidekick_df <- read.csv(sidekick_file_path)
# Correct sidekick time formatting
sidekick_df <- sidekick_df %>% mutate(time_utc = substring(c(sidekick_df$time_utc), 1, 19))
# Add numerical time value column
sidekick_df <- sidekick_df %>% mutate(time_value = get_time_value(sidekick_df$time_utc))
# Trim Sidekick data to the time of the calculated track
sidekick_df <- sidekick_df %>%
  filter(time_value >= min(track_df$time) & time_value <= max(track_df$time))

track_error_df <- calc_track_error(sidekick_df, track_df)
print(track_error_df)
print(min(track_error_df$error))
print(max(track_error_df$error))
print(paste("GS Solution Error = ", mean(track_error_df$error), " +/- ", sd(track_error_df$error)))
print(paste("ML Solution Error = ", mean(track_error_df$ml_error), " +/- ", sd(track_error_df$ml_error)))

print(min(track_error_df$ml_error))
print(max(track_error_df$ml_error))

compare_map <- map_track_error(node_locs, track_error_df, sidekick_df, my_tile_url)
compare_map

source("R/functions/node/map_node_locations_publication.R")
pub_tile_url <- "https://mt2.google.com/vt/lyrs=s&x={x}&y={y}&z={z}"
node_map_pub <- map_node_locations_publication(node_locs, track_error_df, sidekick_df,tile_url = pub_tile_url)
node_map_pub

# Uncertainty analysis
ggplot() +
  geom_point(data = track_error_df, aes(x = i, y = ml_error), color = "orange") +
  geom_point(data = track_error_df, aes(x = i, y = error), color = "red") +
  xlab("Track Point #") +
  ylab("Solution Error (m)") +
  publication_plot_theme

ggplot() +
  #geom_point(data = track_error_df, aes(x = max_rssi, y = ml_error), color = "orange") +
  geom_point(data = track_error_df, aes(x = max_rssi, y = error)) +
  xlab("Max RSSI (dBm)") +
  ylab("Position Error (m)") +
  classic_plot_theme


track_ml_error = data.frame(error = track_error_df$ml_error)



hist <- ggplot(track_error_df, aes(x=error)) +
  geom_histogram(data = track_error_df, fill = "red", alpha = 0.5, aes(y = ..density..), color = "black", binwidth = 5, boundary = 0) +
  geom_histogram(data = track_ml_error, fill = "orange", alpha = 0.5, aes(y = ..density..), color = "black", binwidth = 5, boundary = 0) +
  geom_density(data = track_error_df,alpha = 0.5, fill = "red",color = "black", show.legend = TRUE) +
  geom_density(data = track_ml_error,alpha = 0.5, fill = "orange", color = "black", show.legend = TRUE) +
  geom_vline(aes(xintercept=mean(track_error_df$error)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(track_error_df$ml_error)), color="orange", linetype="dashed", size=1) +
  xlab("Solution Error (m)") +
  ylab("Solution Density") +
  publication_plot_theme +
#  scale_fill_manual(name="error",values=c("red","orange"),labels=c("Grid Search","Multilateration"))
  scale_colour_manual("error",values = c("red","orange"))
hist

plot_df = data.frame(error = track_error_df$error, time = track_error_df$i*10, Method = "Grid Search")
plot_df_2 = data.frame(error = track_error_df$ml_error, time = track_error_df$i*10, Method = "Multilateration")
plot_df <- rbind(plot_df,plot_df_2)

source("R/defs/plot_themes.R")
hist2 <- ggplot(plot_df, aes(x=error, fill = Method)) +
  geom_histogram(alpha = 0.5, color = "black", binwidth = 10,boundary = 0, position = "identity") +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Grid Search")$error)),linetype="dashed", size=1, color = "#F8766D" ) +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Multilateration")$error)),linetype="dashed", size=1, color = "#00BFC4") +
  xlab("Solution Error (m)") +
  ylab("Counts / 10 Meters") +
  xlim(0,160) +
  publication_plot_theme  
hist2

hist2 <- ggplot(plot_df, aes(x=error, fill = Method)) +
  #geom_histogram(alpha = 0.5, color = "black", binwidth = 5,boundary = 0, position = "identity") +
  geom_density(data = subset(plot_df,Method == "Grid Search"), alpha = 0.5) +
  geom_density(data = subset(plot_df,Method == "Multilateration"), alpha = 0.5) +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Grid Search")$error)),linetype="dashed", size=1, color = "#F8766D" ) +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Multilateration")$error)),linetype="dashed", size=1, color = "#00BFC4") +
  xlab("Solution Error (m)") +
  ylab("Solution Density") +
  xlim(0,160) +
  publication_plot_theme  
hist2

hist2 <- ggplot(plot_df, aes(x=error, fill = Method)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), color = "black", binwidth = 10,boundary = 0, position = "identity") +
  geom_density(data = subset(plot_df,Method == "Grid Search"), alpha = 0.5) +
  geom_density(data = subset(plot_df,Method == "Multilateration"), alpha = 0.5) +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Grid Search")$error)),linetype="dashed", size=1, color = "#F8766D" ) +
  geom_vline(aes(xintercept=mean(subset(plot_df,Method == "Multilateration")$error)),linetype="dashed", size=1, color = "#00BFC4") +
  xlab("Solution Error (m)") +
  ylab("Solution Density") +
  xlim(0,160) +
  publication_plot_theme  
hist2

# Error vs time
error_v_time <- ggplot(plot_df, aes(x = time, y = error, color = Method)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept=mean(subset(plot_df,Method == "Grid Search")$error)),linetype="dashed", size=1, color = "#F8766D", alpha = 0.5 ) +
  geom_hline(aes(yintercept=mean(subset(plot_df,Method == "Multilateration")$error)),linetype="dashed", size=1, color = "#00BFC4", alpha = 0.5 ) +
  xlab("Time (s)") +
  ylab("Solution Error (m)") +
  publication_plot_theme
error_v_time

## Plotting simulation results here while it runs in the other sessions

####. RUNTIME
data <- read.csv("~/development/R_analysis/localization_sim/outputs/all.csv")
data$n_cell <- (data$inputs.grid_max_x / data$inputs.grid_cell_size)^2
data$run_scale <- data$n_points * (25 * (data$inputs.filter_time_range / data$inputs.beep_int_sec) + data$n_cell)
ggplot(data) +
  geom_point(mapping = aes(x =run_scale, y = run_time , color = inputs.beep_int_sec)) +
#  xlim(0,2e8) +
#  ylim(0,1000) +
  classic_plot_theme

####. SPACING
data <- read.csv("~/development/R_analysis/localization_sim/outputs/node_spacing_results_beep10.csv")
ggplot(data, aes(x = avg_node_spacing)) +
  geom_point(aes(y = gs_mean_error, color = "Grid Search"), size = 4) +
  geom_line(aes(y = gs_mean_error, color = "Grid Search"), size = 1) +
  geom_errorbar(aes(ymin = gs_mean_error - gs_sd_error, 
                    ymax = gs_mean_error + gs_sd_error, 
                    color = "Grid Search"), width = 2) +
  geom_point(aes(y = ml_mean_error, color = "Multilateration"), size = 4) +
  geom_line(aes(y = ml_mean_error, color = "Multilateration"), size = 1) +
  geom_errorbar(aes(ymin = ml_mean_error - ml_sd_error, 
                    ymax = ml_mean_error + ml_sd_error, 
                    color = "Multilateration"), width = 2) +
  scale_color_manual(values = c("Grid Search" = "#F8766D", "Multilateration" = "#00BFC4"),
                     name = "Method") +
  labs(x = "Receiver Spacing (meters)", 
       y = "Mean Error (meters)", 
       ) +
  scale_x_continuous(breaks = seq(0,200,by = 20)) +
  publication_plot_theme +
  theme(
    legend.position = c(.15, .85),
  )

ggplot(data, aes(x = avg_node_spacing)) +
  geom_point(aes(y = gs_mean_error, color = "Grid Search"), size = 4) +
  geom_line(aes(y = gs_mean_error, color = "Grid Search"), size = 1) +
  geom_errorbar(aes(ymin = gs_mean_error - gs_sd_error, 
                    ymax = gs_mean_error + gs_sd_error, 
                    color = "Grid Search"), width = 2) +
  labs(x = "Receiver Spacing (meters)", 
       y = "Mean Error (meters)", 
  ) +
  scale_x_continuous(breaks = seq(0,200,by = 20)) +
  publication_plot_theme +
  theme(
    legend.position = c(-.15, -.85),
  )

ggplot(data, aes(x = avg_node_spacing)) +
  geom_point(aes(y = gs_mean_error / avg_node_spacing, color = "Grid Search"), size = 4) +
  geom_line(aes(y = gs_mean_error / avg_node_spacing, color = "Grid Search"), size = 1) +
  geom_errorbar(aes(ymin = (gs_mean_error - gs_sd_error) / avg_node_spacing, 
                    ymax = (gs_mean_error + gs_sd_error) / avg_node_spacing, 
                    color = "Grid Search"), width = 2) +
  geom_point(aes(y = ml_mean_error / avg_node_spacing, color = "Multilateration"), size = 4) +
  geom_line(aes(y = ml_mean_error / avg_node_spacing, color = "Multilateration"), size = 1) +
  geom_errorbar(aes(ymin = (ml_mean_error - ml_sd_error) / avg_node_spacing, 
                    ymax = (ml_mean_error + ml_sd_error) / avg_node_spacing, 
                    color = "Multilateration"), width = 2) +
  scale_color_manual(values = c("Grid Search" = "#F8766D", "Multilateration" = "#00BFC4"),
                     name = "Method") +
  labs(x = "Receiver Spacing (meters)", 
       y = "Mean Error / Receiver Spacing", 
       title = "Mean Error vs Receiver Spacing") +
  scale_x_continuous(breaks = seq(0,200,by = 20)) +
  publication_plot_theme +
  theme(
    legend.position = c(.15, .85),
  )

ggplot(data, aes(x = avg_node_spacing)) +
  geom_point(aes(y = ml_mean_error / gs_mean_error), size = 4) +
  labs(x = "Node Spacing (meters)", 
       y = "Multilateration Error / Grid Search Error", 
       title = "Mean Error vs Node Spacing") +
  scale_x_continuous(breaks = seq(0,200,by = 20)) +
  publication_plot_theme +
  theme(
    legend.position = c(.15, .85),
  )
