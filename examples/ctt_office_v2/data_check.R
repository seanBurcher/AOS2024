library(dplyr)

source("R/defs/plot_themes.R")
############################################
# Comapre NODE SD data to Station Data
############################################

my_tag_id <- "1E336107"
my_node_id <- "D46BA6A2"
start_time <- as.POSIXct("2025-05-27 19:00:00", tz = "GMT")
stop_time <- as.POSIXct("2025-05-27 21:00:00", tz = "GMT")

single_node_data <- read.csv("data/ctt_office_v2/node_files/D46BA6A2_434.csv")
single_node_data$time <- as.POSIXct(single_node_data$time, format = "%Y-%m-%dT%H:%M:%SZ",tz = "UTC")
single_node_data <- single_node_data %>% filter(tag_id == my_tag_id)
single_node_data <- single_node_data %>% filter(time >= start_time & time <= stop_time)

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

detection_df <- detection_df %>% filter(node_id == my_node_id)
detection_df <- detection_df %>% filter(time >= start_time & time <= stop_time)

ggplot() +
  geom_point(data = single_node_data, mapping = aes(x = time, y = rssi), color = "black", alpha = 0.5) +
  geom_line(data = single_node_data, mapping = aes(x = time, y = rssi), color = "black", alpha = 0.5) +
  geom_point(data = detection_df, mapping = aes(x = time, y = tag_rssi), color = "red") +
  geom_line(data = detection_df, mapping = aes(x = time, y = tag_rssi), color = "red") +
  classic_plot_theme


source("examples/ctt_office_v2/load_node_files.R")
node_dets <- load_node_data_434("data/ctt_office_v2/node_files/")

