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
node_loc_plot <- plot_node_locations(node_health_df, theme = classic_plot_theme)
node_loc_plot
# Write the node locations to a file
export_node_locations("examples/meadows/output/node_locations.csv", node_locs)

# Draw a map with the node locations
source("R/functions/node/map_node_locations_publication.R")
pub_tile_url <- "https://mt2.google.com/vt/lyrs=s&x={x}&y={y}&z={z}"
node_map_pub <- map_node_locations_publication(node_locs, tile_url = pub_tile_url)
node_map_pub
