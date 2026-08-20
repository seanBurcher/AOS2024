library(sf)
source("R/functions/utils/convert_utm_to_latlon.R")

sample_data <- data.frame(
  station_id = c("A", "B", "C"),
  x = c(500000, 501000, 502000),
  y = c(4500000, 4501000, 4502000),
  elevation = c(100, 150, 200)
)

print("Original UTM data:")
print(sample_data)

converted_data <- convert_utm_to_latlon(
  utm_df = sample_data,
  utm_zone = 10,
  hemisphere = "N",
  x_col = "x",
  y_col = "y"
)

print("Converted to lat/lon:")
print(converted_data)

meadows_data <- read.csv("meadows_node_locations.csv")
if (file.exists("meadows_node_locations.csv")) {
  print("Converting meadows node locations...")
  
  converted_meadows <- convert_utm_to_latlon(
    utm_df = meadows_data,
    utm_zone = 10,  
    hemisphere = "N"
  )
  
  print(head(converted_meadows))
}