convert_utm_to_latlon <- function(utm_df, utm_zone, hemisphere = "N", 
                                   x_col = "x", y_col = "y") {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for UTM to lat/lon conversion. Please install it with: install.packages('sf')")
  }
  
  if (missing(utm_zone)) {
    stop("UTM zone must be specified (e.g., 10, 33, etc.)")
  }
  
  if (!(hemisphere %in% c("N", "S"))) {
    stop("Hemisphere must be either 'N' (North) or 'S' (South)")
  }
  
  if (!(x_col %in% names(utm_df))) {
    stop(paste("Column", x_col, "not found in data frame"))
  }
  
  if (!(y_col %in% names(utm_df))) {
    stop(paste("Column", y_col, "not found in data frame"))
  }
  
  utm_crs <- paste0("+proj=utm +zone=", utm_zone, 
                    " +datum=WGS84 +units=m +no_defs",
                    ifelse(hemisphere == "S", " +south", ""))
  
  utm_sf <- sf::st_as_sf(utm_df, coords = c(x_col, y_col), crs = utm_crs)
  
  latlon_sf <- sf::st_transform(utm_sf, crs = 4326)
  
  coords <- sf::st_coordinates(latlon_sf)
  
  result_df <- utm_df
  result_df$longitude <- coords[, "X"]
  result_df$latitude <- coords[, "Y"]
  
  result_df[[x_col]] <- NULL
  result_df[[y_col]] <- NULL
  
  return(result_df)
}