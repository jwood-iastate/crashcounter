#' Merge and Aggregate Crash Data by Road Segments using GPS Data - Spatial Roadway Files
#'
#' This function merges a road segments dataset with a crash dataset based on
#' GPS locations and distances. It aggregates crash data per road segment,
#' assigning each crash to the nearest segment within a specified buffer
#' distance. Optional conditions can filter crashes based on various attributes.
#' Additionally, crashes are assigned to segments only if the crash's route ID
#' matches the segment's route ID, if such information is provided.
#'
#' @param road_segments A data frame or `sf` object containing road segment data.
#'   Expected fields include segment identifiers specified by `road_id_vars`, as
#'   well as the geometry of the segments. If not already an `sf` object,
#'   the function will attempt to create one using the provided geometry column.
#' @param crashes A data frame containing crash data. Expected fields include
#'   crash locations specified by latitude and longitude.
#' @param crash_lat The name of the variable for the crash latitude.
#' @param crash_long The name of the variable for the crash longitude.
#' @param road_id_vars Optional. A character vector specifying the column names
#'   that uniquely identify each road segment. If `NULL` (default), a new
#'   identifier will be created.
#' @param road_geom Optional. A string specifying the name of the geometry column
#'   in `road_segments`. If this is used, it should be of class `sfc_LINESTRING`
#'   or `sfc_MULTILINESTRING`.
#' @param crash_coords Optional. A string specifying the name of the variable
#'   with the GPS coordinates for the crashes. If this is used, it will override
#'   `crash_lat` and `crash_long`. The variable should be of class `sfc_POINT`.
#' @param conditions Optional. A condition or set of conditions for filtering
#'   the crashes data frame before aggregation. This can be specified as a
#'   character string (e.g., `"Severity > 2"`), an expression (e.g.,
#'   `expression(Severity > 2)`), or a list of expressions (e.g.,
#'   `list(expression(Severity > 2), expression(Time < 6 | Time > 18))`).
#'   If `NULL` (default), no additional conditions are applied.
#' @param countvarname Optional. A string specifying the name of the new variable
#'   with the crash counts. The default is `"Total_Crashes"` but should be
#'   specified by the user if any conditions are provided.
#' @param dist_feet Optional. A numeric value specifying the distance threshold
#'   (in feet). Either `dist_feet` or `dist_meters` should be used. The default
#'   is `dist_feet = 50`.
#' @param dist_meters Optional. A numeric value specifying the distance
#'   threshold (in meters). If this is used, it overrides `dist_feet`.
#' @param crashRoute Optional. The name of the route ID variable in the crash
#'   data frame to match the road segments on. If this is provided along with
#'   `roadRoute`, crashes will be assigned only to road segments where
#'   the crash's route ID matches the segment's route ID.
#' @param roadRoute Optional. The name of the route ID variable in the
#'   `road_segments` data frame. This column should contain the route IDs
#'   associated with each road segment.
#' @param projection_input Optional. A character string or numeric value specifying
#'   the CRS (Coordinate Reference System) of the input data. Defaults to 4326
#'   (WGS84 latitude/longitude). If the data frames are spatial objects, this will be extracted.
#' @param projection_working Optional. A character string or numeric value specifying
#'   the CRS to which data will be transformed for accurate distance calculations.
#'   Defaults to `NULL` which skips the transformation.
#'
#' @return A data frame with the original road segment data and a new variable that
#'   contains the crash counts for the specified conditions (if any).
#'
#' @import dplyr
#' @import sf
#' @importFrom tidyr replace_na
#'
#' @examples
#'
#' # Sample road segments data with route IDs
#' library(sf)
#' road_segments_df <- data.frame(
#'   segment_id = 1:3,
#'   route_id = c("Route66", "I-5", "I-10")
#' )
#' # Create simple lines for the example
#' road_segments_df$geometry <- st_sfc(
#'   st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
#'   st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
#'   st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
#' )
#' road_segments_sf <- st_sf(road_segments_df, crs = 4326)
#'
#' # Sample crash data with route IDs
#' crashes_df <- data.frame(
#'   crash_id = 1:5,
#'   crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
#'   crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
#'   Severity = c(1, 3, 2, 5, 4),
#'   Time = c(2, 15, 20, 5, 23),
#'   route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
#' )
#'
#' # Run the function
#' result <- crashCounts.seg.gdb(
#'   road_segments = road_segments_sf,
#'   crashes = crashes_df,
#'   crash_lat = "crash_latitude",
#'   crash_long = "crash_longitude",
#'   road_id_vars = c("segment_id"),
#'   crashRoute = "route_id",
#'   roadRoute = "route_id",
#'   dist_feet = 75,
#'   projection_input = 4326,
#'   projection_working = 3857
#' )
#'
#' # View the result
#' print(result)
#'
#' @export
crashCounts.seg.gdb <- function(
    road_segments,
    crashes,
    crash_lat,
    crash_long,
    road_id_vars = NULL,
    road_geom = NULL,
    crash_coords = NULL,
    conditions = NULL,
    countvarname = "Total_Crashes",
    dist_feet = 50,
    dist_meters = NULL,
    crashRoute = NULL,
    roadRoute = NULL,
    projection_input = 4326,
    projection_working = NULL
) {

  library(dplyr)
  library(sf)
  library(tidyr)

  # If no identifiers are given for the road segments, create one
  if (is.null(road_id_vars)) {
    road_segments <- road_segments %>%
      mutate(road_id = row_number())
    road_id_vars <- "road_id"
  }

  # Ensure crashes have a unique ID
  if (!"crash_id" %in% names(crashes)) {
    crashes$crash_id <- 1:nrow(crashes)
  }

  # Create spatial objects if needed
  if (is.null(road_geom)) {
    if (!inherits(road_segments, "sf")) {
      stop("road_segments must be an sf object or road_geom must be provided.")
    }
  } else {
    if (!inherits(road_segments[[road_geom]], c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
      stop("road_geom must be a column of class sfc_LINESTRING or sfc_MULTILINESTRING")
    }
    road_segments <- st_sf(road_segments, geometry = road_segments[[road_geom]], crs = projection_input)
  }

  if (is.null(crash_coords)) {
    crashes <- sf::st_as_sf(crashes, coords = c(crash_long, crash_lat), crs = projection_input)
  } else {
    if (!inherits(crashes[[crash_coords]], "sfc_POINT")) {
      stop("crash_coords must be a column of class sfc_POINT")
    }
    crashes <- st_sf(crashes, geometry = crashes[[crash_coords]], crs = projection_input)
  }

  # Transform to working projection for distance calculations
  if (!is.null(projection_working)) {
    road_segments <- st_transform(road_segments, crs = projection_working)
    crashes <- st_transform(crashes, crs = projection_working)
  }

  # Apply user-specified conditions if provided
  if (!is.null(conditions)) {
    if (is.character(conditions)) {
      crashes <- crashes %>%
        filter(eval(parse(text = conditions)))
    } else if (is.expression(conditions)) {
      crashes <- crashes %>%
        filter(eval(conditions))
    } else if (is.list(conditions)) {
      for (cond in conditions) {
        crashes <- crashes %>%
          filter(eval(cond))
      }
    }
  }

  # Filter road segments and crashes by route IDs if provided
  if (!is.null(crashRoute) && !is.null(roadRoute)) {
    road_routes <- unique(road_segments[[roadRoute]])
    crashes <- crashes %>%
      filter(.data[[crashRoute]] %in% road_routes)
  }

  # Compute buffer distance
  dist <- ifelse(is.null(dist_meters), dist_feet * 0.3048, dist_meters) # Convert feet to meters if dist_meters is null

  # Compute which crashes are within distance of which road segments
  within_dist_list <- st_is_within_distance(crashes, road_segments, dist = dist)

  # Remove crashes that are not within any segment buffer
  valid_indices <- lengths(within_dist_list) > 0
  crashes <- crashes[valid_indices, ]
  within_dist_list <- within_dist_list[valid_indices]

  # Build data frame of matches
  matches_df <- data.frame(
    crash_id = rep(crashes$crash_id, times = lengths(within_dist_list)),
    road_row = unlist(within_dist_list)
  )

  # Add 'road_id_vars' columns from 'road_segments'
  matches_df <- cbind(matches_df, road_segments[matches_df$road_row, road_id_vars, drop = FALSE])

  # Add 'roadRoute' from 'road_segments'
  if (!is.null(roadRoute)) {
    matches_df$roadRoute <- road_segments[[roadRoute]][matches_df$road_row]
  }

  # Add 'crashRoute' from 'crashes'
  if (!is.null(crashRoute)) {
    matches_df$crashRoute <- crashes[[crashRoute]][match(matches_df$crash_id, crashes$crash_id)]
  }

  # Filter matches where crashRoute matches roadRoute
  if (!is.null(crashRoute) && !is.null(roadRoute)) {
    matches_df <- matches_df %>%
      filter(crashRoute == roadRoute)
  }

  # Check if after filtering any matches are left
  if (nrow(matches_df) == 0) {
    # No crashes matched to road segments based on route IDs
    # Set crash counts to zero and return road_segments
    road_segments <- road_segments %>%
      mutate(!!countvarname := 0)
    return(road_segments)
  }

  # Extract geometries
  crash_geoms <- st_geometry(crashes)
  road_geoms <- st_geometry(road_segments)

  # Add geometries to matches_df
  matches_df$crash_geom <- crash_geoms[match(matches_df$crash_id, crashes$crash_id)]
  matches_df$road_geom <- road_geoms[matches_df$road_row]

  # Compute distances
  matches_df$distance <- st_distance(matches_df$crash_geom, matches_df$road_geom, by_element = TRUE)

  # For each crash, select the road segment with minimum distance
  nearest_matches <- matches_df %>%
    group_by(crash_id) %>%
    slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Now, count crashes per road segment
  crash_counts <- nearest_matches %>%
    group_by(across(all_of(road_id_vars))) %>%
    summarise(NumCrashes = n(), .groups = 'drop')

  # Merge crash_counts back to road_segments data
  road_segments <- road_segments %>%
    left_join(crash_counts, by = road_id_vars)

  # Rename the crash count variable
  road_segments <- road_segments %>%
    mutate(!!countvarname := replace_na(NumCrashes, 0)) %>%
    select(-NumCrashes)

  return(road_segments)
}
