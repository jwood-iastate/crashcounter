#' Merge and Aggregate Crash Data by Intersections using GPS Data
#'
#' This function merges an intersection dataset with a crash dataset based on
#' GPS locations and distances. It aggregates crash data per intersection,
#' assigning each crash to the nearest intersection within a specified buffer
#' distance. Optional conditions can filter crashes based on various attributes.
#' Additionally, crashes are assigned to intersections only if the crash's route ID
#' matches one of the intersection's route IDs, if such information is provided.
#'
#' @param intersections A data frame containing intersection data. Expected
#'   fields include intersection identifiers specified by `int_id_vars`, as
#'   well as the latitude and longitude coordinates of the intersections.
#' @param crashes A data frame containing crash data. Expected fields include
#'   crash locations specified by latitude and longitude.
#' @param int_lat The name of the variable for the intersection latitude.
#' @param int_long The name of the variable for the intersection longitude.
#' @param crash_lat The name of the variable for the crash latitude.
#' @param crash_long The name of the variable for the crash longitude.
#' @param int_id_vars Optional. A character vector specifying the column names
#'   that uniquely identify each intersection. If `NULL` (default), a new
#'   identifier will be created.
#' @param int_coords Optional. A string specifying the name of the variable with
#'   the GPS coordinates (latitude and longitude) of the intersections. If this
#'   is used, it will override `int_lat` and `int_long`. The variable should be
#'   of class `sfc_POINT`.
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
#'   is `dist_feet = 250`.
#' @param dist_meters Optional. A numeric value specifying the distance
#'   threshold (in meters). If this is used, it overrides `dist_feet`.
#' @param crashRoute Optional. The name of the route ID variable in the crash
#'   data frame to match the intersections on. If this is provided along with
#'   `intersectionRoutes`, crashes will be assigned only to intersections where
#'   the crash's route ID matches one of the intersection's route IDs.
#' @param intersectionRoutes Optional. The name of the route IDs variable in the
#'   intersections data frame. This column should contain the route IDs
#'   associated with each intersection. It can be a single route ID or a list
#'   of route IDs (e.g., a character vector or a list column).
#' @param projection_input Optional. A character string or numeric value specifying
#'   the CRS (Coordinate Reference System) of the input data. Defaults to 4326
#'   (WGS84 latitude/longitude). If the data frames are spatial objects, this will be extracted.
#' @param projection_working Optional. A character string or numeric value specifying
#'   the CRS to which data will be transformed for accurate distance calculations.
#'   Defaults to `NULL` which skips the transformation.
#'
#' @return A data frame with the original intersection data and a new variable that
#'   contains the crash counts for the specified conditions (if any).
#'
#' @import dplyr
#' @import sf
#' @importFrom tidyr replace_na
#'
#' @examples
#'
#' # Sample intersection data with route IDs
#' intersections_df <- data.frame(
#'   intersection_id = 1:3,
#'   latitude = c(34.0522, 34.0525, 34.0528),
#'   longitude = c(-118.2437, -118.2440, -118.2443),
#'   routes = c("Route66,I-5", "I-5,I-10", "Route66,I-10")
#' )
#'
#' # Sample crash data with route IDs
#' crashes_df <- data.frame(
#'   crash_id = 1:5,
#'   crash_latitude = c(34.0520, 34.0526, 34.0527, 34.0510, 34.0530),
#'   crash_longitude = c(-118.2430, -118.2441, -118.2442, -118.2445, -118.2446),
#'   Severity = c(1, 3, 2, 5, 4),
#'   Time = c(2, 15, 20, 5, 23),
#'   route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
#' )
#'
#' # Run the function
#' result <- crashCounts.int(
#'   intersections = intersections_df,
#'   crashes = crashes_df,
#'   int_lat = "latitude",
#'   int_long = "longitude",
#'   crash_lat = "crash_latitude",
#'   crash_long = "crash_longitude",
#'   int_id_vars = c("intersection_id"),
#'   crashRoute = "route_id",
#'   intersectionRoutes = "routes",
#'   dist_feet = 500,
#'   projection_input = 4326,
#'   projection_working = 3857
#' )
#'
#' # View the result
#' print(result)
#'
#' # Using provided datasets
#' data(crashes)
#' data(intersections)
#'
#' # Run the function
#' result <- crashCounts.int(
#'             intersections = intersections,
#'             crashes = crashes,
#'             dist_feet = 300
#'  )
#'
#'  # View the result
#'  print(result)
#'
#' @export
crashCounts.int <- function(
    intersections,
    crashes,
    int_lat,
    int_long,
    crash_lat,
    crash_long,
    int_id_vars = NULL,
    int_coords = NULL,
    crash_coords = NULL,
    conditions = NULL,
    countvarname = "Total_Crashes",
    dist_feet = 250,
    dist_meters = NULL,
    crashRoute = NULL,
    intersectionRoutes = NULL,
    projection_input = 4326,
    projection_working = NULL
) {

  # If no identifiers are given for the intersections, create one
  if (is.null(int_id_vars)) {
    intersections <- intersections %>%
      mutate(int_id = row_number())
    int_id_vars <- "int_id"
  }

  # Ensure crashes have a unique ID
  if (!"crash_id" %in% names(crashes)) {
    crashes$crash_id <- 1:nrow(crashes)
  }

  # Create spatial objects if needed
  if (is.null(int_coords)) {
    intersections <- sf::st_as_sf(intersections, coords = c(int_long, int_lat), crs = projection_input)
  } else {
    if (!inherits(intersections[[int_coords]], "sfc_POINT")) {
      stop("int_coords must be a column of class sfc_POINT")
    }
    intersections <- st_sf(intersections, geometry = intersections[[int_coords]], crs = projection_input)
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
    intersections <- st_transform(intersections, crs = projection_working)
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

  # Compute buffer distance
  dist <- ifelse(is.null(dist_meters), dist_feet * 0.3048, dist_meters) # Convert feet to meters if dist_meters is null

  # Compute which crashes are within distance of which intersections
  within_dist_list <- st_is_within_distance(crashes, intersections, dist = dist)

  # Remove crashes that are not within any intersection buffer
  valid_indices <- lengths(within_dist_list) > 0
  crashes <- crashes[valid_indices, ]
  within_dist_list <- within_dist_list[valid_indices]

  # Build data frame of matches
  matches_df <- data.frame(
    crash_id = rep(crashes$crash_id, times = lengths(within_dist_list)),
    int_row = unlist(within_dist_list)
  )

  # Add 'int_id_vars' columns from 'intersections'
  matches_df <- cbind(matches_df, intersections[matches_df$int_row, int_id_vars, drop = FALSE])

  # Process intersectionRoutes to ensure it's a list of route IDs
  if (!is.null(intersectionRoutes)) {
    if (!is.list(intersections[[intersectionRoutes]])) {
      # Assume it's a character column, possibly with comma-separated route IDs
      intersections[[intersectionRoutes]] <- strsplit(as.character(intersections[[intersectionRoutes]]), ",")
    }
    # Add 'intersectionRoutes' from 'intersections'
    matches_df$intersectionRoutes <- intersections[[intersectionRoutes]][matches_df$int_row]
  }

  # Add 'crashRoute' from 'crashes'
  if (!is.null(crashRoute)) {
    matches_df$crashRoute <- crashes[[crashRoute]][match(matches_df$crash_id, crashes$crash_id)]
  }

  # Filter matches where crashRoute is among intersectionRoutes
  if (!is.null(crashRoute) && !is.null(intersectionRoutes)) {
    matches_df <- matches_df %>%
      filter(mapply(function(crash_route, intersection_routes) {
        crash_route %in% intersection_routes
      }, crashRoute, intersectionRoutes))
  }

  # Check if after filtering any matches are left
  if (nrow(matches_df) == 0) {
    # No crashes matched to intersections based on route IDs
    # Set crash counts to zero and return intersections
    intersections <- intersections %>%
      mutate(!!countvarname := 0)
    return(intersections)
  }

  # Extract geometries
  crash_geoms <- st_geometry(crashes)
  int_geoms <- st_geometry(intersections)

  # Add geometries to matches_df
  matches_df$crash_geom <- crash_geoms[match(matches_df$crash_id, crashes$crash_id)]
  matches_df$int_geom <- int_geoms[matches_df$int_row]

  # Compute distances
  matches_df$distance <- st_distance(matches_df$crash_geom, matches_df$int_geom, by_element = TRUE)

  # For each crash, select the intersection with minimum distance
  nearest_matches <- matches_df %>%
    group_by(crash_id) %>%
    slice_min(order_by = distance, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Now, count crashes per intersection
  crash_counts <- nearest_matches %>%
    group_by(across(all_of(int_id_vars))) %>%
    summarise(NumCrashes = n(), .groups = 'drop')

  # Merge crash_counts back to intersections data
  intersections <- intersections %>%
    left_join(crash_counts, by = int_id_vars)

  # Rename the crash count variable
  intersections <- intersections %>%
    mutate(!!countvarname := replace_na(NumCrashes, 0)) %>%
    select(-NumCrashes)

  return(intersections)
}
