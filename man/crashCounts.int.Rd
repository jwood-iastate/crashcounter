% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crashCountsIntersectionsGDP.R
\name{crashCounts.int}
\alias{crashCounts.int}
\title{Merge and Aggregate Crash Data by Intersections using GPS Data}
\usage{
crashCounts.int(
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
)
}
\arguments{
\item{intersections}{A data frame containing intersection data. Expected
fields include intersection identifiers specified by `int_id_vars`, as
well as the latitude and longitude coordinates of the intersections.}

\item{crashes}{A data frame containing crash data. Expected fields include
crash locations specified by latitude and longitude.}

\item{int_lat}{The name of the variable for the intersection latitude.}

\item{int_long}{The name of the variable for the intersection longitude.}

\item{crash_lat}{The name of the variable for the crash latitude.}

\item{crash_long}{The name of the variable for the crash longitude.}

\item{int_id_vars}{Optional. A character vector specifying the column names
that uniquely identify each intersection. If `NULL` (default), a new
identifier will be created.}

\item{int_coords}{Optional. A string specifying the name of the variable with
the GPS coordinates (latitude and longitude) of the intersections. If this
is used, it will override `int_lat` and `int_long`. The variable should be
of class `sfc_POINT`.}

\item{crash_coords}{Optional. A string specifying the name of the variable
with the GPS coordinates for the crashes. If this is used, it will override
`crash_lat` and `crash_long`. The variable should be of class `sfc_POINT`.}

\item{conditions}{Optional. A condition or set of conditions for filtering
the crashes data frame before aggregation. This can be specified as a
character string (e.g., `"Severity > 2"`), an expression (e.g.,
`expression(Severity > 2)`), or a list of expressions (e.g.,
`list(expression(Severity > 2), expression(Time < 6 | Time > 18))`).
If `NULL` (default), no additional conditions are applied.}

\item{countvarname}{Optional. A string specifying the name of the new variable
with the crash counts. The default is `"Total_Crashes"` but should be
specified by the user if any conditions are provided.}

\item{dist_feet}{Optional. A numeric value specifying the distance threshold
(in feet). Either `dist_feet` or `dist_meters` should be used. The default
is `dist_feet = 250`.}

\item{dist_meters}{Optional. A numeric value specifying the distance
threshold (in meters). If this is used, it overrides `dist_feet`.}

\item{crashRoute}{Optional. The name of the route ID variable in the crash
data frame to match the intersections on. If this is provided along with
`intersectionRoutes`, crashes will be assigned only to intersections where
the crash's route ID matches one of the intersection's route IDs.}

\item{intersectionRoutes}{Optional. The name of the route IDs variable in the
intersections data frame. This column should contain the route IDs
associated with each intersection. It can be a single route ID or a list
of route IDs (e.g., a character vector or a list column).}

\item{projection_input}{Optional. A character string or numeric value specifying
the CRS (Coordinate Reference System) of the input data. Defaults to 4326
(WGS84 latitude/longitude). If the data frames are spatial objects, this will be extracted.}

\item{projection_working}{Optional. A character string or numeric value specifying
the CRS to which data will be transformed for accurate distance calculations.
Defaults to `NULL` which skips the transformation.}
}
\value{
A data frame with the original intersection data and a new variable that
  contains the crash counts for the specified conditions (if any).
}
\description{
This function merges an intersection dataset with a crash dataset based on
GPS locations and distances. It aggregates crash data per intersection,
assigning each crash to the nearest intersection within a specified buffer
distance. Optional conditions can filter crashes based on various attributes.
Additionally, crashes are assigned to intersections only if the crash's route ID
matches one of the intersection's route IDs, if such information is provided.
}
\examples{

# Sample intersection data with route IDs
intersections_df <- data.frame(
  intersection_id = 1:3,
  latitude = c(34.0522, 34.0525, 34.0528),
  longitude = c(-118.2437, -118.2440, -118.2443),
  routes = c("Route66,I-5", "I-5,I-10", "Route66,I-10")
)

# Sample crash data with route IDs
crashes_df <- data.frame(
  crash_id = 1:5,
  crash_latitude = c(34.0520, 34.0526, 34.0527, 34.0510, 34.0530),
  crash_longitude = c(-118.2430, -118.2441, -118.2442, -118.2445, -118.2446),
  Severity = c(1, 3, 2, 5, 4),
  Time = c(2, 15, 20, 5, 23),
  route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
)

# Run the function
result <- crashCounts.int(
  intersections = intersections_df,
  crashes = crashes_df,
  int_lat = "latitude",
  int_long = "longitude",
  crash_lat = "crash_latitude",
  crash_long = "crash_longitude",
  int_id_vars = c("intersection_id"),
  crashRoute = "route_id",
  intersectionRoutes = "routes",
  dist_feet = 500,
  projection_input = 4326,
  projection_working = 3857
)

# View the result
print(result)

# Using provided datasets
data(crashes)
data(intersections)

# Run the function
result <- crashCounts.int(
            intersections = intersections,
            crashes = crashes,
            dist_feet = 300
 )

 # View the result
 print(result)

}
