
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crashcounter

<!-- badges: start -->

[![codecov](https://codecov.io/gh/jwood-iastate/crashcounter/branch/master/graph/badge.svg?token=ofsyBpE669)](https://codecov.io/gh/jwood-iastate/crashcounter)
<!-- badges: end -->

The goal of crashcounter is to provide simple functions for use in
preparing crash data for count regression modeling. Users should be able
to get crash counts for road segments and intersections (total crashes,
crashes that meet specific conditions, etc.) using the function(s) in
this package.

## Installation

You can install the development version of crashcounter like so:

``` r
library(remotes)
remotes::install_github("jwood-iastate/crashcounter")
```

## Example: Road Segments using Linear Referencing System (LRS)

Function: `crashCounts.seg()`

This function merges a roads dataset with a crashes dataset based on
specified route identifiers and segment boundaries. It aggregates crash
data per road segment, allowing for optional user-specified conditions
to filter crashes based on various attributes.

``` r
library(crashcounter)
set.seed(123) # for reproducibility

roads <- data.frame(
  County = rep(c("A", "B"), each = 4),
  RouteNo = rep(1:2, each = 2, times = 2),
  Region = rep(c("North", "South"), each = 4),
  BeginMeas = c(0, 10, 20, 30, 0, 15, 30, 45),
  EndMeas = c(10, 20, 30, 40, 15, 30, 45, 60),
  NoLanes = sample(2:4, 8, replace = TRUE),
  LaneWidth = runif(8, 3.0, 3.5),
  ShoulderWidth = runif(8, 1.0, 2.5),
  SpeedLimit = sample(c(55, 65, 75), 8, replace = TRUE)
)

crashes <- data.frame(
  County = sample(c("A", "B"), 50, replace = TRUE),
  RouteNo = sample(1:2, 50, replace = TRUE),
  Region = sample(c("North", "South"), 50, replace = TRUE),
  Dist = runif(50, 0, 40),
  Severity = sample(1:5, 50, replace = TRUE),
  Type = sample(c("Collision", "Rollover", "Off-road", "Animal"), 50, replace = TRUE),
  DayofWeek = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 50, replace = TRUE),
  Month = sample(1:12, 50, replace = TRUE),
  Year = sample(2018:2020, 50, replace = TRUE),
  Time = sample(0:23, 50, replace = TRUE),
  NoVehicles = sample(1:4, 50, replace = TRUE),
  NoPeople = sample(1:6, 50, replace = TRUE),
  NoPedestrians = sample(0:2, 50, replace = TRUE)
)

# Run the function
result_basic <- crashCounts.seg(
  roads = roads,
  crashes = crashes,
  road_id_vars = c("County", "RouteNo", "Region"),
  start_mp = "BeginMeas",
  end_mp = "EndMeas",
  crash_mp = "Dist",
  countvarname = "Total_Crashes"
)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# View the result
print(result_basic)
#>   County RouteNo Region BeginMeas EndMeas NoLanes LaneWidth ShoulderWidth
#> 1      A       1  North         0      10       4  3.275718      1.369132
#> 2      A       1  North        10      20       4  3.228307      1.063089
#> 3      A       2  North        20      30       4  3.478417      1.491881
#> 4      A       2  North        30      40       3  3.226667      2.431755
#> 5      B       1  South         0      15       4  3.338785      2.334309
#> 6      B       1  South        15      30       3  3.286317      2.039205
#> 7      B       2  South        30      45       3  3.051462      1.960760
#> 8      B       2  South        45      60       3  3.449912      2.491405
#>   SpeedLimit Total_Crashes
#> 1         55             2
#> 2         75             1
#> 3         65             3
#> 4         75             1
#> 5         65             1
#> 6         55             1
#> 7         65             0
#> 8         75             0
```

For this example, the unique road segments are identified using the
combination of:

- `County`
- `Region`
- `RouteNo`
- `BeginMeas`
- `EndMeas`

In many cases, roadway datasets will have a single variable identifying
the route (in this case, the combination of `County`, `Region`, and
`RouteNo` are required). The variable `BeginMeas` is the same concept as
a starting milepost and `EndMeas` is the same concept as the end
milepost for the road segment.

To identify crashes that occur on a given road segment, they have to
match `County`, `Region`, and `RouteNo` and have a value for `Dist` that
is greater than or equal to `BeginMeas` and less than `EndMeas`. To get
specific crash types, conditions can be added.

To illustrate how to add other crash types and use conditions, several
additional counts are added below.

Adding Fatal+Injury Crashes - Illustrating the use of a single
condition:

``` r
result_severity <- crashCounts.seg(
  roads = result_basic, # Use the roads dataframe that has the total crashes
  crashes = crashes,
  road_id_vars = c("County", "RouteNo", "Region"),
  start_mp = "BeginMeas",
  end_mp = "EndMeas",
  crash_mp = "Dist",
  conditions = "Severity > 2",
  countvarname = "FatalInjury_Crashes"
)

# View the result
print(result_severity)
#>   County RouteNo Region BeginMeas EndMeas NoLanes LaneWidth ShoulderWidth
#> 1      A       1  North         0      10       4  3.275718      1.369132
#> 2      A       1  North        10      20       4  3.228307      1.063089
#> 3      A       2  North        20      30       4  3.478417      1.491881
#> 4      A       2  North        30      40       3  3.226667      2.431755
#> 5      B       1  South         0      15       4  3.338785      2.334309
#> 6      B       1  South        15      30       3  3.286317      2.039205
#> 7      B       2  South        30      45       3  3.051462      1.960760
#> 8      B       2  South        45      60       3  3.449912      2.491405
#>   SpeedLimit Total_Crashes FatalInjury_Crashes
#> 1         55             2                   1
#> 2         75             1                   1
#> 3         65             3                   1
#> 4         75             1                   1
#> 5         65             1                   1
#> 6         55             1                   0
#> 7         65             0                   0
#> 8         75             0                   0
```

Next, add more conditions (in this case, nighttime fatal+injury
crashes):

``` r
# Aggregation with multiple conditions: Count crashes with severity greater than 2 and during nighttime
result_multiple <- crashCounts.seg(
  roads = result_severity,
  crashes = crashes,
  road_id_vars = c("County", "RouteNo", "Region"),
  start_mp = "BeginMeas",
  end_mp = "EndMeas",
  crash_mp = "Dist",
  conditions = list(
    expression(Severity > 2),
    expression(Time < 6 | Time > 18)  # Assuming nighttime is before 6 AM or after 6 PM
  ),
  countvarname = "NightFatalInj_Crashes"
)

# View the result
print(result_multiple)
#>   County RouteNo Region BeginMeas EndMeas NoLanes LaneWidth ShoulderWidth
#> 1      A       1  North         0      10       4  3.275718      1.369132
#> 2      A       1  North        10      20       4  3.228307      1.063089
#> 3      A       2  North        20      30       4  3.478417      1.491881
#> 4      A       2  North        30      40       3  3.226667      2.431755
#> 5      B       1  South         0      15       4  3.338785      2.334309
#> 6      B       1  South        15      30       3  3.286317      2.039205
#> 7      B       2  South        30      45       3  3.051462      1.960760
#> 8      B       2  South        45      60       3  3.449912      2.491405
#>   SpeedLimit Total_Crashes FatalInjury_Crashes NightFatalInj_Crashes
#> 1         55             2                   1                     0
#> 2         75             1                   1                     1
#> 3         65             3                   1                     1
#> 4         75             1                   1                     0
#> 5         65             1                   1                     1
#> 6         55             1                   0                     0
#> 7         65             0                   0                     0
#> 8         75             0                   0                     0
```

## Example: Segments using spatial data

Function: `crashCounts.seg.gdb()`

This function is similar to `crashCounts.seg()` but is designed to work
with spatial data stored in a file geodatabase (GDB). The function reads
the road and crash data from a GDB and returns the aggregated crash data
per road segment.

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE
# Sample road segments data with route IDs
road_segments_df <- data.frame(
  segment_id = 1:3,
  route_id = c("Route66", "I-5", "I-10")
)
# Create simple lines for the example
road_segments_df$geometry <- st_sfc(
  st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
  st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
  st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
)
road_segments_sf <- st_sf(road_segments_df, crs = 4326)

# Sample crash data with route IDs
crashes_df <- data.frame(
  crash_id = 1:5,
  crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
  crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
  Severity = c(1, 3, 2, 5, 4),
  Time = c(2, 15, 20, 5, 23),
  route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
)

# Run the function
result <- crashCounts.seg.gdb(
  road_segments = road_segments_sf,
  crashes = crashes_df,
  crash_lat = "crash_latitude",
  crash_long = "crash_longitude",
  road_id_vars = c("segment_id"),
  crashRoute = "route_id",
  roadRoute = "route_id",
  dist_feet = 75,
  projection_input = 4326,
  projection_working = 3857
)

# View the result
print(result)
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -13163530 ymin: 4035518 xmax: -13161300 ymax: 4036861
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   segment_id route_id                       geometry Total_Crashes
#> 1          1  Route66 LINESTRING (-13163530 40355...             0
#> 2          2      I-5 LINESTRING (-13162417 40355...             0
#> 3          3     I-10 LINESTRING (-13162417 40368...             0
```

# Example: Intersections

Function: `crashCounts.int()`

This function merges a roads dataset with a crashes dataset based on
specified intersection identifiers. It aggregates crash data per
intersection, allowing for optional user-specified conditions to filter
crashes based on various attributes.

``` r
# Sample data frames
intersections_df <- data.frame(
  intersection_id = 1:3,
  latitude = c(34.0522, 34.0525, 34.0528),
  longitude = c(-118.2437, -118.2440, -118.2443)
)

crashes_df <- data.frame(
  crash_id = 1:5,
  crash_latitude = c(34.0520, 34.0526, 34.0527, 34.0510, 34.0530),
  crash_longitude = c(-118.2430, -118.2441, -118.2442, -118.2445, -118.2446),
  Severity = c(1, 3, 2, 5, 4),
  Time = c(2, 15, 20, 5, 23)
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
  dist_feet = 500,
)

# View the result
print(result)
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -118.2443 ymin: 34.0522 xmax: -118.2437 ymax: 34.0528
#> Geodetic CRS:  WGS 84
#>   intersection_id Total_Crashes                  geometry
#> 1               1             1 POINT (-118.2437 34.0522)
#> 2               2             1  POINT (-118.244 34.0525)
#> 3               3             2 POINT (-118.2443 34.0528)
```
