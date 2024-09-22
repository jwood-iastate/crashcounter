# library(testthat)

test_that("crashCounts.seg.gdb works with sample data", {
  library(sf)
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

  expect_true("Total_Crashes" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_true(is.numeric(result$Total_Crashes))
})

test_that("crashCounts.seg.gdb works when road_id_vars is NULL", {
  road_segments_df <- data.frame(
    route_id = c("Route66", "I-5", "I-10")
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66"),
    Severity = c(1, 3, 2, 5, 4)
  )

  result <- crashCounts.seg.gdb(
    road_segments = road_segments_sf,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = NULL,
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  )

  result <- crashCounts.seg.gdb(
    road_segments = result,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    conditions = expression(Severity > 3),
    countvarname = "Severe_Crashes",
    projection_input = 4326,
    projection_working = 3857
  )

  result <- crashCounts.seg.gdb(
    road_segments = result,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    conditions = list(expression(Severity > 2)),
    countvarname = "Injury_Crashes",
    projection_input = 4326,
    projection_working = 3857
  )

  expect_true("road_id" %in% names(result))
  expect_true("Total_Crashes" %in% names(result))
})

test_that("crashCounts.seg.gdb error from crash_coords being incorrect type", {
  road_segments_df <- data.frame(
    route_id = c("Route66", "I-5", "I-10")
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

 expect_error(crashCounts.seg.gdb(
    road_segments = road_segments_sf,
    crashes = crashes_df,
    crash_coords  = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = NULL,
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  ))
})

test_that("crashCounts.seg.gdb works when road_geom is provided", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10")
  )
  road_geom <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_df$road_geom <- road_geom

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

  result <- crashCounts.seg.gdb(
    road_segments = road_segments_df,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    road_geom = "road_geom",
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  )

  expect_true("Total_Crashes" %in% names(result))
  expect_true(inherits(result, "sf"))
})

test_that("crashCounts.seg.gdb works with conditions as character", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10")
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
    Severity = c(1, 3, 2, 5, 4),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

  result <- crashCounts.seg.gdb(
    road_segments = road_segments_sf,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_feet = 75,
    conditions = "Severity > 3",
    countvarname = "Severe_Crashes",
    projection_input = 4326,
    projection_working = 3857
  )

  expect_true("Severe_Crashes" %in% names(result))
  expect_true(is.numeric(result$Severe_Crashes))
})

test_that("crashCounts.seg.gdb uses dist_meters over dist_feet", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10")
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

  result_feet <- crashCounts.seg.gdb(
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

  result_meters <- crashCounts.seg.gdb(
    road_segments = road_segments_sf,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    crashRoute = "route_id",
    roadRoute = "route_id",
    dist_meters = 30,
    projection_input = 4326,
    projection_working = 3857
  )

  expect_true(identical(result_feet$Total_Crashes, result_meters$Total_Crashes))
})

test_that("crashCounts.seg.gdb works when crashRoute and roadRoute are NULL", {
  road_segments_df <- data.frame(
    segment_id = 1:3
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.05, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.25, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

  result <- crashCounts.seg.gdb(
    road_segments = road_segments_sf,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    crashRoute = NULL,
    roadRoute = NULL,
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  )

  expect_true("Total_Crashes" %in% names(result))
  expect_true(is.numeric(result$Total_Crashes))
})

test_that("crashCounts.seg.gdb works when projection_working is NULL", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10")
  )
  road_segments_df$geometry <- st_sfc(
    st_linestring(rbind(c(-118.25, 34.05), c(-118.24, 34.05))),
    st_linestring(rbind(c(-118.24, 34.05), c(-118.24, 34.06))),
    st_linestring(rbind(c(-118.24, 34.06), c(-118.23, 34.06)))
  )
  road_segments_sf <- st_sf(road_segments_df, crs = 4326)

  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.05, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.25, -118.242, -118.235, -118.243, -118.238),
    route_id = c("Route66", "I-5", "I-10", "I-5", "Route66")
  )

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
    projection_working = NULL
  )

  expect_true("Total_Crashes" %in% names(result))
  expect_true(is.numeric(result$Total_Crashes))
})

test_that("crashCounts.seg.gdb errors when road_geom is invalid", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10"),
    road_geom = c("geom1", "geom2", "geom3")
  )
  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238)
  )

  expect_error(crashCounts.seg.gdb(
    road_segments = road_segments_df,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    road_geom = "road_geom",
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  ), "road_geom must be a column of class sfc_LINESTRING or sfc_MULTILINESTRING")
})

test_that("crashCounts.seg.gdb errors when road_geom is NULL and road_segments is not sf object", {
  road_segments_df <- data.frame(
    segment_id = 1:3,
    route_id = c("Route66", "I-5", "I-10")
  )
  crashes_df <- data.frame(
    crash_id = 1:5,
    crash_latitude = c(34.0505, 34.055, 34.065, 34.051, 34.062),
    crash_longitude = c(-118.245, -118.242, -118.235, -118.243, -118.238)
  )

  expect_error(crashCounts.seg.gdb(
    road_segments = road_segments_df,
    crashes = crashes_df,
    crash_lat = "crash_latitude",
    crash_long = "crash_longitude",
    road_id_vars = c("segment_id"),
    road_geom = NULL,
    dist_feet = 75,
    projection_input = 4326,
    projection_working = 3857
  ), "road_segments must be an sf object or road_geom must be provided.")
})


test_that("crashCounts.seg works with sample data", {
  set.seed(123)
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
    Severity = sample(1:5, 50, replace = TRUE)
  )

  result <- crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    countvarname = "Total_Crashes"
  )

  expect_true("Total_Crashes" %in% names(result))
  expect_equal(nrow(result), nrow(roads))
  expect_true(is.numeric(result$Total_Crashes))
})

test_that("crashCounts.seg works with conditions as character", {
  set.seed(123)
  roads <- data.frame(
    County = rep(c("A", "B"), each = 4),
    RouteNo = rep(1:2, each = 2, times = 2),
    Region = rep(c("North", "South"), each = 4),
    BeginMeas = c(0, 10, 20, 30, 0, 15, 30, 45),
    EndMeas = c(10, 20, 30, 40, 15, 30, 45, 60)
  )

  crashes <- data.frame(
    County = sample(c("A", "B"), 50, replace = TRUE),
    RouteNo = sample(1:2, 50, replace = TRUE),
    Region = sample(c("North", "South"), 50, replace = TRUE),
    Dist = runif(50, 0, 40),
    Severity = sample(1:5, 50, replace = TRUE)
  )

  result <- crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    conditions = "Severity > 3",
    countvarname = "Severe_Crashes"
  )

  expect_true("Severe_Crashes" %in% names(result))
  expect_true(is.numeric(result$Severe_Crashes))
})

test_that("crashCounts.seg works when no crashes match", {
  roads <- data.frame(
    County = "A",
    RouteNo = 1,
    Region = "North",
    BeginMeas = 0,
    EndMeas = 10
  )

  crashes <- data.frame(
    County = "B",
    RouteNo = 2,
    Region = "South",
    Dist = 20
  )

  result <- crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    countvarname = "Total_Crashes"
  )

  expect_equal(result$Total_Crashes, 0)
})

test_that("crashCounts.seg errors when required columns are missing", {
  roads <- data.frame(
    County = "A",
    RouteNo = 1,
    Region = "North",
    BeginMeas = 0,
    EndMeas = 10
  )

  crashes <- data.frame(
    County = "A",
    RouteNo = 1,
    Region = "North",
    Severity = 3
  )

  expect_error(crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    countvarname = "Total_Crashes"
  ))
})

test_that("crashCounts.seg works with conditions as expression", {
  set.seed(123)
  roads <- data.frame(
    County = rep(c("A", "B"), each = 4),
    RouteNo = rep(1:2, each = 2, times = 2),
    Region = rep(c("North", "South"), each = 4),
    BeginMeas = c(0, 10, 20, 30, 0, 15, 30, 45),
    EndMeas = c(10, 20, 30, 40, 15, 30, 45, 60)
  )

  crashes <- data.frame(
    County = sample(c("A", "B"), 50, replace = TRUE),
    RouteNo = sample(1:2, 50, replace = TRUE),
    Region = sample(c("North", "South"), 50, replace = TRUE),
    Dist = runif(50, 0, 40),
    Severity = sample(1:5, 50, replace = TRUE),
    Time = sample(0:23, 50, replace = TRUE)
  )

  conditions_list <- expression(Severity > 2)

  result <- crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    conditions = conditions_list,
    countvarname = "Night_Severe_Crashes"
  )

  expect_true("Night_Severe_Crashes" %in% names(result))
  expect_true(is.numeric(result$Night_Severe_Crashes))
})

test_that("crashCounts.seg works with conditions as list", {
  set.seed(123)
  roads <- data.frame(
    County = rep(c("A", "B"), each = 4),
    RouteNo = rep(1:2, each = 2, times = 2),
    Region = rep(c("North", "South"), each = 4),
    BeginMeas = c(0, 10, 20, 30, 0, 15, 30, 45),
    EndMeas = c(10, 20, 30, 40, 15, 30, 45, 60)
  )

  crashes <- data.frame(
    County = sample(c("A", "B"), 50, replace = TRUE),
    RouteNo = sample(1:2, 50, replace = TRUE),
    Region = sample(c("North", "South"), 50, replace = TRUE),
    Dist = runif(50, 0, 40),
    Severity = sample(1:5, 50, replace = TRUE),
    Time = sample(0:23, 50, replace = TRUE)
  )

  conditions_list <- list(expression(Severity > 2), expression(Time >= 18 | Time <= 6))

  result <- crashCounts.seg(
    roads = roads,
    crashes = crashes,
    road_id_vars = c("County", "RouteNo", "Region"),
    start_mp = "BeginMeas",
    end_mp = "EndMeas",
    crash_mp = "Dist",
    conditions = conditions_list,
    countvarname = "Night_Severe_Crashes"
  )

  expect_true("Night_Severe_Crashes" %in% names(result))
  expect_true(is.numeric(result$Night_Severe_Crashes))
})

test_that("crashCounts.int",{

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
      countvarname = "Crashes",
      projection_input = 4326,
      projection_working = 3857
    )

  expect_true("Crashes" %in% names(result))
})


test_that("crashCounts.int Errors",{

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
  expect_error(crashCounts.int(
    intersections = intersections_df,
    crashes = crashes_df,
    int_coords = "latitude",
    crash_lat = "latitude",
    crash_long = "longitude",
    int_id_vars = c("intersection_id"),
  ))

  expect_error(crashCounts.int(
    intersections = intersections_df,
    crashes = crashes_df,
    int_lat = "latitude",
    int_long = "longitude",
    crash_lat = "latitude",
    crash_long = "longitude",
    int_id_vars = c("intersection_id"),
  ))

  expect_error(crashCounts.int(
    intersections = intersections_df,
    crashes = crashes_df,
    int_lat = "latitude",
    int_long = "longitude",
    crash_coords = "crash_latitude",
    intersectionRoutes = "routes"))

})

test_that("crashCounts.int works with geospatial data",{

  # Using provided datasets
  data(crashes)
  data(intersections)

  # Run the function
  result <- crashCounts.int(
    intersections = intersections,
    crashes = crashes,
    dist_feet = 300
  )

  expect_true("Total_Crashes" %in% names(result))
})

test_that("crashCounts.int works with conditions",{

  # Using provided datasets
  data(crashes)
  data(intersections)

  # Run the function
  result <- crashCounts.int(
    intersections = intersections,
    crashes = crashes,
    dist_feet = 300,
    conditions = expression(CSEVERITY > 3)
  )

  expect_true("Total_Crashes" %in% names(result))


  result2 <- crashCounts.int(
    intersections = intersections,
    crashes = crashes,
    dist_feet = 300,
    conditions = expression(CSEVERITY <5 & CSEVERITY > 2))

  expect_true("Total_Crashes" %in% names(result2))

  result3 <- crashCounts.int(
    intersections = intersections,
    crashes = crashes,
    dist_feet = 300,
    conditions = "CSEVERITY <5")

  expect_true("Total_Crashes" %in% names(result3))


})
