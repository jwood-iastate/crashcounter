test_that("Package works", {

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

    expect_warning(crashCounts(
      roads = roads,
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
  )
})

test_that("Package Single condition", {

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

    expect_warning(crashCounts(
      roads = roads,
      crashes = crashes,
      road_id_vars = c("County", "RouteNo", "Region"),
      start_mp = "BeginMeas",
      end_mp = "EndMeas",
      crash_mp = "Dist",
      conditions = "Severity > 2",  # Severity condition      
      countvarname = "NightFatalInj_Crashes"
    )
  )
})

test_that("No condition", {

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

    expect_warning(crashCounts(
      roads = roads,
      crashes = crashes,
      road_id_vars = c("County", "RouteNo", "Region"),
      start_mp = "BeginMeas",
      end_mp = "EndMeas",
      crash_mp = "Dist",
      countvarname = "NightFatalInj_Crashes"
    )
  )
})

