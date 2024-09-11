
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crashcounter

<!-- badges: start -->

[![codecov](https://codecov.io/gh/jwood-iastate/crashcounter/graph/badge.svg?token=ofsyBpE669)](https://codecov.io/gh/jwood-iastate/crashcounter)
<!-- badges: end -->

The goal of crashcounter is to provide a simple function for use in
preparing crash data for count regression modeling. Users should be able
to get crash counts for road segments (total crashes, crashes that meet
specific conditions, etc.) using the function(s) in this package.

## Installation

You can install the development version of crashcounter like so:

``` r
library(devtools)
install_github("jwood-iastate/crashcounter")
```

## Example

TO illustrate how this can be used, a roadway dataframe and a crash
dataframe are created and shown below.

``` r
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

print(roads)
#>   County RouteNo Region BeginMeas EndMeas NoLanes LaneWidth ShoulderWidth
#> 1      A       1  North         0      10       4  3.275718      1.369132
#> 2      A       1  North        10      20       4  3.228307      1.063089
#> 3      A       2  North        20      30       4  3.478417      1.491881
#> 4      A       2  North        30      40       3  3.226667      2.431755
#> 5      B       1  South         0      15       4  3.338785      2.334309
#> 6      B       1  South        15      30       3  3.286317      2.039205
#> 7      B       2  South        30      45       3  3.051462      1.960760
#> 8      B       2  South        45      60       3  3.449912      2.491405
#>   SpeedLimit
#> 1         55
#> 2         75
#> 3         65
#> 4         75
#> 5         65
#> 6         55
#> 7         65
#> 8         75
```

``` r
head(crashes)
#>   County RouteNo Region     Dist Severity      Type DayofWeek Month Year Time
#> 1      B       2  South 10.60071        1  Rollover    Friday     2 2018   23
#> 2      B       2  South 23.77373        4  Off-road    Sunday     6 2018   22
#> 3      A       2  South 19.25159        2 Collision    Sunday    10 2020    3
#> 4      A       1  South 10.60131        4  Off-road  Thursday    12 2019   23
#> 5      A       1  South 22.58362        5    Animal   Tuesday     9 2020    2
#> 6      A       2  South 36.52753        5 Collision    Friday    12 2019   13
#>   NoVehicles NoPeople NoPedestrians
#> 1          4        5             0
#> 2          3        6             2
#> 3          2        4             2
#> 4          2        4             2
#> 5          2        5             0
#> 6          2        3             2
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

While these are synthetic data, actual datasets can easily be used.

To use the function for getting the crash counts for total crashes (no
conditions):

``` r
library(crashcounter) # Load the library for use

result_basic <- crashCounts(
  roads = roads,
  crashes = crashes,
  road_id_vars = c("County", "RouteNo", "Region"),
  start_mp = "BeginMeas",
  end_mp = "EndMeas",
  crash_mp = "Dist",
  countvarname = "Total_Crashes"
)
#> Joining with `by = join_by(County, RouteNo, Region, BeginMeas, EndMeas)`

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

Adding Fatal+Injury Crashes - Illustrating the use of a single
condition:

``` r
result_severity <- crashCounts(
  roads = result_basic, # Use the roads dataframe that has the total crashes
  crashes = crashes,
  road_id_vars = c("County", "RouteNo", "Region"),
  start_mp = "BeginMeas",
  end_mp = "EndMeas",
  crash_mp = "Dist",
  conditions = "Severity > 2",
  countvarname = "FatalInjury_Crashes"
)
#> Joining with `by = join_by(County, RouteNo, Region, BeginMeas, EndMeas)`

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
result_multiple <- crashCounts(
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
#> Joining with `by = join_by(County, RouteNo, Region, BeginMeas, EndMeas)`

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
