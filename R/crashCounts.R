#' Merge and Aggregate Crash Data by Road Segments
#'
#' This function merges a roads dataset with a crashes dataset based on specified route identifiers
#' and segment boundaries. It then aggregates crash data per road segment, allowing
#' for optional user-specified conditions to filter crashes based on various attributes.
#'
#' @param roads A data frame containing road segment data. Expected fields include segment identifiers
#'   specified by `road_id_vars`, as well as the starting and ending milepost variables specified
#'   by `start_mp` and `end_mp`.
#' @param crashes A data frame containing crash data. Expected fields include crash location specified
#'   by `crash_mp` and the same segment identifiers as `roads` specified by `road_id_vars`.
#' @param road_id_vars A character vector specifying the column names that uniquely identify each road
#'   segment. For example, `c("County", "RouteNo", "Region")` or `c("RouteID")` if only a single
#'   identifier is required.
#' @param start_mp A string specifying the name of the column in `roads` that represents the starting
#'   milepost or measurement for each road segment.
#' @param end_mp A string specifying the name of the column in `roads` that represents the ending
#'   milepost or measurement for each road segment.
#' @param crash_mp A string specifying the name of the column in `crashes` that represents the crash
#'   location (e.g., milepoint or distance).
#' @param conditions Optional. A condition or set of conditions for filtering the crashes data frame
#'   before aggregation. This can be specified as a character string (e.g., `"Severity > 2"`),
#'   an expression (e.g., `expression(Severity > 2)`), or a list of expressions
#'   (e.g., `list(expression(Severity > 2), expression(Time < 6 | Time > 18))`). If `NULL`
#'   (default), no additional conditions are applied.
#' @param countvarname Optional. A string for the name of the new variable with the crash counts.
#'   The default is `"Total_Crashes"` but should be specified by the user for any time conditions
#'   are provided.
#'
#' @return A data frame with the original road data and a new variable that has the crash
#'   counts for the specified conditions (if any).
#' @import dplyr
#' @examples
#' set.seed(123)
#' roads <- data.frame(
#'   County = rep(c("A", "B"), each = 4),
#'   RouteNo = rep(1:2, each = 2, times = 2),
#'   Region = rep(c("North", "South"), each = 4),
#'   BeginMeas = c(0, 10, 20, 30, 0, 15, 30, 45),
#'   EndMeas = c(10, 20, 30, 40, 15, 30, 45, 60),
#'   NoLanes = sample(2:4, 8, replace = TRUE),
#'   LaneWidth = runif(8, 3.0, 3.5),
#'   ShoulderWidth = runif(8, 1.0, 2.5),
#'   SpeedLimit = sample(c(55, 65, 75), 8, replace = TRUE)
#' )
#'
#' crashes <- data.frame(
#'   County = sample(c("A", "B"), 50, replace = TRUE),
#'   RouteNo = sample(1:2, 50, replace = TRUE),
#'   Region = sample(c("North", "South"), 50, replace = TRUE),
#'   Dist = runif(50, 0, 40),
#'   Severity = sample(1:5, 50, replace = TRUE),
#'   Type = sample(c("Collision", "Rollover", "Off-road", "Animal"), 50, replace = TRUE),
#'   DayofWeek = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 50, replace = TRUE),
#'   Month = sample(1:12, 50, replace = TRUE),
#'   Year = sample(2018:2020, 50, replace = TRUE),
#'   Time = sample(0:23, 50, replace = TRUE),
#'   NoVehicles = sample(1:4, 50, replace = TRUE),
#'   NoPeople = sample(1:6, 50, replace = TRUE),
#'   NoPedestrians = sample(0:2, 50, replace = TRUE)
#' )
#'
#' # Simple merge
#' result_basic <- crashCounts.seg(
#'   roads = roads,
#'   crashes = crashes,
#'   road_id_vars = c("County", "RouteNo", "Region"),
#'   start_mp = "BeginMeas",
#'   end_mp = "EndMeas",
#'   crash_mp = "Dist",
#'   countvarname = "Total_Crashes"
#' )
#'
#' # View the result
#' print(result_basic)
#' @export
crashCounts.seg <- function(
    roads,
    crashes,
    road_id_vars,
    start_mp,
    end_mp,
    crash_mp,
    conditions = NULL,
    countvarname = "Total_Crashes"
) {
  library(dplyr)

  # Ensure that start_mp, end_mp, and crash_mp are numeric
  roads <- roads %>%
    mutate(
      across(all_of(c(start_mp, end_mp)), as.numeric)
    )

  crashes <- crashes %>%
    mutate(
      across(all_of(crash_mp), as.numeric)
    )

  # Apply additional user-specified conditions if provided
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

  # Merge crashes to roads on road_id_vars
  merged_data <- roads %>%
    left_join(crashes, by = road_id_vars)

  # Filter merged data where crash_mp is between start_mp and end_mp
  merged_data <- merged_data %>%
    filter(
      .data[[crash_mp]] >= .data[[start_mp]] & .data[[crash_mp]] < .data[[end_mp]] |
        is.na(.data[[crash_mp]])
    )

  # Count crashes per road segment
  crash_counts <- merged_data %>%
    group_by(across(all_of(c(road_id_vars, start_mp, end_mp)))) %>%
    summarise(
      NumCrashes = sum(!is.na(.data[[crash_mp]])),
      .groups = 'drop'
    )

  # Merge crash counts back to roads
  roaddata <- roads %>%
    left_join(crash_counts, by = c(road_id_vars, start_mp, end_mp))

  # Replace NA crash counts with zero
  roaddata[[countvarname]] <- tidyr::replace_na(roaddata$NumCrashes, 0)

  # Remove temporary NumCrashes column
  roaddata <- roaddata %>%
    select(-NumCrashes)

  return(roaddata)
}
