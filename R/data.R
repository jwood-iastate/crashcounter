#' Sample Crash Data
#'
#' A sample data set containing crash information for use in demonstrating the crash analysis functions.
#'
#' @format An \code{sf} data frame with spatial point geometry and the following variables:
#' \describe{
#'   \item{ROUTEID}{Route ID}
#'   \item{MEASURE}{Milepost for the Crash}
#'   \item{CRASH_KEY}{Crash Identifier}
#'   \item{CRASH_YEAR}{Year the Crash Occurred}
#'   \item{LATITUDE}{GPS Latitude}
#'   \item{LONGITUDE}{GPS Longitude}
#'   \item{MPO_NUMBER}{Metropolitan Planning Organization Number}
#'   \item{DOTDSTRCT}{DOT District}
#'   \item{COUNTY}{County Name}
#'   \item{CITYBR}{City or Town}
#'   \item{WZ_RELATED}{Work Zone Related}
#'   \item{CSEVERITY}{Crash Severity}
#'   \item{FATALITIES}{Number of Fatalities}
#'   \item{INJURIES}{Number of Injuries}
#'   \item{MAJINJURY}{Number of Major Injuries}
#'   \item{MININJURY}{Number of Minor Injuries}
#'   \item{POSSINJURY}{Number of Possible Injuries}
#'   \item{UNKINJURY}{Number of Unknown Injuries}
#'   \item{PROPDMG}{Property Damage}
#'   \item{VEHICLES}{Number of Vehicles}
#' }
"crashes"

#' Sample Intersection Data
#'
#' A sample data set containing intersection information for use in demonstrating the crash analysis functions.
#'
#' @format An \code{sf} data frame with spatial point geometry and the following variables:
#' \describe{
#'   \item{Intersection_ID}{Intersection ID}
#'   \item{Lat_int}{Intersection Latitude}
#'   \item{Long_int}{Intersection Longitude}
#'   \item{major_AADT}{Major Road Annual Average Daily Traffic}
#'   \item{minor_AADT}{Minor Road Annual Average Daily Traffic}
#' }
"intersections"
