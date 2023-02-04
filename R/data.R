#' Population and number of out- and in-commuters by US county in 2000 
#' (data.frame)
#'
#' A dataset containing the number of inhabitant, in-commuters and out-commuters 
#' for 3018 US counties in 2000.
#'
#' @format A `data.frame` with 3,108 rows and 3 columns:
#' \describe{
#'   \item{rownames}{County ID.}
#'   \item{Population}{Number of inhabitants).}
#'   \item{Out-commuters}{Number of out-commuters.}
#'   \item{In-commuters}{Number of in-commuters.}
#' }
#' @source <https://www2.census.gov/programs-surveys/decennial/tables/2000/county-to-county-worker-flow-files/>
"mass"

#' Great-circle distances between US counties
#'
#' A dataset containing the great-circle distance (in meters) between 3,108 US 
#' counties.
#'
#' @format A `matrix` with 3,108 rows and 3,108 columns. Each element of the 
#' matrix represents the distance between two counties. County ID as rownames and 
#' colnames.
#' @source <https://www.sciencebase.gov/catalog/item/4f4e4a2ee4b07f02db615738>
"distance"

#' Origin-Destination commuting matrix between US counties in 2000
#'
#' A dataset containing the number of commuters between 3,108 US counties.
#'
#' @format A `matrix` with 3,108 rows and 3,108 columns. Each element of the 
#' matrix represents the number of commuters between two counties. County ID as 
#' rownames and colnames.
#' @source <https://www2.census.gov/programs-surveys/decennial/tables/2000/county-to-county-worker-flow-files/>
"od"

#' Spatial distribution of US counties in 2000
#'
#' A dataset containing the geometry of 3,108 US counties.
#'
#' @format A
#' \describe{
#'   \item{ID}{County ID.}
#'   \item{Longitude}{Longitude coordinate of the centroid of the county.}
#'   \item{Latitude}{Latitude coordinate of the centroid of the county.}
#'   \item{Area}{Surface area of the county (in square meter).}
#'   \item{geometry}{Geometry of the county.}
#' }
#' @source <https://www.sciencebase.gov/catalog/item/4f4e4a2ee4b07f02db615738>
"spcounty"

