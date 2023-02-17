#' Population and number of out- and in-commuters by US Kansas county in 2000
#' (data.frame)
#'
#' A dataset containing the number of inhabitant, in-commuters and out-commuters
#' for 105 US Kansas counties in 2000.
#'
#' @format A `data.frame` with 105 rows and 3 columns:
#' \describe{
#'   \item{rownames}{County ID.}
#'   \item{Population}{Number of inhabitants).}
#'   \item{Out-commuters}{Number of out-commuters.}
#'   \item{In-commuters}{Number of in-commuters.}
#' }
#' @source <https://www2.census.gov/programs-surveys/decennial/tables/2000/county-to-county-worker-flow-files/>
"mass"

#' Great-circle distances between US Kansas counties
#'
#' A dataset containing the great-circle distance (in kilometers) between 105
#' US Kansas counties.
#'
#' @format A `matrix` with 105 rows and 105 columns. Each element of the
#' matrix represents the distance between two counties. County ID as rownames
#' and colnames.
#' @source <https://www.sciencebase.gov/catalog/item/4f4e4a2ee4b07f02db615738>
"distance"

#' Origin-Destination commuting matrix between US Kansas counties in 2000
#'
#' A dataset containing the number of commuters between 105 US Kansas counties.
#'
#' @format A `matrix` with 105 rows and 105 columns. Each element of the
#' matrix represents the number of commuters between two counties. County ID as
#' rownames and colnames.
#' @source <https://www2.census.gov/programs-surveys/decennial/tables/2000/county-to-county-worker-flow-files/>
"od"

#' Spatial distribution of US Kansas counties in 2000
#'
#' A dataset containing the geometry of 105 US Kansas counties.
#'
#' @format
#' \describe{
#'   \item{ID}{County ID.}
#'   \item{Longitude}{Longitude coordinate of the centroid of the county.}
#'   \item{Latitude}{Latitude coordinate of the centroid of the county.}
#'   \item{Area}{Surface area of the county (in square kilometer).}
#'   \item{geometry}{Geometry of the county.}
#' }
#' @source <https://www.sciencebase.gov/catalog/item/4f4e4a2ee4b07f02db615738>
"county"