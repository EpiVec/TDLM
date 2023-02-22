#' Extract distances and surface areas from a spatial object
#'
#' This function returns a matrix of distances between locations (in kilometer)
#'  along with a vector surface areas of the locations (in squared kilometer).
#'
#' @param geometry a spatial object that can be handled by the `sf` package.
#'
#' @param id name or number of the column to use as rownames and colnames for
#' the output distance matrix (optional, NULL by default). A vector with length
#' equal to the number of locations can also be used.
#'
#' @param show_progress a boolean indicating if a progress bar should be
#' displayed.
#'
#' @details The `geometry` must be projected in a valid coordinate reference
#' system. It will be reprojected in degrees longitude/latitude to compute the
#' great-circle distances between centroids' locations with an internal function
#' and to compute the surface area with the function [st_area][sf::st_area] from
#' the [sf](https://cran.rstudio.com/web/packages/sf/index.html) package. The
#' centroid of the locations are extracted with the function
#' [st_centroid_coords][sfExtras::st_centroid_coords] from
#' the [sfExtras](https://cran.rstudio.com/web/packages/sfExtras/index.html)
#'  package.
#
#' @note The outputs are based on the locations contained in `geometry` and
#' sorted in the same order. An optional `id` can also be provided to be used as
#' names for the outputs.
#'
#' @return
#' A list composed of two elements. The first element is a squared matrix
#' representing the great-circle distance (in kilometer) between locations. The
#' second element is a vector containing the surface area of each location
#' (in squared kilometer).
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [extract_opportunities()] [check_format_names()]
#'
#' @examples
#' data(county)
#'
#' res <- extract_spatial_information(county, id = "ID")
#'
#' dim(res$distance)
#'
#' length(res$surface)
#'
#' @export
extract_spatial_information <- function(geometry,
                                        id = NULL,
                                        show_progress = FALSE) {
  # Controls geometry
  if (class(geometry)[1] != "sf") {
    stop("It seems that the geometry used is not an sf object.",
      call. = FALSE
    )
  }
  crs <- sf::st_crs(geometry)
  if (is.na(crs)) {
    stop("Not possible to identify the coordinate reference system of geometry.",
      call. = FALSE
    )
  }

  # Controls id
  if (!is.null(id)) {
    if (length(id) == 1) {
      if (is.character(id)) {
        if (!(id %in% colnames(geometry))) {
          stop("If id is a character, it should be a column name of geometry.",
            call. = FALSE
          )
        }
        index <- which(colnames(geometry) == id)
        if (length(index) > 1) {
          stop("Two or more columns with name id.",
            call. = FALSE
          )
        }
      } else if (is.factor(id)) {
        id <- as.character(id)
        if (!(id %in% colnames(geometry))) {
          stop("If id is a character, it should be a column name of geometry.",
            call. = FALSE
          )
        }
        index <- which(colnames(geometry) == id)
        if (length(index) > 1) {
          stop("Two or more columns with name id.",
            call. = FALSE
          )
        }
      } else if (is.numeric(id)) {
        if (id %% 1 != 0) {
          stop("If id is numeric, it should be an integer.", call. = FALSE)
        } else {
          if ((id < 1)) {
            stop("id should be stricltly positive.", call. = FALSE)
          }
          if ((id > dim(geometry)[2])) {
            stop("id should be lower or equal to ", dim(geometry)[2], ".",
              call. = FALSE
            )
          }
        }
        index <- id
      } else {
        stop("id should be numeric or character.", call. = FALSE)
      }

      idnames <- as.character(geometry[, index, drop = TRUE])
    } else {
      if (!is.vector(id)) {
        stop("id should be a vector (if length > 1).", call. = FALSE)
      }
      if (is.numeric(id)) {
        idnames <- as.character(id)
      } else if (is.factor(id)) {
        idnames <- as.character(id)
      } else if (is.character(id)) {
        idnames <- id
      } else {
        stop("id should be numeric or character.", call. = FALSE)
      }

      if (length(idnames) != dim(geometry)[1]) {
        stop("id must have a length equal to the number of locations.",
          call. = FALSE
        )
      }
    }

    if (sum(duplicated(idnames)) > 0) {
      stop("Duplicated names associated with id.", call. = FALSE)
    }
  }

  # Controls show_progress
  controls(args = show_progress, type = "boolean")

  # Project geometry in lon/lat
  geometry <- sf::st_transform(geometry, 4326)

  # Extract distances
  lonlat <- sfExtras::st_centroid_coords(geometry)
  lon <- as.numeric(lonlat[, 1])
  lat <- as.numeric(lonlat[, 2])

  if (show_progress) {
    pb <- utils::txtProgressBar(min = 0, max = dim(lonlat)[1], style = 3)
    distance <- NULL
    for (k in 1:dim(lonlat)[1]) {
      utils::setTxtProgressBar(pb, k)
      lonk <- lonlat[k, 1]
      latk <- lonlat[k, 2]
      distance <- rbind(distance, haversine(lonk, latk, lon, lat))
    }
    close(pb)
  } else {
    distance <- NULL
    for (k in 1:dim(lonlat)[1]) {
      lonk <- lonlat[k, 1]
      latk <- lonlat[k, 2]
      distance <- rbind(distance, haversine(lonk, latk, lon, lat))
    }
  }

  # Extract surface area
  surface <- as.numeric(sf::st_area(geometry)) / 1000000

  # Names
  if (!is.null(id)) {
    rownames(distance) <- idnames
    colnames(distance) <- idnames
    names(surface) <- idnames
  }

  # Return output
  res <- list(distance = distance, surface = surface)
  return(res)
}
