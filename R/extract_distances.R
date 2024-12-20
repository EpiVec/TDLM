#' Compute the distance between pairs of locations
#'
#' This function computes the distance between pairs of locations based on 
#' geographical coordinates.
#'
#' @param coords A two-column `matrix` or `data.frame` where each row represents 
#' the coordinates of a location (see Details).
#' 
#' @param method A `character` string indicating which method to choose to 
#' compute the distances (see Details). Available options are `"Haversine"` or 
#' `"Euclidean"`.
#' 
#' @param id A vector with length equal to the number of locations, used as 
#' row names and column names for the output distance matrix (optional, `NULL` 
#' by default). 
#' 
#' @param show_progress A boolean indicating whether a progress bar should be
#' displayed.
#' 
#' @return
#' A square matrix representing the distance (in kilometers) between locations.
#'
#' @details 
#' `coords` must contain two columns: the first one for the longitude 
#' or "X" coordinates, and the second one for the latitude or "Y" coordinates. 
#' The `"Haversine"` method is used to compute great-circle distances from 
#' longitude/latitude, while the `"Euclidean"` method should be used for "X/Y" 
#' coordinates.
#' 
#' @note 
#' The outputs are based on the locations contained in `coords`, sorted 
#' in the same order. An optional `id` can also be provided to be used as 
#' names for the outputs.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso 
#'  Associated functions:  
#' [extract_opportunities()][extract_spatial_information()] 
#' 
#' @examples
#' data(coords)
#'
#' distance <- extract_distances(coords = coords, 
#'                               method = "Haversine",
#'                               id = rownames(coords))
#'
#' @export
extract_distances <- function(coords, 
                              method = "Haversine", 
                              id = NULL,
                              show_progress = FALSE) {
  
  # Option (disabling scientific notation)
  oldop <- options()
  on.exit(options(oldop))
  options(scipen = 999)

  # Controls coords
  if (!is.data.frame(coords) & !is.matrix(coords)) {
    stop("coords must be a matrix or a data.frame.",
         call. = FALSE)
  }
  if (dim(coords)[2] != 2) {
    stop("coords must be a data.frame with two columns.",
         call. = FALSE)
  }
  nbna <- sum(is.na(coords))
  if (nbna > 0) {
    stop("NA(s) detected in coords.", 
         call. = FALSE)
  }
  if (!is.numeric(coords[,1]) | !is.numeric(coords[,2])) {
    stop("coords must have only numeric values.",
    call. = FALSE)
  }
  
  # Controls method
  controls(args = method, type = "character")
  if (!(method %in% c("Haversine", "Euclidean"))) {
    stop(paste0("Please choose method from the following:\n",
                "Haversine or Euclidean."),
         call. = FALSE)
  }
  
  # Controls id
  if (!is.null(id)) {
    if (is.numeric(id)) {
      idnames <- as.character(id)
    } else if (is.factor(id)) {
      idnames <- as.character(id)
    } else if (is.character(id)) {
      idnames <- id
    } else {
      stop("id should be numeric or character.", 
           call. = FALSE)
    }
    if (length(idnames) != dim(coords)[1]) {
      stop("id must have a length equal to the number of locations.",
           call. = FALSE
      )
    }
    if (sum(duplicated(idnames)) > 0) {
      stop("Duplicated names associated with id.", 
           call. = FALSE)
    }
  }
  
  # Controls show_progress
  controls(args = show_progress, type = "boolean")
  
  # Compute distance
  coords <- as.matrix(coords)
  if(method == "Haversine"){
    if (show_progress) {
      pb <- utils::txtProgressBar(min = 0, max = dim(coords)[1], style = 3)
      distance <- lapply(1:dim(coords)[1], function(k){
        utils::setTxtProgressBar(pb, k)
        haversine(coords[k, 1], coords[k, 2], coords[,1], coords[,2])
      })
      distance <- do.call(rbind, distance)
      close(pb)
    } else {
      distance <- lapply(1:dim(coords)[1], function(k){
        haversine(coords[k, 1], coords[k, 2], coords[,1], coords[,2])
      })
      distance <- do.call(rbind, distance)
    }
  }
  
  if(method == "Euclidean"){
    if (show_progress) {
      pb <- utils::txtProgressBar(min = 0, max = dim(coords)[1], style = 3)
      distance <- as.matrix(stats::dist(coords, diag = TRUE, upper = TRUE))
      close(pb)
    } else {
      distance <- as.matrix(stats::dist(coords, diag = TRUE, upper = TRUE))
    }
    distance <- distance/1000
  }
  
  # Names
  if (!is.null(id)) {
    rownames(distance) <- idnames
    colnames(distance) <- idnames
  }
  
  # Return output
  return(distance)

}
