#' Compute the number of opportunities between pairs of locations
#'
#' This function compute the number of opportunities between pairs of locations
#' as defined in \insertCite{Lenormand2016}{TDLM}.
#' For a given pair of location the number of opportunities between the location
#' of origin and the location of destination is based on the number of
#' opportunities in a circle of radius distance between origin and destination
#' centered in the origin (excluding the origin and destination).
#'
#' @param opportunity a numeric vector the number of opportunites per
#' location. The value should be positive.
#'
#' @param distance a squared matrix representing the distance between locations.
#'
#' @param check_names a boolean indicating if the ID location are used as vector
#' names, matrix rownames and colnames and if they should be checked
#' (see Details).
#'
#' @details `opportunity` and `distance` should be based on the same number of
#' locations sorted in the same order. It is recommended to use the location ID
#' as vector names, matrix rownames and matrix colnames and to set
#' `check_names = TRUE` to check that everything is ok (`check_names = FALSE` by
#' default).
#'
#' @return
#' A squared matrix in which each element represents the number of opportunites
#' between a pair of locations.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(distance)
#' # sij=extract_opportunities(opportunity=as.numeric(mass[,1]),
#' # distance=distance,
#' # check_names=FALSE)
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' @export
extract_opportunities <- function(opportunity, distance, check_names = FALSE) {

  # Set path to jar
  tdlmdir <- list.dirs(.libPaths(), recursive = FALSE)
  tdlmpath <- tdlmdir[grep("TDLM", tdlmdir)]
  wdjar <- paste0(tdlmpath, "/java/jar/")
  if (!file.exists(wdjar)) {
    stop(paste0("Impossible to access ", wdjar, ". Please check that 
                the folder ", wdjar, "is accessible."), call. = FALSE)
  }

  # Controls
  controls(args = NULL, vector = opportunity, type = "vector_positive")
  controls(args = NULL, matrix = distance, type = "matrix")
  controls(
    args = NULL, vector = opportunity, matrix = distance,
    type = "vector_matrix"
  )
  controls(args = check_names, type = "boolean")

  # Check names
  if (check_names) {

  }

  # Create temp
  pathtemp <- paste0(wdjar, "temp/")
  if (file.exists(pathtemp)) {
    unlink(pathtemp, recursive = TRUE)
  }
  dir.create(pathtemp, showWarnings = FALSE, recursive = TRUE)

  # Format data
  mass <- cbind(opportunity, opportunity, opportunity, opportunity)

  # Export data
  readr::write_delim(as.data.frame(mass),
    paste0(pathtemp, "Mass.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE
  )
  readr::write_delim(as.data.frame(distance),
    paste0(pathtemp, "Distance.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE
  )

  # Run Sij
  param <- paste0(pathtemp, " ", pathtemp)

  cmd <- paste0("java -jar ", wdjar, "Sij.jar ", param)

  system(cmd)

  # Import data
  sij <- readr::read_delim(paste0(pathtemp, "Sij.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE,
    show_col_types = FALSE
  )
  sij <- as.matrix(sij)
  if (check_names) {
    rownames(sij) <- rownames(distance)
    colnames(sij) <- colnames(distance)
  } else {
    rownames(sij) <- NULL
    colnames(sij) <- NULL
  }

  # Delete temp
  unlink(pathtemp, recursive = TRUE)

  # Return output
  return(sij)
}
