#' Compute the number of opportunities between pairs of locations
#'
#' This function computes the number of opportunities between pairs of locations
#' as defined in \insertCite{Lenormand2016}{TDLM}.
#' For a given pair of location the number of opportunities between the location
#' of origin and the location of destination is based on the number of
#' opportunities in a circle of radius equal to the distance between origin and
#' destination centered in the origin. The number of opportunities at origin
#' and destination are not included.
#'
#' @param opportunity a numeric vector representing the number of opportunities
#' per location. The value should be positive.
#'
#' @param distance a squared matrix representing the distance between locations.
#'
#' @param check_names a boolean indicating if the ID location are used as
#' vector names, matrix rownames and colnames and if they should be checked
#' (see Note).
#'
#' @note `opportunity` and `distance` should be based on the same number of
#' locations sorted in the same order. It is recommended to use the location ID
#' as vector names, matrix rownames and matrix colnames and to set
#' `check_names = TRUE` to verify that everything is in order before running
#' this function (`check_names = FALSE` by default). Note that the function
#' [check_format_names()] can be used to control the validity of all the inputs
#' before running the main package's functions.
#'
#' @return
#' A squared matrix in which each element represents the number of opportunities
#' between a pair of locations.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [calib_param()] [extract_spatial_information()] 
#' [check_format_names()]
#'
#' @examples
#' data(mass)
#' data(distance)
#'
#' opportunity <- mass[, 1]
#'
#' sij <- extract_opportunities(
#'   opportunity = opportunity,
#'   distance = distance,
#'   check_names = FALSE
#' )
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' @export
extract_opportunities <- function(opportunity, distance, check_names = FALSE) {
  # Option (disabling scientific notation)
  options(scipen = 999)

  # Set path to jar
  libpath <- .libPaths()[1]
  wdjar <- paste0(libpath, "/TDLM/java/")
  if (!dir.exists(wdjar)) {
    stop(paste0("Impossible to access ", wdjar, ". Please check that 
    the folder ", wdjar, " is accessible."), call. = FALSE)
  }
  if (!file.exists(paste0(wdjar, "Sij.jar"))) {
    stop(paste0("It seems that an error occurred during the package 
    installation.\n", "The folder ", wdjar, "should contain three .jar files."),
      call. = FALSE
    )
  }

  # Controls
  controls(
    args = NULL,
    vectors = list(opportunity = opportunity),
    type = "vectors_positive"
  )
  controls(
    args = NULL,
    matrices = list(distance = distance),
    type = "matrices_positive"
  )
  controls(
    args = NULL,
    vectors = list(opportunity = opportunity),
    matrices = list(distance = distance),
    type = "vectors_matrices"
  )
  controls(args = check_names, type = "boolean")

  # Check names
  if (check_names) {
    controls(
      args = NULL,
      vectors = list(opportunity = opportunity),
      matrices = list(distance = distance),
      type = "vectors_matrices_checknames"
    )
  }

  # Create temp
  pathtemp <- paste0(
    wdjar, "temp_", round(as.numeric(as.POSIXct(Sys.time()))),
    "/"
  )
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
  args <- paste0(pathtemp, " ", pathtemp)

  cmd <- paste0("java -jar ", wdjar, "Sij.jar ", args)

  system(cmd)

  # Import JAR output
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
