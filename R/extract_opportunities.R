#' Compute the number of opportunities between pairs of locations
#'
#' This function computes the number of opportunities between pairs of locations
#' as defined in Lenormand \emph{et al.} (2016). For a given pair of locations, 
#' the number of opportunities between the origin location and the destination 
#' location is based on the number of opportunities within a circle of radius 
#' equal to the distance between the origin and the destination, with the 
#' origin location as the center. The number of opportunities at the origin 
#' and destination locations are not included.
#'
#' @param opportunity A `numeric` vector representing the number of opportunities
#' per location. The value should be positive.
#'
#' @param distance A squared `matrix` representing the distances between 
#' locations.
#'
#' @param check_names A `boolean` indicating whether the location IDs are used 
#' as `vector` names, `matrix` row names, and `matrix` column names, and 
#' whether they should be checked (see Note).
#' 
#' @return
#' A squared `matrix` in which each element represents the number of 
#' opportunities between a pair of locations.
#'
#' @note 
#' `opportunity` and `distance` should be based on the same number of
#' locations sorted in the same order. It is recommended to use the location ID
#' as `matrix` `rownames` and `matrix` `colnames` and to set
#' `check_names = TRUE` to verify that everything is consistent before running
#' this function (`check_names = FALSE` by default). Note that the function
#' [check_format_names()] can be used to validate all inputs
#' before running the main package's functions.
#' 
#' @references
#' Lenormand M, Bassolas A, Ramasco JJ (2016) Systematic comparison of trip 
#' distribution laws and models. \emph{Journal of Transport Geography} 51, 
#' 158-169.
#' 
#' @seealso 
#' Associated functions:
#' [extract_distances()], [extract_spatial_information()]. 
#' 
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(distance)
#'
#' opportunity <- mass[, 1]
#'
#' sij <- extract_opportunities(opportunity = opportunity,
#'                              distance = distance,
#'                              check_names = FALSE)
#'
#' @export
extract_opportunities <- function(opportunity, 
                                  distance, 
                                  check_names = FALSE) {
  
  # Option (disabling scientific notation)
  oldop <- options()
  on.exit(options(oldop))
  options(scipen = 999)

  # Set path to jar
  #libpath <- .libPaths()[1]
  #wdjar <- paste0(libpath, "/TDLM/java/")
  #if (!dir.exists(wdjar)) {
  #  stop(paste0("Impossible to access ", wdjar, ". Please check that 
  #  the folder ", wdjar, " is accessible."), call. = FALSE)
  #}
  #if (!file.exists(paste0(wdjar, "Sij.jar"))) {
  #  stop(paste0("It seems that an error occurred during the package 
  #  installation.\n", "The folder ", wdjar, "should contain three .jar files."),
  #    call. = FALSE
  #  )
  #}
  
  # Set path to jar
  allibpath <- .libPaths()
  nlib <- length(allibpath)
  testlib <- FALSE
  for(k in 1:nlib){
    libpath <- allibpath[k]
    wdjar <- paste0(libpath, "/TDLM/java/")
    if (dir.exists(wdjar)) {
      testlib <- TRUE
      break
    }  
  }
  if (!testlib) {
    stop("Impossible to access TDLM/java in any path(s) stored in .libPaths().", 
         call. = FALSE)
  }
  if (!file.exists(paste0(wdjar, "Sij.jar"))) {
    stop(paste0("It seems that an error occurred during the package ", 
                "installation.\n", 
                "The folder ", wdjar, "should contain the file Sij.jar."),
         call. = FALSE)
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
  pathtemp <- paste0(tempdir(),"/")

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
  if(!file.exists(paste0(pathtemp, "Sij.csv"))){
    stop(paste0("The TDLM package depends on Java. It seems that ",
                "Java did not run properly or did not produce the expected ",
                "outputs. Please ensure that Java is installed and working, ",
                "and open an issue at https://github.com/RTDLM/TDLM/issues if ",
                "the problem persists."),
         call. = FALSE)
  }
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
  #unlink(pathtemp, recursive = TRUE)

  # Return output
  return(sij)
}
