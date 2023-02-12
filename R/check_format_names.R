#' Check format of TDLM's inputs
#'
#' This function checks that the TDLM's inputs have the required format.
#'
#' @param vectors a list of vectors. The list can contain one vector. It is
#' recommended to name each element of the list. If `vectors = NULL` only the
#' matrices will be considered.
#'
#' @param matrices a list of matrices. The list can contain one matrix. It is
#' recommended to name each element of the list. If `matrices = NULL` only the
#' vectors will be considered.
#'
#' @param check a character indicating what types of check
#' ("format" or "format_and_names") should be used (see Details).
#'
#' @details The `TDLM`'s inputs should be based on the same number of
#' locations sorted in the same order. `check = "format"` will run basic check
#' to ensure that the structure of the inputs (dimensions, class, type...) is
#' correct. It is recommended to use the location ID
#' as vector names, matrix rownames and matrix colnames, set
#' `check  = "format_and_names"` to check the names (`check =
#' "format_and_names"` by default). The checks are run successively so run the
#' function as many times as needed to get the message indicating that the
#' inputs passed the check successfully.
#'
#' @return
#' A message indicating if the check has passed or failed.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(distance)
#'
#' @export
check_format_names <- function(vectors, matrices, check = "format_and_names") {
  
  # Controls
  if(is.null(vectors) & is.null(matrices)){
    stop("At least one of the vectors or matrices argument should be non-null.")  
  }
  if (!is.null(vectors)) {
    controls(args = vectors, type = "list")
    if (is.null(names(vectors))) {
      names(vectors) <- paste0("Vector ", 1:length(vectors))
      message(paste0(
        "No names identified in the vectors list.\n",
        "Names have been automatically assigned.\n"
      ))
    }
  }  
  if (!is.null(matrices)) {
    controls(args = matrices, type = "list")
    if (is.null(names(matrices))) {
      names(matrices) <- paste0("Matrix ", 1:length(matrices))
      message(paste0(
        "No names identified in the matrices list.\n",
        "Names have been automatically assigned.\n"
      ))
    }
  }
  controls(args = check, type = "character")
  if (!(check %in% c("format", "format_and_names"))) {
    stop("Please choose check among the followings values:
format or format_and_names", call. = FALSE)
  }

  # Format
  if (!is.null(vectors) & is.null(matrices)) {
    controls(
      args = NULL,
      vectors = vectors,
      type = "vectors_positive"
    )
    controls(
      args = NULL,
      vectors = vectors,
      type = "vectors_vectors"
    )
  }  

  if (is.null(vectors) & !is.null(matrices)) {
    controls(
      args = NULL,
      vectors = vectors,
      matrices = matrices,
      type = "matrices_matrices"
    )
  } 
  
  if (is.null(vectors) & is.null(matrices)) {
    controls(
      args = NULL,
      vectors = vectors,
      type = "vectors_positive"
    )
    controls(
      args = NULL,
      matrices = matrices,
      type = "matrices_positive"
    )
    controls(
      args = NULL,
      vectors = vectors,
      matrices = matrices,
      type = "vectors_matrices"
    )
  }
    
  # Names
  if (check == "format_and_names") {
    if (!is.null(matrices)) {
      controls(
        args = NULL,
        vectors = vectors,
        matrices = matrices,
        type = "vectors_matrices_checknames"
      )
    } else {
      controls(
        args = NULL,
        vectors = vectors,
        matrices = matrices,
        type = "vectors_checknames"
      )
    }
  }

  # Return
  message(paste0("The inputs passed the ", check, " check successfully!"))
}
