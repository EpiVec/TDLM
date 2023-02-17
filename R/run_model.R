#' Estimate mobility flows based on different trip distribution models
#'
#' This function estimates mobility flows using different distribution laws and
#' models. As described in \insertCite{Lenormand2016}{TDLM}, the function
#' uses a two-step approach to generate mobility flows by separating the trip
#' distribution law, gravity or intervening opportunities, from the modeling
#' approach used to generate the flows from this law.  This function only uses
#' the second step to generate mobility flow based on a matrix of probability
#' using different models.
#'
#' @param proba a squared matrix of probability. The sum of the matrix element
#' must be equal to 1. It will be normalized automatically if it is not the 
#' case.
#'
#' @param model a character indicating which model to use.
#'
#' @param nb_trips a numeric value indicating the total number of trips. Must 
#' be an integer if `multi = TRUE` (see Details).
#'
#' @param out_trips a numeric vector representing the number of outgoing
#' trips per location. Must be a vector of integers 
#' if `multi = TRUE` (see Details).
#'
#' @param in_trips a numeric vector representing the number of incoming
#' trips per location. Must be a vector of integers 
#' if `multi = TRUE` (see Details).
#'
#' @param multi a boolean indicating if the flows should be generated with 
#' random draws from a multinomial distribution (see Details).  
#'
#' @param nbrep an integer indicating the number of replications
#' associated to the model run. `nbrep = 1` if `multi = FALSE` (see Details).
#'
#' @param check_names a boolean indicating if the ID location are used as
#' vector names, matrix rownames and colnames and if they should be checked
#' (see Note).
#'
#' @details
#' \loadmathjax
#'
#' We propose four constrained models to generate the flow from the matrix
#' of probability. These models respect different level of
#' constraints. These constraints can preserve the total number of trips
#' (argument `nb_trips`) OR the number of out-going trips
#' \mjeqn{O_{i}}{O_{i}} (argument `out_trips`) AND/OR the number of in-coming
#' \mjeqn{D_{j}}{D_{j}} (argument `in_trips`) according to the model. The sum of
#' out-going trips \mjeqn{\sum_{i} O_{i}}{\sum_{i} O_{i}} should be equal to the
#' sum of in-coming trips \mjeqn{\sum_{j} D_{j}}{\sum_{j} D_{j}}.
#'
#' 1) Unconstrained model (`model = "UM"`). Only `nb_trips` will be preserved
#' (arguments `out_trips` and `in_trips` will not be used).
#' 2) Production constrained model (`model = "PCM"`). Only `out_trips` will be
#' preserved (arguments `nb_trips` and `in_trips` will not be used).
#' 3) Attraction constrained model (`model = "ACM"`). Only `in_trips` will be
#' preserved (arguments `nb_trips` and `out_trips` will not be used).
#' 4) Doubly constrained model (`model = "DCM"`). Both `out_trips` and
#' `in_trips` will be preserved (arguments `nb_trips`will not be used).
#' 
#' By default, when `multi = TRUE`, `nbrep` matrices will be generated from 
#' `proba` with multinomial random draws that will take different form according
#' to the model used. In this case, the models will deal with positive integers 
#' as inputs and outputs. Nevertheless, it is also possible to generate a unique 
#' average matrix (`nbrep = 1`) based on an infinite number of drawings. In this
#' case, the models' inputs can be either positive integer or real numbers. 
#'
#' @note All the inputs should be based on the same number of
#' locations sorted in the same order. It is recommended to use the location ID
#' as vector names, matrix rownames and matrix colnames and to set
#' `check_names = TRUE` to verify that everything is in order before running
#' this function (`check_names = FALSE` by default). Note that the function
#' [check_format_names()] can be used to control the validity of all the inputs
#' before running the main package's functions.
#'
#' @return
#' An object of class `TDLM`. A list of list of matrix containing for each
#' parameter value the `nbrep` simulated matrices . If `length(param) == 1` or
#' `law == "Rad"` or `law == "Unif` only a list of matrix will be returned.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [gof()] [run_law_model()] [run_law()] [check_format_names()]
#'
#' @examples
#' data(mass)
#' data(distance)
#' ind <- sample(dim(distance)[1], 100)
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' @export
run_model <- function(proba,
                      model = "UM",
                      nb_trips = 1000,
                      out_trips = NULL,
                      in_trips = out_trips,
                      multi = TRUE,
                      nbrep = 3,
                      check_names = FALSE) {
  # Set path to jar
  libpath <- .libPaths()[1]
  wdjar <- paste0(libpath, "/TDLM/java/")
  if (!dir.exists(wdjar)) {
    stop(paste0("Impossible to access ", wdjar, ". Please check that 
    the folder ", wdjar, " is accessible."), call. = FALSE)
  }
  if (!file.exists(paste0(wdjar, "TDLM.jar"))) {
    stop(paste0("It seems that an error occurred during the package 
    installation.\n", "The folder ", wdjar, "should contain three .jar files."),
      call. = FALSE
    )
  }

  # Controls
  controls(args = multi, type = "boolean")
  if(!multi){
    nbrep = 1
  }
  controls(args = nbrep, type = "strict_positive_integer")
  controls(args = check_names, type = "boolean")
  
  # Control PROBA
  controls(
    args = NULL,
    matrices = list(proba = proba),
    type = "matrices_proba"
  )
  proba = proba / sum(proba)

  # Controls MODEL
  models <- c("UM", "PCM", "ACM", "DCM")
  controls(args = model, type = "character")
  if (!(model %in% models)) {
    stop("Please choose check among the followings values:
UM, PCM, ACM or DCM",
         call. = FALSE
    )
  }
  if (model == "UM") {
    if(multi){
      controls(args = nb_trips, type = "strict_positive_integer")
    }else{
      controls(args = nb_trips, type = "strict_positive_numeric")
    }
  }
  if (model == "PCM") {
    if(multi){
      controls(
        args = NULL,
        vectors = list(out_trips = out_trips),
        type = "vectors_positive_integer"
      )
    }else{
      controls(
        args = NULL,
        vectors = list(out_trips = out_trips),
        type = "vectors_positive"
      )
    }
  }
  if (model == "ACM") {
    if(multi){
      controls(
        args = NULL,
        vectors = list(in_trips = in_trips),
        type = "vectors_positive_integer"
      )
    }else{
      controls(
        args = NULL,
        vectors = list(in_trips = in_trips),
        type = "vectors_positive"
      )
    }
  }
  if (model == "DCM") {
    if(multi){
      controls(
        args = NULL,
        vectors = list(
          out_trips = out_trips,
          in_trips = in_trips
        ),
        type = "vectors_positive_integer"
      )
    }else{
      controls(
        args = NULL,
        vectors = list(
          out_trips = out_trips,
          in_trips = in_trips
        ),
        type = "vectors_positive"
      )
    }
    if (sum(out_trips) != sum(in_trips)) {
      stop("Total number of out-going and in-coming trips must be equal.",
           call. = FALSE
      )
    }
  }
  # Check names
  if (check_names) {
    if (model == "UM") {
      controls(
        args = NULL,
        matrices = list(proba = proba),
        type = "matrices_checknames"
      )
    }
    if (model == "PCM") {
      controls(
        args = NULL,
        vectors = list(out_trips = out_trips),
        matrices = list(proba = proba),
        type = "vectors_matrices_checknames"
      )
    }
    if (model == "ACM") {
      controls(
        args = NULL,
        vectors = list(in_trips = in_trips),
        matrices = list(proba = proba),
        type = "vectors_matrices_checknames"
      )
    }
    if (model == "DCM") {
      controls(
        args = NULL,
        vectors = list(
          in_trips = in_trips,
          out_trips = out_trips
        ),
        matrices = list(proba = proba),
        type = "vectors_matrices_checknames"
      )
    }
  }

  # Create temp
  pathtemp <- paste0(
    wdjar, "temp_", round(as.numeric(as.POSIXct(Sys.time()))),
    "/"
  )
  dir.create(pathtemp, showWarnings = FALSE, recursive = TRUE)

  # Format and export data
  if (model == "UM") {
    out_trips <- rep(0, dim(proba)[1])
    out_trips[1] <- nb_trips
    mass <- cbind(out_trips, out_trips)
  }
  if (model == "PCM") {
    mass <- cbind(out_trips, out_trips)
  }
  if (model == "ACM") {
    mass <- cbind(in_trips, in_trips)
  }
  if (model == "DCM") {
    mass <- cbind(out_trips, in_trips)
  }
  readr::write_delim(as.data.frame(mass),
    paste0(pathtemp, "Mass.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE
  )
  readr::write_delim(as.data.frame(proba),
    paste0(pathtemp, "Proba.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE
  )

  # Run TDLM
  wdin <- pathtemp
  wdout <- pathtemp
  ismulti <- "true"
  if(!multi){
    ismulti <- "false"
  }

  outputs <- list()
  Args <- c("Model", "#Replications")
  Values <- c(model, nbrep)

  args <- paste0(wdin, " ", wdout, " ", model, " ", nbrep, " ", ismulti)

  cmd <- paste0("java -jar ", wdjar, "TDM.jar ", args)

  system(cmd)

  for (k in 1:nbrep) {
    mat <- readr::read_delim(paste0(pathtemp, "S_", k, ".csv"),
      delim = ";",
      col_name = TRUE,
      progress = FALSE,
      show_col_types = FALSE
    )
    mat <- as.matrix(mat)
    if (check_names) {
      rownames(mat) <- rownames(proba)
      colnames(mat) <- colnames(proba)
    } else {
      rownames(mat) <- NULL
      colnames(mat) <- NULL
    }
    outputs[[k]] <- mat
  }
  names(outputs) <- paste0("replication_", 1:nbrep)
  outputs$info <- data.frame(Argument = Args, Value = Values)

  # Delete temp
  unlink(pathtemp, recursive = TRUE)

  # Class TDLM
  outputs <- outputs[c(length(outputs), 1:(length(outputs) - 1))]
  class(outputs) <- append("TDLM", class(outputs))
  attr(outputs, "from") <- "run_model"

  # Return output
  return(outputs)
}
