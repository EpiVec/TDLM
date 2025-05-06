#' Estimate mobility flows based on different trip distribution models
#'
#' This function estimates mobility flows using different distribution laws 
#' and models. As described in Lenormand \emph{et al.} (2016), the
#' function uses a two-step approach to generate mobility flows by separating
#' the trip distribution law (gravity or intervening opportunities) from the
#' modeling approach used to generate the flows based on this law. This function
#' only uses the second step to generate mobility flow based on a matrix of 
#' probabilities using different models.
#'
#' @param proba A squared `matrix` of probability. The sum of the matrix element
#' must be equal to 1. It will be normalized automatically if it is not the
#' case.
#'
#' @param model A `character` indicating which model to use.
#'
#' @param nb_trips A `numeric` value indicating the total number of trips. Must
#' be an `integer` if `average = FALSE` (see Details).
#'
#' @param out_trips A `numeric` vector representing the number of outgoing
#' trips per location. Must be a vector of integers if `average = FALSE` 
#' (see Details).
#'
#' @param in_trips A `numeric` vector representing the number of incoming
#' trips per location. Must be a vector of integers if `average = FALSE` 
#' (see Details).
#'
#' @param average A `boolean` indicating if the average mobility flow matrix 
#' should be generated instead of the `nbrep` matrices based on random draws 
#' (see Details).
#'
#' @param nbrep An `integer` indicating the number of replications
#' associated with the model run. Note that `nbrep = 1` if `average = TRUE`
#' (see Details).
#'
#' @param maxiter An `integer` indicating the maximal number of iterations for
#' adjusting the Doubly Constrained Model (see Details).
#'
#' @param mindiff A `numeric` strictly positive value indicating the
#' stopping criterion for adjusting the Doubly Constrained Model (see Details).
#'
#' @param check_names A `boolean` indicating whether the location IDs used as 
#' matrix rownames and colnames should be checked for consistency 
#' (see Note).
#' 
#' @return
#' An object of class `TDLM`. A `list` of matrices containing the `nbrep` 
#' simulated matrices.
#'
#' @details
#' We propose four constrained models to generate the flows from these
#' distribution of probability as described in Lenromand \emph{et al.} (2016). 
#' These models respect different level of constraints. These constraints can 
#' preserve the total number of trips (argument `nb_trips`) OR the number of 
#' out-going trips (argument `out_trips`) AND/OR the number of in-coming 
#' (argument `in_trips`) according to the model. The sum of out-going trips 
#' should be equal to the sum of in-coming trips.
#'
#' 1) Unconstrained model (`model = "UM"`). Only `nb_trips` will be preserved
#' (arguments `out_trips` and `in_trips` will not be used).
#' 2) Production constrained model (`model = "PCM"`). Only `out_trips` will be
#' preserved (arguments `nb_trips` and `in_trips` will not be used).
#' 3) Attraction constrained model (`model = "ACM"`). Only `in_trips` will be
#' preserved (arguments `nb_trips` and `out_trips` will not be used).
#' 4) Doubly constrained model (`model = "DCM"`). Both `out_trips` and
#' `in_trips` will be preserved (arguments `nb_trips`will not be used). The
#' doubly constrained model is based on an Iterative Proportional Fitting
#' process (Deming & Stephan, 1940). The arguments `maxiter` (50 by
#' default) and `mindiff` (0.01 by default) can be used to tune the model.
#' `mindiff` is the minimal tolerated relative error between the
#' simulated and observed marginals. `maxiter` ensures that the algorithm stops
#'  even if it has not converged toward the `mindiff` wanted value.
#'
#' By default, when `average = FALSE`, `nbrep` matrices are generated from
#' `proba` with multinomial random draws that will take different forms
#' according to the model used. In this case, the models will deal with positive
#' integers as inputs and outputs. Nevertheless, it is also possible to generate
#' an average matrix based on a multinomial distribution (based on an infinite
#' number of drawings). In this case, the models' inputs can be either positive
#' integer or real numbers and the output (`nbrep = 1` in this case) will be a
#' matrix of positive real numbers.
#'
#' @note 
#' All inputs should be based on the same number of
#' locations, sorted in the same order. It is recommended to use the location ID
#' as `matrix` `rownames` and `matrix` `colnames` and to set
#' `check_names = TRUE` to verify that everything is consistent before running
#' this function (`check_names = FALSE` by default). Note that the function
#' [check_format_names()] can be used to validate all inputs
#' before running the main package's functions.
#'
#' @references
#' Deming WE & Stephan FF (1940) On a Least Squares Adjustment of a Sample 
#' Frequency Table When the Expected Marginal Totals Are Known. \emph{Annals of 
#' Mathematical Statistics} 11, 427-444.
#' 
#' Lenormand M, Bassolas A, Ramasco JJ (2016) Systematic comparison of trip 
#' distribution laws and models. \emph{Journal of Transport Geography} 51, 
#' 158-169.
#'
#' @seealso 
#' For more details illustrated with a practical example, 
#' see the vignette: 
#' \url{https://rtdlm.github.io/TDLM/articles/TDLM.html#run-functions}.
#' 
#' Associated functions: 
#' [run_law_model()], [run_law()], [gof()]. 
#' 
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(od)
#'
#' proba <- od / sum(od)
#'
#' Oi <- as.numeric(mass[, 2])
#' Dj <- as.numeric(mass[, 3])
#'
#' res <- run_model(
#'   proba = proba,
#'   model = "DCM", nb_trips = NULL, out_trips = Oi, in_trips = Dj,
#'   average = FALSE, nbrep = 3, maxiter = 50, mindiff = 0.01,
#'   check_names = FALSE
#' )
#'
#' # print(res)
#'
#' @export
run_model <- function(proba,
                      model = "UM",
                      nb_trips = 1000,
                      out_trips = NULL,
                      in_trips = out_trips,
                      average = FALSE,
                      nbrep = 3,
                      maxiter = 50,
                      mindiff = 0.01,
                      check_names = FALSE) {
  
  # Option (disabling scientific notation)
  oldop <- options()
  on.exit(options(oldop))
  options(scipen = 999)

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
  if (!file.exists(paste0(wdjar, "TDM.jar"))) {
    stop(paste0("It seems that an error occurred during the package 
    installation.\n", "The folder ", wdjar, "should contain the file TDM.jar."),
         call. = FALSE
    )
  }

  # Controls
  controls(args = average, type = "boolean")
  if (average) {
    nbrep <- 1
  }
  controls(args = nbrep, type = "strict_positive_integer")
  controls(args = check_names, type = "boolean")

  # Control PROBA
  controls(
    args = NULL,
    matrices = list(proba = proba),
    type = "matrices_positive"
  )
  proba <- proba / sum(proba)

  # Controls MODEL
  models <- c("UM", "PCM", "ACM", "DCM")
  controls(args = model, type = "character")
  if (!(model %in% models)) {
    stop(paste0("Please choose model from the following:\n",
                "UM, PCM, ACM or DCM."),
         call. = FALSE)
  }

  if (model == "DCM") {
    controls(args = maxiter, type = "strict_positive_integer")
    controls(args = mindiff, type = "strict_positive_numeric")
  } else {
    maxiter <- "50"
    mindiff <- "0.01"
  }

  if (model == "UM") {
    if (!average) {
      controls(args = nb_trips, type = "strict_positive_integer")
    } else {
      controls(args = nb_trips, type = "strict_positive_numeric")
    }
  }
  if (model == "PCM") {
    if (!average) {
      controls(
        args = NULL,
        vectors = list(out_trips = out_trips),
        type = "vectors_positive_integer"
      )
    } else {
      controls(
        args = NULL,
        vectors = list(out_trips = out_trips),
        type = "vectors_positive"
      )
    }
  }
  if (model == "ACM") {
    if (!average) {
      controls(
        args = NULL,
        vectors = list(in_trips = in_trips),
        type = "vectors_positive_integer"
      )
    } else {
      controls(
        args = NULL,
        vectors = list(in_trips = in_trips),
        type = "vectors_positive"
      )
    }
  }
  if (model == "DCM") {
    if (!average) {
      controls(
        args = NULL,
        vectors = list(
          out_trips = out_trips,
          in_trips = in_trips
        ),
        type = "vectors_positive_integer"
      )
    } else {
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
  pathtemp <- paste0(tempdir(),"/")

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
  multi <- "true"
  if (average) {
    multi <- "false"
  }
  maxiterDCM <- maxiter
  minratioDCM <- mindiff

  outputs <- list()
  Args <- c("Model", "#Replications")
  if (average) {
    Values <- c(model, paste0(nbrep, " (average)"))
  } else {
    Values <- c(model, nbrep)
  }

  args <- paste0(
    wdin, " ", wdout, " ", model, " ", nbrep, " ", multi, " ",
    maxiterDCM, " ", minratioDCM
  )

  cmd <- paste0("java -jar ", wdjar, "TDM.jar ", args)

  system(cmd)
  
  if(!file.exists(paste0(pathtemp, "S_1.csv"))){
    stop(paste0("The TDLM package depends on Java. It seems that ",
                "Java did not run properly or did not produce the expected ",
                "outputs. Please ensure that Java is installed and working, ",
                "and open an issue at https://github.com/RTDLM/TDLM/issues if ",
                "the problem persists."),
         call. = FALSE)
  }

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
  #unlink(pathtemp, recursive = TRUE)

  # Class TDLM
  outputs <- outputs[c(length(outputs), 1:(length(outputs) - 1))]
  class(outputs) <- append("TDLM", class(outputs))
  attr(outputs, "from") <- "run_model"

  # Return output
  return(outputs)
}
