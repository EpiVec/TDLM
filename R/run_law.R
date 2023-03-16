#' Estimate mobility flows based on different trip distribution laws
#'
#' This function estimates mobility flows using different distribution laws.
#' As described in \insertCite{Lenormand2016;textual}{TDLM}, we
#' propose a two-step approach to generate mobility flows by separating the trip
#' distribution law, gravity or intervening opportunities, from the modeling
#' approach used to generate the flows from this law. This function only uses
#' the first step to generate a probability distribution based on the different
#' laws.
#'
#' @param law a character indicating which law to use (see Details).
#'
#' @param mass_origin a numeric vector representing the mass at origin (i.e.
#' demand).
#'
#' @param mass_destination a numeric vector representing the mass at
#' destination (i.e. attractiveness).
#'
#' @param distance a squared matrix representing the distance between locations
#' (see Details).
#'
#' @param opportunity a squared matrix representing the number of opportunities
#' between locations (see Details). Can be easily computed with 
#' [extract_opportunities()].
#'
#' @param param a vector of numeric value(s) used to adjust the importance of
#' `distance` or `opportunity` associated with the chosen law. A single value or
#' a vector of several parameter values can be used (see Return). Not necessary
#' for the original radiation law or the uniform law (see Details).
#'
#' @param check_names a boolean indicating if the ID location are used as
#' vector names, matrix rownames and colnames and if they should be checked
#' (see Note).
#'
#' @details
#' \loadmathjax
#'
#' We compute the matrix `proba` estimating the probability
#' \mjeqn{p_{ij}}{p_{ij}} to observe a trip from location \mjeqn{i}{i} to
#' another location \mjeqn{j}{j}
#' (\mjeqn{\sum_{i}\sum_{j} p_{ij}=1}{\sum_{i}\sum_{j} p_{ij}=1}). This
#' probability is based on the demand \mjeqn{m_{i}}{m_{i}}
#' (argument `mass_origin`) and the attractiveness
#' \mjeqn{m_{j}}{m_{j}} (argument `mass_destination`). Note that the population
#' is typically used as a surrogate for both quantities (this is why
#' `mass_destination = mass_origin` by default). It also depends on the
#' distance \mjeqn{d_{ij}}{d_{ij}} between locations (argument `distance`) OR
#' the number of opportunities \mjeqn{s_{ij}}{s_{ij}} between locations
#' (argument `opportunity`) depending on the chosen law. Both the effect of the
#' distance and the number of opportunities can be adjusted with a parameter
#' (argument `param`) except for the original radiation law or the uniform law.
#'
#' In this package we consider eight probabilistic laws
#' described in details in \insertCite{Lenormand2016;textual}{TDLM}. Four
#' gravity laws
#' \insertCite{Carey1858,Zipf1946,Barthelemy2011,Lenormand2016}{TDLM}, three
#' intervening opportunity laws
#' \insertCite{Schneider1959,Simini2012,Yang2014}{TDLM} and a uniform law.
#'
#' 1) Gravity law with an exponential distance decay function
#' (`law = "GravExp"`). The arguments `mass_origin`, `mass_destination`
#' (optional), `distance` and `param` will be used.
#' 2) Normalized gravity law with an exponential distance decay function
#' (`law = "NGravExp"`). The arguments `mass_origin`, `mass_destination`
#' (optional), `distance` and `param` will be used.
#' 3) Gravity law with a power distance decay function
#' (`law = "GravPow"`). The arguments `mass_origin`, `mass_destination`
#' (optional), `distance` and `param` will be used.
#' 4) Normalized gravity law with a power distance decay function
#' (`law = "NGravPow"`). The arguments `mass_origin`, `mass_destination`
#' (optional), `distance` and `param` will be used.
#' 5) Schneider's intervening opportunities law (`law = "Schneider"`). The
#' arguments `mass_origin`, `mass_destination` (optional), `opportunity` and
#' `param` will be used.
#' 6) Radiation law (`law = "Rad"`). The arguments `mass_origin`,
#' `mass_destination` (optional) and `opportunity` will be used.
#' 7) Extended radiation law (`law = "RadExt"`). The arguments `mass_origin`,
#' `mass_destination` (optional), `opportunity` and `param` will be used.
#' 8) Uniform law (`law = "Unif"`). The argument `mass_origin` will be used to
#' extract the number of locations.
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
#' An object of class `TDLM`. A list of list of matrices containing for each
#' parameter value the matrix of probabilities (called `proba`). If
#' `length(param) = 1` or `law = "Rad"` or `law = "Unif` only a list of
#' matrices will be returned.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [gof()] [run_law_model()] [run_model()] [extract_opportunities()] 
#' [check_format_names()]
#'
#' @examples
#' data(mass)
#' data(distance)
#'
#' mi <- as.numeric(mass[, 1])
#' mj <- mi
#'
#' res <- run_law(
#'   law = "GravExp", mass_origin = mi, mass_destination = mj,
#'   distance = distance, opportunity = NULL, param = 0.01,
#'   check_names = FALSE
#' )
#'
#' # print(res)
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' \insertRef{Carey1858}{TDLM}
#'
#' \insertRef{Zipf1946}{TDLM}
#'
#' \insertRef{Barthelemy2011}{TDLM}
#'
#' \insertRef{Schneider1959}{TDLM}
#'
#' \insertRef{Simini2012}{TDLM}
#'
#' \insertRef{Yang2014}{TDLM}
#'
#' @export
run_law <- function(law = "Unif",
                    mass_origin,
                    mass_destination = mass_origin,
                    distance = NULL,
                    opportunity = NULL,
                    param = NULL,
                    check_names = FALSE) {
  # Option (disabling scientific notation)
  options(scipen = 999)

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
  controls(args = check_names, type = "boolean")

  # Controls LAW
  if (law == "Unif") {
    law <- "Rand"
  }
  laws <- c(
    "GravExp", "NGravExp", "GravPow", "NGravPow", "Schneider", "Rad", "RadExt",
    "Rand"
  )
  dist_laws <- c("GravExp", "NGravExp", "GravPow", "NGravPow")
  oppo_laws <- c("Schneider", "Rad", "RadExt")
  rand_laws <- c("Rand")
  controls(args = law, type = "character")
  if (!(law %in% laws)) {
    stop("Please choose law among the followings values:
GravExp, NGravExp, GravPow, NGravPow, Schneider, Rad, ExtRad or Unif",
      call. = FALSE
    )
  }

  if ((law != "Rad") & (law != "Rand")) { # Param
    controls(args = param, type = "numeric_vector")
  }

  if ((law %in% dist_laws) | (law %in% oppo_laws)) {
    controls(
      args = NULL,
      vectors = list(
        mass_origin = mass_origin,
        mass_destination = mass_destination
      ),
      type = "vectors_positive"
    )
    if (law %in% dist_laws) {
      controls(
        args = NULL,
        matrices = list(distance = distance),
        type = "matrices_positive"
      )
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination
        ),
        matrices = list(distance = distance),
        type = "vectors_matrices"
      )
    }
    if (law %in% oppo_laws) {
      controls(
        args = NULL,
        matrices = list(opportunity = opportunity),
        type = "matrices_positive"
      )
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination
        ),
        matrices = list(opportunity = opportunity),
        type = "vectors_matrices"
      )
    }
  }
  if (law %in% rand_laws) {
    mass_destination <- mass_origin
    controls(
      args = NULL,
      vectors = list(
        mass_origin = mass_origin,
        mass_destination = mass_destination
      ),
      type = "vectors_positive"
    )
  }

  # Check names
  if (check_names) {
    if (law %in% dist_laws) {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination
        ),
        matrices = list(distance = distance),
        type = "vectors_matrices_checknames"
      )
    }
  }
  if (law %in% oppo_laws) {
    controls(
      args = NULL,
      vectors = list(
        mass_origin = mass_origin,
        mass_destination = mass_destination
      ),
      matrices = list(opportunity = opportunity),
      type = "vectors_matrices_checknames"
    )
  }
  if (law %in% rand_laws) {
    controls(
      args = NULL,
      vectors = list(
        mass_origin = mass_origin,
        mass_destination = mass_destination
      ),
      type = "vectors_checknames"
    )
  }

  # Create temp
  pathtemp <- paste0(
    wdjar, "temp_", round(as.numeric(as.POSIXct(Sys.time()))),
    "/"
  )
  dir.create(pathtemp, showWarnings = FALSE, recursive = TRUE)

  # Format and export data
  mass <- cbind(
    mass_origin, mass_destination,
    rep(1, length(mass_origin)),
    rep(1, length(mass_origin))
  )
  readr::write_delim(as.data.frame(mass),
    paste0(pathtemp, "Mass.csv"),
    delim = ";",
    col_name = TRUE,
    progress = FALSE
  )
  if (law %in% dist_laws) {
    readr::write_delim(as.data.frame(distance),
      paste0(pathtemp, "Distance.csv"),
      delim = ";",
      col_name = TRUE,
      progress = FALSE
    )
  }
  if (law %in% oppo_laws) {
    readr::write_delim(as.data.frame(opportunity),
      paste0(pathtemp, "Sij.csv"),
      delim = ";",
      col_name = TRUE,
      progress = FALSE
    )
  }

  # Run TDLM
  wdin <- pathtemp
  wdout <- pathtemp
  pij_only <- "true"
  model <- "UM"
  nbrep <- "1"
  pij_write <- "true"
  ismulti <- "true"
  maxiterDCM <- "50"
  minratioDCM <- "0.01"

  nbparam <- length(param)
  if ((law == "Rad") | (law == "Rand") | (nbparam == 1)) { # Param 1

    outputs <- list()
    if ((law == "Rad") | (law == "Rand")) {
      if (law == "Rand") {
        beta <- "0.01"
        Args <- c("Law")
        Values <- c("Unif")
      }
      if (law == "Rad") {
        beta <- "0.01"
        Args <- c("Law")
        Values <- c(law)
      }
    } else {
      beta <- param
      Args <- c("Law", "#Parameters", "Parameter")
      Values <- c(law, 1, param)
    }

    args <- paste0(
      wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
      model, " ", nbrep, " ", pij_write, " ", ismulti, " ",
      maxiterDCM, " ", minratioDCM
    )

    cmd <- paste0("java -jar ", wdjar, "TDLM.jar ", args)

    system(cmd)

    mat <- readr::read_delim(paste0(pathtemp, "pij.csv"),
      delim = ";",
      col_name = TRUE,
      progress = FALSE,
      show_col_types = FALSE
    )
    mat <- as.matrix(mat)
    if (check_names) {
      rownames(mat) <- names(mass_origin)
      colnames(mat) <- names(mass_origin)
    } else {
      rownames(mat) <- NULL
      colnames(mat) <- NULL
    }
    outputs$proba <- mat

    outputs$info <- data.frame(Argument = Args, Value = Values)
  } else { # Param > 1

    outputs <- list()
    Args <- c("Law", "#Parameters", paste0("Parameter ", 1:nbparam))
    Values <- c(law, nbparam, param)
    for (i in 1:nbparam) {
      beta <- param[i]
      outputs[[i]] <- list()

      args <- paste0(
        wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
        model, " ", nbrep, " ", pij_write, " ", ismulti, " ",
        maxiterDCM, " ", minratioDCM
      )

      cmd <- paste0("java -jar ", wdjar, "TDLM.jar ", args)

      system(cmd)

      mat <- readr::read_delim(paste0(pathtemp, "pij.csv"),
        delim = ";",
        col_name = TRUE,
        progress = FALSE,
        show_col_types = FALSE
      )
      mat <- as.matrix(mat)
      if (check_names) {
        rownames(mat) <- names(mass_origin)
        colnames(mat) <- names(mass_origin)
      } else {
        rownames(mat) <- NULL
        colnames(mat) <- NULL
      }
      outputs[[i]]$proba <- mat
    }
    names(outputs) <- paste0("parameter_", 1:nbparam)
    outputs$info <- data.frame(Argument = Args, Value = Values)
  }

  # Delete temp
  unlink(pathtemp, recursive = TRUE)

  # Class TDLM
  outputs <- outputs[c(length(outputs), 1:(length(outputs) - 1))]
  class(outputs) <- append("TDLM", class(outputs))
  attr(outputs, "from") <- "run_law"

  # Return output
  return(outputs)
}
