#' Estimate mobility flows based of different trip distribution laws and models
#'
#' This function estimates mobility flows using different distribution laws and
#' models. As described in \insertCite{Lenormand2016;textual}{TDLM}, the 
#' function uses a two-step approach to generate mobility flows by separating 
#' the trip distribution law, gravity or intervening opportunities, from the 
#' modeling approach used to generate the flows from this law.
#'
#' @param law a character indicating which law to use.
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
#' between locations (see Details).
#'
#' @param param a vector of numeric value(s) used to adjust the importance of
#' `distance` or `opportunity` associated with the chosen law. A single value or
#' a vector of several parameter values can be used (see Return). Not necessary
#' for the original radiation law or the uniform law (see Details).
#'
#' @param model a character indicating which model to use.
#'
#' @param nb_trips an integer indicating the total number of trips.
#'
#' @param out_trips a vector of integers representing the number of outgoing
#' trips per location.
#'
#' @param in_trips a vector of integers representing the number of incoming
#' trips per location.
#'
#' @param nbrep an integer indicating the number of replications
#' associated to the model run.
#'
#' @param write_proba a boolean indicating if the estimation of the
#' probability to move from one location to another obtained with law
#' should be returned along with the flows estimations.
#'
#' @param check_names a boolean indicating if the ID location are used as
#' vector names, matrix rownames and colnames and if they should be checked
#' (see Note).
#'
#' @details
#' \loadmathjax
#'
#' First, we compute the matrix `proba` estimating the probability
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
#' as described in \insertCite{Lenormand2016;textual}{TDLM}. Four gravity laws
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
#' 7) Extended radiation law (`law = "ExtRad"`). The arguments `mass_origin`,
#' `mass_destination` (optional), `opportunity` and `param` will be used.
#' 8) Uniform law (`law = "Unif"`). The argument `mass_origin` will be used to
#' extract the number of locations.
#'
#' Second, we propose four constrained models to generate the flow from these
#' distribution of probability. These models respect different level of
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
#' `in_trips` will be preserved (arguments `nb_trips`will not be used). The
#' doubly constrained model is based on an Iterative Proportional Fitting
#' process \insertCite{Deming1940}{TDLM}.
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
#' parameter value the `nbrep` simulated matrices end the matrix of probability
#' (called `proba`) if `write_proba = TRUE`. If `length(param) == 1` or
#' `law == "Rad"` or `law == "Unif` only a list of matrix will be returned.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [gof()] [run_law()] [run_model()] [check_format_names()]
#'
#' @examples
#' data(mass)
#' data(distance)
#' ind <- sample(dim(distance)[1], 100)
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
#' \insertRef{Deming1940}{TDLM}
#'
#' @export
run_law_model <- function(law = "NGravExp",
                          mass_origin,
                          mass_destination = mass_origin,
                          distance,
                          opportunity,
                          param,
                          model = "DCM",
                          nb_trips,
                          out_trips,
                          in_trips,
                          nbrep = 3,
                          write_proba = FALSE,
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

  # Controls LAW
  if (law == "Unif") {
    law <- "Rand"
  }
  laws <- c(
    "GravExp", "NGravExp", "GravPow", "NGravPow", "Schneider", "Rad", "ExtRad",
    "Rand"
  )
  dist_laws <- c("GravExp", "NGravExp", "GravPow", "NGravPow")
  oppo_laws <- c("Schneider", "Rad", "ExtRad")
  rand_laws <- c("Rand")
  controls(args = law, type = "character")
  if (!(law %in% laws)) {
    stop("Please choose check among the followings values:
GravExp, NGravEx, GravPow, NGravPow, Schneider, Rad, ExtRad or Unif",
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
    controls(args = nb_trips, type = "strict_positive_integer")
  }
  if (model == "PCM") {
    controls(
      args = NULL,
      vectors = list(out_trips = out_trips),
      type = "vectors_positive_integer"
    )
  }
  if (model == "ACM") {
    controls(
      args = NULL,
      vectors = list(in_trips = in_trips),
      type = "vectors_positive_integer"
    )
  }
  if (model == "DCM") {
    controls(
      args = NULL,
      vectors = list(
        out_trips = out_trips,
        in_trips = in_trips
      ),
      type = "vectors_positive_integer"
    )
    if (sum(out_trips) != sum(in_trips)) {
      stop("Total number of out-going and in-coming trips must be equal.",
        call. = FALSE
      )
    }
  }

  # Controls LAW and MODEL (and potential checknames)
  if (law %in% dist_laws) {
    if (model == "UM") {
      if (check_names) {
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
    if (model == "PCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips
        ),
        matrices = list(distance = distance),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips
          ),
          matrices = list(distance = distance),
          type = "vectors_matrices_checknames"
        )
      }
    }
    if (model == "ACM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          in_trips = in_trips
        ),
        matrices = list(distance = distance),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            in_trips = in_trips
          ),
          matrices = list(distance = distance),
          type = "vectors_matrices_checknames"
        )
      }
    }
    if (model == "DCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips,
          in_trips = in_trips
        ),
        matrices = list(distance = distance),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips,
            in_trips = in_trips
          ),
          matrices = list(distance = distance),
          type = "vectors_matrices_checknames"
        )
      }
    }
  }
  if (law %in% oppo_laws) {
    if (model == "UM") {
      if (check_names) {
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
    }
    if (model == "PCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips
        ),
        matrices = list(opportunity = opportunity),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips
          ),
          matrices = list(opportunity = opportunity),
          type = "vectors_matrices_checknames"
        )
      }
    }
    if (model == "ACM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          in_trips = in_trips
        ),
        matrices = list(opportunity = opportunity),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            in_trips = in_trips
          ),
          matrices = list(opportunity = opportunity),
          type = "vectors_matrices_checknames"
        )
      }
    }
    if (model == "DCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips,
          in_trips = in_trips
        ),
        matrices = list(opportunity = opportunity),
        type = "vectors_matrices"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips,
            in_trips = in_trips
          ),
          matrices = list(opportunity = opportunity),
          type = "vectors_matrices_checknames"
        )
      }
    }
  }
  if (law %in% rand_laws) {
    if (model == "UM") {
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination
          ),
          type = "vectors_checknames"
        )
      }
    }
    if (model == "PCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips
        ),
        type = "vectors_vectors"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips
          ),
          type = "vectors_checknames"
        )
      }
    }
    if (model == "ACM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          in_trips = in_trips
        ),
        type = "vectors_vectors"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            in_trips = in_trips
          ),
          type = "vectors_checknames"
        )
      }
    }
    if (model == "DCM") {
      controls(
        args = NULL,
        vectors = list(
          mass_origin = mass_origin,
          mass_destination = mass_destination,
          out_trips = out_trips,
          in_trips = in_trips
        ),
        type = "vectors_vectors"
      )
      if (check_names) {
        controls(
          args = NULL,
          vectors = list(
            mass_origin = mass_origin,
            mass_destination = mass_destination,
            out_trips = out_trips,
            in_trips = in_trips
          ),
          type = "vectors_checknames"
        )
      }
    }
  }

  # Controls other
  controls(args = nbrep, type = "strict_positive_integer")
  controls(args = write_proba, type = "boolean")
  controls(args = check_names, type = "boolean")

  # Create temp
  pathtemp <- paste0(
    wdjar, "temp_", round(as.numeric(as.POSIXct(Sys.time()))),
    "/"
  )
  dir.create(pathtemp, showWarnings = FALSE, recursive = TRUE)

  # Format and export data
  if (model == "UM") {
    out_trips <- rep(0, length(mass_origin))
    out_trips[1] <- nb_trips
    mass <- cbind(mass_origin, mass_destination, out_trips, out_trips)
  }
  if (model == "PCM") {
    mass <- cbind(mass_origin, mass_destination, out_trips, out_trips)
  }
  if (model == "ACM") {
    mass <- cbind(mass_origin, mass_destination, in_trips, in_trips)
  }
  if (model == "DCM") {
    mass <- cbind(mass_origin, mass_destination, out_trips, in_trips)
  }
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
  pij_only <- "false"
  pij_write <- write_proba

  nbparam <- length(param)
  if ((law == "Rad") | (law == "Rand") | (nbparam == 1)) { # Param 1

    outputs <- list()
    Args <- c("Law", "Model", "#Replications", "#Parameters", "Parameter")
    if ((law == "Rad") | (law == "Rand")) {
      if (law == "Rand") {
        beta <- "0.01"
        Values <- c("Unif", model, nbrep, 1, NA)
      }
      if (law == "Rad") {
        beta <- "0.01"
        Values <- c(law, model, nbrep, 1, NA)
      }
    } else {
      beta <- param
      Values <- c(law, model, nbrep, 1, param)
    }

    args <- paste0(
      wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
      model, " ", nbrep, " ", pij_write
    )

    cmd <- paste0("java -jar ", wdjar, "TDLM.jar ", args)

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
        rownames(mat) <- names(mass_origin)
        colnames(mat) <- names(mass_origin)
      } else {
        rownames(mat) <- NULL
        colnames(mat) <- NULL
      }
      outputs[[k]] <- mat
    }
    names(outputs) <- paste0("replication_", 1:nbrep)

    if (write_proba) {
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
    }

    outputs$info <- data.frame(Argument = Args, Value = Values)
  } else { # Param > 1

    outputs <- list()
    Args <- c("Law", "Model", "#Replications", "#Parameters", paste0("Parameter ", 1:nbparam))
    Values <- c(law, model, nbrep, nbparam, param)
    for (i in 1:nbparam) {
      beta <- param[i]
      outputs[[i]] <- list()

      args <- paste0(
        wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
        model, " ", nbrep, " ", pij_write
      )

      cmd <- paste0("java -jar ", wdjar, "TDLM.jar ", args)

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
          rownames(mat) <- names(mass_origin)
          colnames(mat) <- names(mass_origin)
        } else {
          rownames(mat) <- NULL
          colnames(mat) <- NULL
        }
        outputs[[i]][[k]] <- mat
      }
      names(outputs[[i]]) <- paste0("replication_", 1:nbrep)

      if (write_proba) {
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
    }
    names(outputs) <- paste0("parameter_", 1:nbparam)
    outputs$info <- data.frame(Argument = Args, Value = Values)
  }

  # Delete temp
  # unlink(pathtemp, recursive = TRUE)

  # Class TDLM
  outputs <- outputs[c(length(outputs), 1:(length(outputs) - 1))]
  class(outputs) <- append("TDLM", class(outputs))
  attr(outputs, "from") <- "run_law_model"
  attr(outputs, "proba") <- write_proba

  # Return output
  return(outputs)
}
