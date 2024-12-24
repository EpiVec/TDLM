#' Estimate mobility flows based on different trip distribution laws and models
#'
#' This function estimates mobility flows using different distribution laws and
#' models. As described in Lenormand \emph{et al.} (2016), the
#' function uses a two-step approach to generate mobility flows by separating
#' the trip distribution law (gravity or intervening opportunities) from the
#' modeling approach used to generate the flows based on this law.
#'
#' @param law A `character` indicating which law to use (see Details).
#'
#' @param mass_origin A `numeric` vector representing the mass at the origin (i.e.
#' demand).
#'
#' @param mass_destination A `numeric` vector representing the mass at
#' the destination (i.e. attractiveness).
#'
#' @param distance A squared `matrix` representing the distance between locations
#' (see Details).
#'
#' @param opportunity A squared `matrix` representing the number of opportunities
#' between locations (see Details). Can be easily computed with 
#' [extract_opportunities()].
#'
#' @param param A `numeric` vector or a single `numeric` value used to adjust 
#' the importance of `distance` or `opportunity` associated with the chosen law.
#' Not necessary for the original radiation law or the uniform law (see 
#' Details).
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
#' @param write_proba A `boolean` indicating if the estimation of the
#' probability to move from one location to another obtained with the
#' distribution law should be returned along with the flow estimations.
#'
#' @param check_names A `boolean` indicating whether the location IDs used as 
#' matrix rownames and colnames should be checked for consistency 
#' (see Note).
#' 
#' @return
#' An object of class `TDLM`. A `list` of `list` of matrices containing for each
#' parameter value the `nbrep` simulated matrices and the matrix of
#' probabilities (called `proba`) if `write_proba = TRUE`. If 
#' `length(param) = 1` or `law = "Rad"` or `law = "Unif"` only a list of
#' matrices will be returned.
#'
#' @details
#' First, we compute the matrix `proba` estimating the probability to observe a
#' trip from one location to another. This probability is based on the demand 
#' (argument `mass_origin`) and the attractiveness (argument 
#' `mass_destination`). Note that the population is typically used as a 
#' surrogate for both quantities (this is why `mass_destination = mass_origin` 
#' by default). It also depends on the distance between locations 
#' (argument `distance`) OR the number of opportunities between locations
#' (argument `opportunity`) depending on the chosen law. Both the effect of the
#' distance and the number of opportunities can be adjusted with a parameter
#' (argument `param`) except for the original radiation law and the uniform law.
#'
#' In this package we consider eight probabilistic laws described in details in 
#' Lenormand \emph{et al.} (2016). Four
#' gravity laws (Barthelemy, 2011), three
#' intervening opportunity laws (Schneider, 1959; Simini \emph{et al.}, 2012; 
#' Yang \emph{et al.}, 2014) and a uniform law.
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
#' Second, we propose four constrained models to generate the flows from these
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
#' Barthelemy M (2011). Spatial Networks. \emph{Physics Reports} 499, 1-101.
#' 
#' Deming WE & Stephan FF (1940) On a Least Squares Adjustment of a Sample 
#' Frequency Table When the Expected Marginal Totals Are Known. \emph{Annals of 
#' Mathematical Statistics} 11, 427-444.
#' 
#' Lenormand M, Bassolas A, Ramasco JJ (2016) Systematic comparison of trip 
#' distribution laws and models. \emph{Journal of Transport Geography} 51, 
#' 158-169.
#' 
#' Schneider M (1959) Gravity models and trip distribution theory. \emph{Papers 
#' of the regional science association} 5, 51-58.
#'  
#' Simini F, González MC, Maritan A & Barabási A (2012) A universal model for 
#' mobility and migration patterns. \emph{Nature} 484, 96-100. 
#' 
#' Yang Y, Herrera C, Eagle N & González MC (2014) Limits of Predictability in 
#' Commuting Flows in the Absence of Data for Calibration. \emph{Scientific 
#' Reports} 4, 5662.
#'
#' @seealso 
#' For more details illustrated with a practical example, 
#' see the vignette: 
#' \url{https://epivec.github.io/TDLM/articles/TDLM.html#run-functions}.
#' 
#' Associated functions: 
#' [run_law()], [run_model()], [gof()]. 
#' 
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(distance)
#'
#' mi <- as.numeric(mass[, 1])
#' mj <- mi
#' Oi <- as.numeric(mass[, 2])
#' Dj <- as.numeric(mass[, 3])
#'
#' res <- run_law_model(law = "GravExp",
#'                      mass_origin = mi, 
#'                      mass_destination = mj,
#'                      distance = distance, 
#'                      opportunity = NULL, 
#'                      param = 0.01,
#'                      model = "DCM", 
#'                      nb_trips = NULL,
#'                      out_trips = Oi, 
#'                      in_trips = Dj,
#'                      average = FALSE, 
#'                      nbrep = 3, 
#'                      maxiter = 50, 
#'                      mindiff = 0.01,
#'                      write_proba = FALSE,
#'                      check_names = FALSE)
#'
#' print(res)
#'
#' @export
run_law_model <- function(law = "Unif",
                          mass_origin,
                          mass_destination = mass_origin,
                          distance = NULL,
                          opportunity = NULL,
                          param = NULL,
                          model = "UM",
                          nb_trips = 1000,
                          out_trips = NULL,
                          in_trips = out_trips,
                          average = FALSE,
                          nbrep = 3,
                          maxiter = 50,
                          mindiff = 0.01,
                          write_proba = FALSE,
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
  if (!file.exists(paste0(wdjar, "TDLM.jar"))) {
    stop(paste0("It seems that an error occurred during the package 
    installation.\n", "The folder ", wdjar, "should contain the file TDLM.jar."),
      call. = FALSE
    )
  }

  # Controls
  controls(args = average, type = "boolean")
  if (average) {
    nbrep <- 1
  }
  controls(args = nbrep, type = "strict_positive_integer")
  controls(args = write_proba, type = "boolean")
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
    stop(paste0("Please choose law from the following:\n",
                "GravExp, NGravExp, GravPow, NGravPow, Schneider, ",
                "Rad, RadExt or Unif."),
      call. = FALSE)
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
    controls(args = maxiter, type = "strict_positive_integer")
    controls(args = mindiff, type = "strict_positive_numeric")
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

  # Create temp
  pathtemp <- paste0(tempdir(),"/")

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
  pij_write <- "false"
  if (write_proba) {
    pij_write <- "true"
  }
  multi <- "true"
  if (average) {
    multi <- "false"
  }
  maxiterDCM <- maxiter
  minratioDCM <- mindiff

  nbparam <- length(param)
  if ((law == "Rad") | (law == "Rand") | (nbparam == 1)) { # Param 1

    outputs <- list()
    if ((law == "Rad") | (law == "Rand")) {
      if (law == "Rand") {
        beta <- "0.01"
        Args <- c("Law", "Model", "#Replications")
        if (average) {
          Values <- c("Unif", model, paste0(nbrep, " (average)"))
        } else {
          Values <- c("Unif", model, nbrep)
        }
      }
      if (law == "Rad") {
        beta <- "0.01"
        Args <- c("Law", "Model", "#Replications")
        Values <- c(law, model, nbrep)
        if (average) {
          Values <- c(law, model, paste0(nbrep, " (average)"))
        } else {
          Values <- c(law, model, nbrep)
        }
      }
    } else {
      beta <- param
      Args <- c("Law", "Model", "#Replications", "#Parameters", "Parameter")
      if (average) {
        Values <- c(law, model, paste0(nbrep, " (average)"), 1, param)
      } else {
        Values <- c(law, model, nbrep, 1, param)
      }
    }

    args <- paste0(
      wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
      model, " ", nbrep, " ", pij_write, " ", multi, " ",
      maxiterDCM, " ", minratioDCM
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
    Args <- c(
      "Law", "Model", "#Replications", "#Parameters",
      paste0("Parameter ", 1:nbparam)
    )
    if (average) {
      Values <- c(law, model, paste0(nbrep, " (average)"), nbparam, param)
    } else {
      Values <- c(law, model, nbrep, nbparam, param)
    }
    for (i in 1:nbparam) {
      beta <- param[i]
      outputs[[i]] <- list()

      args <- paste0(
        wdin, " ", wdout, " ", law, " ", beta, " ", pij_only, " ",
        model, " ", nbrep, " ", pij_write, " ", multi, " ",
        maxiterDCM, " ", minratioDCM
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
  #unlink(pathtemp, recursive = TRUE)

  # Class TDLM
  outputs <- outputs[c(length(outputs), 1:(length(outputs) - 1))]
  class(outputs) <- append("TDLM", class(outputs))
  attr(outputs, "from") <- "run_law_model"
  attr(outputs, "proba") <- write_proba

  # Return output
  return(outputs)
}
