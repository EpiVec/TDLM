#' Compute goodness-of-fit measures between observed and simulated OD matrices
#'
#' This function returns a `data.frame` where each row provides one or
#' several goodness-of-fit measures between a simulated and an observed
#' Origin-Destination (OD) matrix.
#'
#' @param sim An object of class `TDLM` (output of [run_law_model()],
#' [run_law()], or [run_model()]). A matrix or a list of matrices can also be 
#' used (see Note).
#'
#' @param obs A square `matrix` representing the observed mobility flows.
#'
#' @param measures A `character` vector or a single `character` string 
#' indicating which goodness-of-fit measure(s) to compute (see Details). 
#' Available options are `"CPC"`, `"NRMSE"`, `"KL"`, `"CPL"`, `"CPC_d"` and 
#' `"KS"`. If `"all"` is specified, all measures will be calculated.
#'
#' @param distance A square `matrix` representing the distances between 
#' locations. This is only necessary for distance-based measures.
#'
#' @param use_proba A `boolean` indicating whether the `proba` matrix should be 
#' used instead of the simulated OD matrix to compute the measure(s). This is 
#' only valid for output from [run_law_model()] with the argument 
#' `write_proba = TRUE` (see Note).
#'
#' @param check_names A `boolean` indicating whether the location IDs used as 
#' matrix rownames and colnames should be checked for consistency 
#' (see Note).
#'
#' @param bin_size A `numeric` value indicating the size of bins used to 
#' discretize the distance distribution when computing CPC_d (default is 2 
#' kilometers).
#' 
#' @return
#' A `data.frame` providing one or several goodness-of-fit measures between
#' simulated OD(s) and an observed OD. Each row corresponds to a matrix sorted
#' according to the list (or list of lists) elements (names are used if
#' provided).
#'
#' @details
#' Several goodness-of-fit measures are considered, such as the Common Part
#' of Commuters (CPC), the Common Part of Links (CPL), and the Common Part of 
#' Commuters based on the distance (CPC_d), as described in [Lenormand 
#' \emph{et al.} (2016)](http://arxiv.org/abs/1506.04889). It also includes 
#' classical metrics such as the 
#' [Normalized Root Mean Square Error](https://rtdlm.github.io/TDLM/articles/TDLM.html#normalized-root-mean-square-error-nrmse) 
#' (NRMSE), the 
#' [Kullback–Leibler divergence](https://rtdlm.github.io/TDLM/articles/TDLM.html#kullbackleibler-divergence-ks) 
#' (KL), and the Kolmogorov-Smirnov statistic and 
#' p-value (KS). These measures are based on the observed and simulated flow 
#' distance distributions and are computed using the [ks_test][Ecume::ks_test] 
#' function from the [Ecume](https://cran.r-project.org/package=Ecume) package.
#'
#' @note
#' By default, if `sim` is an output of [run_law_model()],
#' the measure(s) are computed only for the simulated OD matrices and
#' not for the `proba` matrix (included in the output when
#' `write_proba = TRUE`). The argument `use_proba` can be used to compute the
#' measure(s) based on the `proba` matrix instead of the simulated
#' OD matrix. In this case, the argument `obs` should also be a `proba` matrix.
#'
#' All inputs should be based on the same number of
#' locations, sorted in the same order. It is recommended to use the location ID
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
#' For more details illustrated with a practical example, 
#' see the vignette: 
#' \url{https://rtdlm.github.io/TDLM/articles/TDLM.html#goodness-of-fit-measures}.
#' 
#' Associated functions: 
#' [run_law()], [run_model()], [run_law_model()].
#' 
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(mass)
#' data(distance)
#' data(od)
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
#'                      nbrep = 1, 
#'                      maxiter = 50, 
#'                      mindiff = 0.01,
#'                      write_proba = FALSE,
#'                      check_names = FALSE)
#'
#' gof(sim = res, 
#'     obs = od, 
#'     measures = "CPC", 
#'     distance = NULL, 
#'     bin_size = 2,
#'     use_proba = FALSE,
#'     check_names = FALSE)
#'
#' @export
gof <- function(sim, 
                obs, 
                measures = "all", 
                distance = NULL, 
                bin_size = 2,
                use_proba = FALSE, 
                check_names = FALSE) {
  
  # List measures
  lsmnodist <- c("CPC", "NRMSE", "KL", "CPL")
  lsmdist <- c("CPC_d", "KS")
  lsm <- c(lsmdist, lsmnodist)
  if ("all" %in% measures) {
    metrics <- lsm
  } else {
    metrics <- measures
  }

  # Controls args
  controls(args = measures, type = "character_vector")
  if (sum(metrics %in% lsm) < length(metrics)) {
    stop(paste0("One or several goodness-of-fit measure(s) chosen are not", 
                " available.\n",
                "Please choose from the following:\n",
                "CPC, CPL, NRMSE, KL, CPC_d, or KS."),
         call. = FALSE)
  }
  controls(args = check_names, type = "boolean")
  if ("CPC_d" %in% metrics) {
    controls(args = bin_size, type = "positive_numeric")
  }

  # Controls data
  tdlm <- FALSE
  if (inherits(sim, "TDLM")) { # TDLM

    # TDLM attributes
    tdlm <- TRUE
    from <- attributes(sim)$from

    if (from == "run_law_model") {
      isproba <- attributes(sim)$proba
      controls(args = use_proba, type = "boolean")
      if (use_proba & !isproba) {
        stop("use_proba cannot be set to TRUE if there is no proba in sim.",
             call. = FALSE)
      }
    }

    # Number of lists
    hlist <- 2
    if (is.matrix(sim[[2]])) {
      hlist <- 1
    }

    # If distance-based metrics
    if (sum(metrics %in% lsmdist) > 0) {
      if (hlist == 2) {
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            distance = distance,
            matrices_in_sim = sim[[2]][[1]]
          ),
          type = "matrices_positive"
        )
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            distance = distance,
            matrices_in_sim = sim[[2]][[1]]
          ),
          type = "matrices_matrices"
        )
      } else {
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            distance = distance,
            matrices_in_sim = sim[[2]]
          ),
          type = "matrices_positive"
        )
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            distance = distance,
            matrices_in_sim = sim[[2]]
          ),
          type = "matrices_matrices"
        )
      }

      # Check names
      if (check_names) {
        if (hlist == 2) {
          controls(
            args = NULL,
            matrices = list(
              obs = obs,
              distance = distance,
              matrices_in_sim = sim[[2]][[1]]
            ),
            type = "matrices_checknames"
          )
        } else {
          controls(
            args = NULL,
            matrices = list(
              obs = obs,
              distance = distance,
              matrices_in_sim = sim[[2]]
            ),
            type = "matrices_checknames"
          )
        }
      }
    } else { # If NOT distance-based metrics

      if (hlist == 2) {
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            matrices_in_sim = sim[[2]][[1]]
          ),
          type = "matrices_positive"
        )
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            matrices_in_sim = sim[[2]][[1]]
          ),
          type = "matrices_matrices"
        )
      } else {
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            matrices_in_sim = sim[[2]]
          ),
          type = "matrices_positive"
        )
        controls(
          args = NULL,
          matrices = list(
            obs = obs,
            matrices_in_sim = sim[[2]]
          ),
          type = "matrices_matrices"
        )
      }

      # Check names
      if (check_names) {
        if (hlist == 2) {
          controls(
            args = NULL,
            matrices = list(
              obs = obs,
              matrices_in_sim = sim[[2]][[1]]
            ),
            type = "matrices_checknames"
          )
        } else {
          controls(
            args = NULL,
            matrices = list(
              obs = obs,
              matrices_in_sim = sim[[2]]
            ),
            type = "matrices_checknames"
          )
        }
      }
    }

    # Not TDLM
  } else {
    if (is.matrix(sim)) {
      list <- 0
      sim <- list(sim = sim)
    } else {
      list <- 1
      if (!is.list(sim)) {
        stop("If sim is not a TDLM object it must be a list or a matrix.",
          call. = FALSE
        )
      }
      if (is.null(names(sim))) {
        names(sim) <- paste0("sim_", seq_len(length(sim)))
        message(paste0(
          "No names identified in the list of matrices.\n",
          "Names have been automatically assigned."
        ))
      }
    }

    if (sum(metrics %in% lsmdist) > 0) { # If distance-based metrics

      controls(
        args = NULL,
        matrices = c(list(
          obs = obs,
          distance = distance
        ), sim),
        type = "matrices_positive"
      )
      controls(
        args = NULL,
        matrices = c(list(
          obs = obs,
          distance = distance
        ), sim),
        type = "matrices_matrices"
      )

      # Check names
      if (check_names) {
        controls(
          args = NULL,
          matrices = c(list(
            obs = obs,
            distance = distance
          ), sim),
          type = "matrices_checknames"
        )
      }
    } else { # If NOT distance-based metrics

      controls(
        args = NULL,
        matrices = c(list(obs = obs), sim),
        type = "matrices_positive"
      )
      controls(
        args = NULL,
        matrices = c(list(obs = obs), sim),
        type = "matrices_matrices"
      )

      # Check names
      if (check_names) {
        controls(
          args = NULL,
          matrices = c(list(obs = obs), sim),
          type = "matrices_checknames"
        )
      }
    }
  }

  # proba
  if (tdlm) {
    if (from == "run_law_model") {
      if (use_proba) {
        if (hlist == 2) {
          for (k in 2:length(sim)) {
            sim[[k]] <- sim[[k]][-c(1:(length(sim[[k]]) - 1))]
          }
        } else {
          sim <- sim[-c(2:(length(sim) - 1))]
        }
        message("proba matrix used!")
      } else {
        if(isproba){
          if (hlist == 2) {
            for (k in 2:length(sim)) {
              sim[[k]] <- sim[[k]][-length(sim[[k]])]
            }
          } else {
            sim <- sim[-length(sim)]
          }
        }
      }
    }
  }

  # Initialize res
  if (tdlm) {
    if (hlist == 2) {
      paramval <- sim$info[-c(1:4),2]
      paramval <- merge(names(sim[[2]]),paramval)[,2]
      nameres <- merge(names(sim[[2]]),names(sim)[-1])
      nameres <- nameres[,c(2,1)]
      res <- data.frame(Parameter = nameres[, 1],
                        Parameter_value = paramval,
                        Simulation = nameres[, 2])
    } else {
      res <- data.frame(Simulation = names(sim)[-1])
    }
  } else {
    res <- data.frame(Matrix = names(sim))
  }
  
  # Compute GOF
  if (tdlm) {
    if (hlist == 2) {
      temp <- NULL
      for (k in 2:length(sim)) {
        if (sum(metrics %in% lsmdist) > 0) {
          tempk <- gofi(
            sim = sim[[k]],
            obs = obs,
            distance = distance,
            measures = metrics,
            bin_size = 2
          )
        } else {
          tempk <- gofi(
            sim = sim[[k]],
            obs = obs,
            measures = metrics
          )
        }
        temp <- rbind(temp, tempk)
      }
    } else {
      if (sum(metrics %in% lsmdist) > 0) {
        temp <- gofi(
          sim = sim[-1],
          obs = obs,
          distance = distance,
          measures = metrics,
          bin_size = bin_size
        )
      } else {
        temp <- gofi(
          sim = sim[-1],
          obs = obs,
          measures = metrics
        )
      }
    }
  } else {
    if (sum(metrics %in% lsmdist) > 0) {
      temp <- gofi(
        sim = sim,
        obs = obs,
        distance = distance,
        measures = metrics,
        bin_size = bin_size
      )
    } else {
      temp <- gofi(
        sim = sim,
        obs = obs,
        measures = metrics
      )
    }
  }
  res <- cbind(res, temp)

  # Return output
  return(res)
}
