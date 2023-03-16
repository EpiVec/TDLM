#' Compute goodness-of-fit measures between observed and simulated OD matrices
#'
#' This function returns a data.frame where each row provides one or
#' several goodness-of-fit measures between a simulated and an observed
#' Origin-Destination matrix.
#'
#' @param sim an object of class `TDLM` (output of [run_law_model()],
#' [run_law()] or [run_model()]).
#' A matrix or a list of matrices can also be used (see Note).
#'
#' @param obs a squared matrix representing the observed mobility flows.
#'
#' @param measures a vector of string(s) indicating which goodness-of-fit
#' measure(s) to chose (see Details). If `"all"` is specified, then all measures
#'  will be calculated.
#'
#' @param distance a squared matrix representing the distance between locations.
#' Only necessary for the distance-based measures.
#'
#' @param use_proba a boolean indicating if the `proba` matrix should be used
#' instead of the simulated OD matrix to compute the measure(s). Only valid for
#'  the output from [run_law_model()] with argument `write_proba = TRUE` (see
#'  Note).
#'
#' @param check_names a boolean indicating if the ID location are used as matrix
#'  rownames and colnames and if they should be checked (see Note).
#'
#' @param bin_size a numeric value indicating the size of bin used to discretize
#' the distance distribution to compute CPC_d (2 "km" by default).
#'
#' @details
#' \loadmathjax
#' With \mjeqn{n}{n} the number of locations, \mjeqn{T_{ij}}{T_{ij}} the observed
#' flow between location \mjeqn{i}{i} and location \mjeqn{j}{j}
#' (argument `obs`), \mjeqn{\tilde{T}_{ij}}{\tilde{T}_{ij}} a simulated flow
#' between location \mjeqn{i}{i} and location \mjeqn{j}{j} (a matrix from
#' argument `sim`), \mjeqn{N=\sum_{i,j=1}^n T_{ij}}{N=\sum_{i,j=1}^n T_{ij}} the
#' sum of observed flows and
#' \mjeqn{\tilde{N}=\sum_{i,j=1}^n \tilde{T}_{ij}}{\tilde{T}=\sum_{i,j=1}^n \tilde{T}_{ij}}
#' the sum of simulated flows.
#'
#' Several goodness-of-fit measures have been considered
#' `measures = c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS")`. The Common Part
#' of Commuters \insertCite{Gargiulo2012,Lenormand2012,Lenormand2016}{TDLM},
#'
#' \mjeqn{\displaystyle CPC(T,\tilde{T}) = \frac{2\cdot\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}{\displaystyle CPC(T,\tilde{T}) = \frac{2\cdot\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}
#'
#' the Normalized Root Mean Square Error (NRMSE),
#'
#' \mjeqn{\displaystyle NRMSE(T,\tilde{T}) = \sqrt{\frac{\sum_{i,j=1}^n (T_{ij}-\tilde{T}_{ij})^2}{N}}}{\displaystyle NRMSE(T,\tilde{T}) = \sqrt{\frac{\sum_{i,j=1}^n (T_{ij}-\tilde{T}_{ij})^2}{N}}}
#'
#' the Kullback–Leibler divergence \insertCite{Kullback1951}{TDLM},
#'
#' \mjeqn{\displaystyle KL(T,\tilde{T}) = \sum_{i,j=1}^n \frac{T_{ij}}{N}\log\left(\frac{T_{ij}}{N}\frac{\tilde{N}}{\tilde{T}_{ij}}\right)}{\displaystyle KL(T,\tilde{T}) = \sum_{i,j=1}^n \frac{T_{ij}}{N}\log\left(\frac{T_{ij}}{N}\frac{\tilde{N}}{\tilde{T}_{ij}}\right)}
#'
#' the Common Part of Links (CPL) \insertCite{Lenormand2016}{TDLM},
#'
#' \mjeqn{\displaystyle CPL(T,\tilde{T}) = \frac{2\cdot\sum_{i,j=1}^n 1_{T_{ij}>0} \cdot 1_{\tilde{T}_{ij}>0}}{\sum_{i,j=1}^n 1_{T_{ij}>0} + \sum_{i,j=1}^n 1_{\tilde{T}_{ij}>0}}}{\displaystyle CPL(T,\tilde{T}) = \frac{2\cdot\sum_{i,j=1}^n 1_{T_{ij}>0} \cdot 1_{\tilde{T}_{ij}>0}}{\sum_{i,j=1}^n 1_{T_{ij}>0} + \sum_{i,j=1}^n 1_{\tilde{T}_{ij}>0}}}
#'
#' the Common Part of Commuters based on the disance
#' \insertCite{Lenormand2016}{TDLM}, noted CPC_d. Let us consider
#' \mjeqn{N_k}{N_k} (and \mjeqn{\tilde{N}_k}{\tilde{N}_k}) the
#' sum of observed (and simulated) flows at a distance comprised in the bin
#' [`bin_size`*k-`bin_size`, `bin_size`*k[.
#'
#' \mjeqn{\displaystyle CPC_d(T,\tilde{T}) = \frac{2\cdot\sum_{k=1}^{\infty} min(N_{k},\tilde{N}_{k})}{N+\tilde{N}}}{\displaystyle CPC_d(T,\tilde{T}) = \frac{2\cdot\sum_{k=1}^{\infty} min(N_{k},\tilde{N}_{k})}{N+\tilde{N}}}
#'
#' and the Kolmogorv-Smirnov statistic and p-value \insertCite{Massey1951}{TDLM}
#' , noted KS. It is based on the observed and simulated flow distance
#' distribution and computed with the [ks_test][Ecume::ks_test] function from
#' the [Ecume](https://cran.r-project.org/package=Ecume) package.
#'
#' @note By default, if `sim` is an output of [run_law_model()]
#' the measure(s) are computed only for the simulated OD matrices and
#' not the `proba` matrix (included in the output when
#' `write_proba = TRUE`). The argument `use_proba` can be used to compute the
#' measure(s) based on the `proba` matrix instead of the simulated
#' OD matrix. In this case the argument `obs` should also be a proba matrix.
#'
#' All the inputs should be based on the same number of
#' locations sorted in the same order. It is recommended to use the location ID
#' as matrix rownames and matrix colnames and to set
#' `check_names = TRUE` to verify that everything is in order before running
#' this function (`check_names = FALSE` by default). Note that the function
#' [check_format_names()] can be used to control the validity of all the inputs
#' before running the main package's functions.
#'
#' @return
#' A data.frame providing one or several goodness-of-fit measure(s) between
#' simulated OD(s) and an observed OD. Each row corresponds to a matrix sorted
#' according to the list (or list of list) elements (names are used if
#' provided).
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [run_law_model()] [run_law()] [run_model()] [run_law_model()]
#' [check_format_names()]
#'
#' @examples
#' \dontrun{
#' data(mass)
#' data(distance)
#' data(od)
#'
#' mi <- as.numeric(mass[, 1])
#' mj <- mi
#' Oi <- as.numeric(mass[, 2])
#' Dj <- as.numeric(mass[, 3])
#'
#' res <- run_law_model(
#'   law = "GravExp", mass_origin = mi, mass_destination = mj,
#'   distance = distance, opportunity = NULL, param = c(0.01, 0.02, 0.03),
#'   model = "DCM", nb_trips = NULL, out_trips = Oi, in_trips = Dj,
#'   average = FALSE, nbrep = 3, maxiter = 50, mindiff = 0.01,
#'   write_proba = FALSE,
#'   check_names = FALSE
#' )
#'
#' gof(
#'   sim = res, obs = od, measures = "all", distance = distance, bin_size = 2,
#'   use_proba = FALSE,
#'   check_names = FALSE
#' )
#' }
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' \insertRef{Gargiulo2012}{TDLM}
#'
#' \insertRef{Lenormand2012}{TDLM}
#'
#' \insertRef{Kullback1951}{TDLM}
#'
#' \insertRef{Massey1951}{TDLM}
#'
#' @export
gof <- function(sim, obs, measures = "all", distance = NULL, bin_size = 2,
                use_proba = FALSE, check_names = FALSE) {
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
    stop("One or several goodness-of-fit measure(s) chosen is not available.
     Please chose among the followings:
         CPC, CPL, NRMSE, KL, CPC_d or KS")
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
          call. = FALSE
        )
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
        names(sim) <- paste0("sim_", 1:length(sim))
        message(paste0(
          "No names identified in the list of matrices.\n",
          "Names have been automatically assigned.\n"
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
