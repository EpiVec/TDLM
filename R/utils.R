controls <- function(args = NULL,
                     vectors = NULL,
                     matrices = NULL,
                     type = "vectors_positive") {
  
  # vectors_positive ###########################################################
  if (type == "vectors_positive") {
    nbv <- length(vectors)
    for (k in 1:nbv) {
      vector <- vectors[[k]]
      namevec <- names(vectors)[k]

      if (!is.numeric(vector)) {
        stop(paste0(namevec, " must be a numeric vector."),
          call. = FALSE
        )
      }
      if (length(vector) < 2) {
        stop(paste0(namevec, " must contain at least two locations."),
          call. = FALSE
        )
      }
      if (sum(is.na(vector)) > 0) {
        stop(paste0("NA(s) detected in ", namevec, "."),
          call. = FALSE
        )
      }
      if (sum(vector != 0) == 0) {
        stop(paste0(namevec, " must contain at least one strictly positive 
        value."),
          call. = FALSE
        )
      }
      if (sum(vector < 0) > 0) {
        stop(paste0(namevec, " must contain only positive values."),
          call. = FALSE
        )
      }
    }
  }

  # vectors_positive_integer ###################################################
  if (type == "vectors_positive_integer") {
    nbv <- length(vectors)
    for (k in 1:nbv) {
      vector <- vectors[[k]]
      namevec <- names(vectors)[k]

      if (!is.numeric(vector)) {
        stop(paste0(namevec, " must be a numeric vector."),
          call. = FALSE
        )
      } else {
        if (sum(vector %% 1 != 0) > 1) {
          stop(paste0(namevec, " must be a vector of integers."),
            call. = FALSE
          )
        } else {
          if (sum(vector < 0) > 0) {
            stop(paste0(namevec, " must contain only positive values."),
              call. = FALSE
            )
          }
        }
      }

      if (length(vector) < 2) {
        stop(paste0(namevec, " should must at least two locations."),
          call. = FALSE
        )
      }
      if (sum(vector != 0) == 0) {
        stop(paste0(namevec, " must contain at least one strictly positive 
        value."),
          call. = FALSE
        )
      }
      if (sum(is.na(vector)) > 0) {
        stop(paste0("NA(s) detected in ", namevec, "."),
          call. = FALSE
        )
      }
      if (sum(vector < 0) > 0) {
        stop(paste0(namevec, " must contain only positive values."),
          call. = FALSE
        )
      }
    }
  }

  # matrices_positive ##########################################################
  if (type == "matrices_positive") {
    nbm <- length(matrices)
    for (k in 1:nbm) {
      matrix <- matrices[[k]]
      namemat <- names(matrices)[k]

      if (!is.matrix(matrix)) {
        stop(paste0(namemat, " must be a matrix."),
          call. = FALSE
        )
      }
      n <- dim(matrix)[1]
      m <- dim(matrix)[2]
      if (n != m) {
        stop(paste0(namemat, " must be squared."),
          call. = FALSE
        )
      }
      if (n < 2) {
        stop(paste0(namemat, " must contain at least two locations."),
          call. = FALSE
        )
      }
      if (sum(is.na(matrix)) > 0) {
        stop(paste0("NA(s) detected in ", namemat, "."),
          call. = FALSE
        )
      }
      if (sum(matrix != 0) == 0) {
        stop(paste0(namemat, " must contain at least one strictly positive 
        value."),
          call. = FALSE
        )
      }
      if (sum(matrix < 0) > 0) {
        stop(paste0(namemat, " must contain only positive values."),
          call. = FALSE
        )
      }
      #if (sum(diag(matrix)) > 0) {
      #  stop(paste0("The diagonal of ", namemat, " must be null."),
      #    call. = FALSE
      #  )
      #}
    }
  }

  # vectors_vectors ############################################################
  if (type == "vectors_vectors") {
    nbloc <- NULL
    nbv <- length(vectors)
    for (k in 1:nbv) {
      nbloc <- c(nbloc, length(vectors[[k]]))
    }

    if (sum(!duplicated(nbloc)) > 1) {
      mess <- "The inputs must contain the same number of locations!\n"
      for (k in 1:nbv) {
        mess <- paste0(
          mess, "         -> ",
          names(vectors)[k], ": ", length(vectors[[k]]),
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
  }

  # matrices_matrices ##########################################################
  if (type == "matrices_matrices") {
    nbloc <- NULL
    nbm <- length(matrices)
    for (k in 1:nbm) {
      nbloc <- c(nbloc, dim(matrices[[k]])[1])
    }

    if (sum(!duplicated(nbloc)) > 1) {
      mess <- "The inputs must contain the same number of locations!\n"
      for (k in 1:nbm) {
        mess <- paste0(
          mess, "         -> ",
          names(matrices)[k], ": ",
          dim(matrices[[k]])[1],
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
  }

  # vectors_matrices ###########################################################
  if (type == "vectors_matrices") {
    nbloc <- NULL
    nbv <- length(vectors)
    for (k in 1:nbv) {
      nbloc <- c(nbloc, length(vectors[[k]]))
    }
    nbm <- length(matrices)
    for (k in 1:nbm) {
      nbloc <- c(nbloc, dim(matrices[[k]])[1])
    }

    if (sum(!duplicated(nbloc)) > 1) {
      mess <- "The inputs must contain the same number of locations!\n"
      for (k in 1:nbv) {
        mess <- paste0(
          mess, "         -> ",
          names(vectors)[k], ": ", length(vectors[[k]]),
          " locations\n"
        )
      }
      for (k in 1:nbm) {
        mess <- paste0(
          mess, "         -> ",
          names(matrices)[k], ": ",
          dim(matrices[[k]])[1],
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
  }

  # vectors_checknames #########################################################
  if (type == "vectors_checknames") {
    n <- length(vectors[[1]])

    # Length names
    nbid <- NULL
    nbv <- length(vectors)
    for (k in 1:nbv) {
      nbid <- c(nbid, length(names(vectors[[k]])))
    }
    if (sum(!duplicated(nbid)) > 1) {
      mess <- "The inputs must contain the same number of names id!\n"
      for (k in 1:nbv) {
        mess <- paste0(
          mess, "         -> ",
          names(vectors)[k], ": ", length(names(vectors[[k]])),
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
    if (nbid[1] < n) {
      stop("The number of names is lower than the number of locations!",
        call. = FALSE
      )
    }

    # names vectors-vectors
    if (nbv > 1) {
      test <- NULL
      for (k1 in 1:(nbv - 1)) {
        for (k2 in (k1 + 1):nbv) {
          testk1k2 <- (length(intersect(
            names(vectors[[k1]]),
            names(vectors[[k2]])
          )) == n)
          test <- rbind(test, data.frame(from = k1, to = k2, test = testk1k2))
        }
      }
      ntest <- dim(test)[1]
      if (sum(test$test) < ntest) {
        mess <- "Different names in vectors:\n"
        for (k in 1:ntest) {
          if (!test$test[k]) {
            mess <- paste0(
              mess,
              "         -> ",
              names(vectors)[test[k, 1]],
              " and ",
              names(vectors)[test[k, 2]], "\n"
            )
          }
        }
        stop(mess, call. = FALSE)
      }
    }
  }

  # matrices_checknames ########################################################
  if (type == "matrices_checknames") {
    n <- dim(matrices[[1]])[1]

    # Length names
    nbid <- NULL
    nbm <- length(matrices)
    for (k in 1:nbm) {
      nbid <- c(nbid, length(rownames(matrices[[k]])))
      nbid <- c(nbid, length(colnames(matrices[[k]])))
    }
    if (sum(!duplicated(nbid)) > 1) {
      mess <- "The inputs must contain the same number of names id!\n"
      for (k in 1:nbm) {
        mess <- paste0(
          mess, "         -> ",
          names(matrices)[k], ": ",
          length(rownames(matrices[[k]])),
          " x ",
          length(colnames(matrices[[k]])),
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
    if (nbid[1] < n) {
      stop("The number of names is lower than the number of locations!",
        call. = FALSE
      )
    }

    # names matrices
    test <- NULL
    for (k in 1:nbm) {
      test <- c(test, length(intersect(
        rownames(matrices[[k]]),
        colnames(matrices[[k]])
      )) == n)
    }
    if (sum(test) < nbm) {
      mess <- "Different rownames and colnames in:\n"
      for (k in 1:nbm) {
        if (!test[k]) {
          mess <- paste0(
            mess,
            "         -> ", names(matrices)[k], "\n"
          )
        }
      }
      stop(mess, call. = FALSE)
    }

    # names matrices-matrices
    if (nbm > 1) {
      test <- NULL
      for (k1 in 1:(nbm - 1)) {
        for (k2 in (k1 + 1):nbm) {
          testk1k2 <- (length(intersect(
            rownames(matrices[[k1]]),
            rownames(matrices[[k2]])
          )) == n)
          test <- rbind(test, data.frame(from = k1, to = k2, test = testk1k2))
        }
      }
      ntest <- dim(test)[1]
      if (sum(test$test) < ntest) {
        mess <- "Different names in matrices:\n"
        for (k in 1:ntest) {
          if (!test$test[k]) {
            mess <- paste0(
              mess,
              "         -> ",
              names(matrices)[test[k, 1]],
              " and ",
              names(matrices)[test[k, 2]], "\n"
            )
          }
        }
        stop(mess, call. = FALSE)
      }
    }
  }


  # vectors_matrices_checknames ################################################
  if (type == "vectors_matrices_checknames") {
    n <- length(vectors[[1]])

    # Length names
    nbid <- NULL
    nbv <- length(vectors)
    for (k in 1:nbv) {
      nbid <- c(nbid, length(names(vectors[[k]])))
    }
    nbm <- length(matrices)
    for (k in 1:nbm) {
      nbid <- c(nbid, length(rownames(matrices[[k]])))
      nbid <- c(nbid, length(colnames(matrices[[k]])))
    }
    if (sum(!duplicated(nbid)) > 1) {
      mess <- "The inputs must contain the same number of names id!\n"
      for (k in 1:nbv) {
        mess <- paste0(
          mess, "         -> ",
          names(vectors)[k], ": ", length(names(vectors[[k]])),
          " locations\n"
        )
      }
      for (k in 1:nbm) {
        mess <- paste0(
          mess, "         -> ",
          names(matrices)[k], ": ",
          length(rownames(matrices[[k]])),
          " x ",
          length(colnames(matrices[[k]])),
          " locations\n"
        )
      }
      stop(mess, call. = FALSE)
    }
    if (nbid[1] < n) {
      stop("The number of names is lower than the number of locations!",
        call. = FALSE
      )
    }

    # names matrices
    test <- NULL
    for (k in 1:nbm) {
      test <- c(test, length(intersect(
        rownames(matrices[[k]]),
        colnames(matrices[[k]])
      )) == n)
    }
    if (sum(test) < nbm) {
      mess <- "Different rownames and colnames in:\n"
      for (k in 1:nbm) {
        if (!test[k]) {
          mess <- paste0(
            mess,
            "         -> ", names(matrices)[k], "\n"
          )
        }
      }
      stop(mess, call. = FALSE)
    }

    # names matrices-matrices
    if (nbm > 1) {
      test <- NULL
      for (k1 in 1:(nbm - 1)) {
        for (k2 in (k1 + 1):nbm) {
          testk1k2 <- (length(intersect(
            rownames(matrices[[k1]]),
            rownames(matrices[[k2]])
          )) == n)
          test <- rbind(test, data.frame(from = k1, to = k2, test = testk1k2))
        }
      }
      ntest <- dim(test)[1]
      if (sum(test$test) < ntest) {
        mess <- "Different names in matrices:\n"
        for (k in 1:ntest) {
          if (!test$test[k]) {
            mess <- paste0(
              mess,
              "         -> ",
              names(matrices)[test[k, 1]],
              " and ",
              names(matrices)[test[k, 2]], "\n"
            )
          }
        }
        stop(mess, call. = FALSE)
      }
    }

    # names vectors-vectors
    if (nbv > 1) {
      test <- NULL
      for (k1 in 1:(nbv - 1)) {
        for (k2 in (k1 + 1):nbv) {
          testk1k2 <- (length(intersect(
            names(vectors[[k1]]),
            names(vectors[[k2]])
          )) == n)
          test <- rbind(test, data.frame(from = k1, to = k2, test = testk1k2))
        }
      }
      ntest <- dim(test)[1]
      if (sum(test$test) < ntest) {
        mess <- "Different names in vectors:\n"
        for (k in 1:ntest) {
          if (!test$test[k]) {
            mess <- paste0(
              mess,
              "         -> ",
              names(vectors)[test[k, 1]],
              " and ",
              names(vectors)[test[k, 2]], "\n"
            )
          }
        }
        stop(mess, call. = FALSE)
      }
    }

    # names vectors-matrices
    test <- NULL
    for (k1 in 1:nbv) {
      for (k2 in 1:nbm) {
        testk1k2 <- (length(intersect(
          names(vectors[[k1]]),
          rownames(matrices[[k2]])
        )) == n)
        test <- rbind(test, data.frame(vec = k1, mat = k2, test = testk1k2))
      }
    }
    ntest <- dim(test)[1]
    if (sum(test$test) < ntest) {
      mess <- "Different names in vectors and matrices:\n"
      for (k in 1:ntest) {
        if (!test$test[k]) {
          mess <- paste0(
            mess,
            "         -> ",
            names(vectors)[test[k, 1]],
            " and ",
            names(matrices)[test[k, 2]], "\n"
          )
        }
      }
      stop(mess, call. = FALSE)
    }
  }

  # boolean ####################################################################
  if (type == "boolean") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.logical(args)) {
      stop(paste0(deparse(substitute(args)), " must be a boolean."),
        call. = FALSE
      )
    }
  }

  # character #################################################################
  if (type == "character") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.character(args)) {
      stop(paste0(deparse(substitute(args)), " must be a character."),
        call. = FALSE
      )
    }
    if (is.factor(args)) {
      args <- as.character(args)
    }
    return(args)
  }

  # character_vector ###########################################################
  if (type == "character_vector") {
    if (!is.character(args)) {
      stop(paste0(deparse(substitute(args)), " must be a character."),
        call. = FALSE
      )
    }
    if (is.factor(args)) {
      args <- as.character(args)
    }
    return(args)
  }

  # numeric_vector #############################################################
  if (type == "numeric_vector") {
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    }
  }

  # positive_numeric ##########################################################
  if (type == "positive_numeric") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    } else {
      if (args < 0) {
        stop(paste0(deparse(substitute(args)), " must be higher than 0."),
          call. = FALSE
        )
      }
    }
  }

  # strict_positive_numeric ####################################################
  if (type == "strict_positive_numeric") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    } else {
      if (args <= 0) {
        stop(
          paste0(
            deparse(substitute(args)),
            " must be strictly higher than 0."
          ),
          call. = FALSE
        )
      }
    }
  }

  # positive_integer ###########################################################
  if (type == "positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    } else {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
          call. = FALSE
        )
      } else {
        if (args < 0) {
          stop(paste0(deparse(substitute(args)), " must be higher than 0."),
            call. = FALSE
          )
        }
      }
    }
  }

  # strict_positive_integer ####################################################
  if (type == "strict_positive_integer") {
    if (length(args) > 1) {
      stop(paste0(deparse(substitute(args)), " must be of length 1."),
        call. = FALSE
      )
    }
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    } else {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
          call. = FALSE
        )
      } else {
        if (args <= 0) {
          stop(paste0(
            deparse(substitute(args)),
            " must be strictly higher than 0."
          ), call. = FALSE)
        }
      }
    }
  }

  # list #######################################################################
  if (type == "list") {
    if (!is.list(args)) {
      stop(paste0(deparse(substitute(args)), " must be a list."),
        call. = FALSE
      )
    }
  }
}


# gofi #########################################################################
gofi <- function(sim, obs, distance,
                 measures = c("CPC", "NRMSE", "KL", "CPL", "CPC_d", "KS"),
                 bin_size) {
  # sim is a matrix
  # obs is a list of matrix
  # distance is a matrix

  # Initialization
  if ("KS" %in% measures) {
    res <- matrix(0, length(sim), (length(measures) + 1))
    colnames(res) <- 1:(length(measures) + 1)
  } else {
    res <- matrix(0, length(sim), length(measures))
    colnames(res) <- 1:length(measures)
  }
  No <- sum(obs)

  # Distance
  if ("CPC_d" %in% measures) {
    maxbreak <- (trunc(max(distance) / bin_size) + 1) * bin_size
    tempd <- cut(as.vector(distance),
      breaks = seq(0, maxbreak, bin_size),
      labels = FALSE, right = FALSE
    )
    Nol <- stats::aggregate(as.vector(obs), list(tempd), sum)[, 2]
  }

  # Loop over sim
  for (k in 1:length(sim)) {
    m <- 1

    simk <- sim[[k]]
    Nk <- sum(simk)

    # CPC ######################################################################
    if ("CPC" %in% measures) {
      cpc <- 2 * sum(pmin(simk, obs)) / (No + Nk)
      res[k, m] <- cpc
      colnames(res)[m] <- "CPC"
      m <- m + 1
    }

    # NRMSE ####################################################################
    if ("NRMSE" %in% measures) {
      nrmse <- sqrt(sum((obs - simk) * (obs - simk)) / No)
      res[k, m] <- nrmse
      colnames(res)[m] <- "NRMSE"
      m <- m + 1
    }

    # KL #######################################################################
    if ("KL" %in% measures) {
      kl <- (obs / No) * log((obs / No) / (simk / Nk))
      kl[is.na(kl)] <- 0
      kl[is.infinite(kl)] <- 0
      kl <- sum(kl)
      res[k, m] <- kl
      colnames(res)[m] <- "KL"
      m <- m + 1
    }

    # CPL ######################################################################
    if ("CPL" %in% measures) {
      cpl <- 2 * sum(pmin(simk > 0, obs > 0)) / (sum(simk > 0) + sum(obs > 0))
      res[k, m] <- cpl
      colnames(res)[m] <- "CPL"
      m <- m + 1
    }

    # CPC_D ####################################################################
    if ("CPC_d" %in% measures) {
      Nl <- stats::aggregate(as.vector(simk), list(tempd), sum)[, 2]
      cpc_d <- 2 * sum(pmin(Nol, Nl)) / (No + Nk)
      res[k, m] <- cpc_d
      colnames(res)[m] <- "CPC_d"
      m <- m + 1
    }

    # KS ####################################################################
    if ("KS" %in% measures) {
      ks <- Ecume::ks_test(as.vector(distance),
        as.vector(distance),
        thresh = .001,
        w_x = as.vector(obs),
        w_y = as.vector(simk)
      )
      res[k, m] <- ks$stat
      res[k, (m + 1)] <- ks$p
      colnames(res)[m] <- "KS_stat"
      colnames(res)[(m + 1)] <- "KS_pval"
      m <- m + 2
    }
  }

  # Return output
  res <- data.frame(res)
  return(res)
}

# haversine ####################################################################
haversine=function(lon1,lat1,lon2,lat2) {
  R=6367 # Earth mean radius [km]
  lon1=lon1*pi/180
  lat1=lat1*pi/180
  lon2=lon2*pi/180
  lat2=lat2*pi/180
  
  dlon = lon2 - lon1
  dlat = lat2 - lat1
  
  a = sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c = 2 * asin(pmin(1,sqrt(a)))
  d = R * c
  return(d)
}
