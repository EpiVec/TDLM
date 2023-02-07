controls <- function(args = NULL,
                     vectors = NULL,
                     matrices = NULL,
                     type = "vectors_positive") {
  # vectors_positive ############################################################
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
        stop(paste0(namevec, " should contain at least two locations."),
          call. = FALSE
        )
      }
      if (sum(is.na(vector)) > 0) {
        stop(paste0("NA(s) detected in ", namevec, "."),
          call. = FALSE
        )
      }
      if (sum(vector < 0) > 0) {
        stop(paste0(namevec, " should contain only positive values."),
          call. = FALSE
        )
      }
    }
  }

  # matrices ###################################################################
  if (type == "matrices") {
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
        stop(paste0(namemat, " should be symmetrical."),
          call. = FALSE
        )
      }
      if (n < 2) {
        stop(paste0(namemat, " should contain at least two locations."),
          call. = FALSE
        )
      }
      if (sum(is.na(matrix)) > 0) {
        stop(paste0("NA(s) detected in ", namemat, "."),
          call. = FALSE
        )
      }
      if (sum(matrix < 0) > 0) {
        stop(paste0(namemat, " should contain only positive values."),
          call. = FALSE
        )
      }
      if (sum(diag(matrix)) > 0) {
        stop(paste0("The diagonal of ", namemat, " should be null."),
          call. = FALSE
        )
      }
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
      mess <- "The inputs should contain the same number of locations!\n"
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

  # vectors_matrices_checknames ###################################################
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
      mess <- "The inputs should contain the same number of names id!\n"
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
        stop(mess, call. = FALSE)
      }
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
    if (!is.logical(args)) {
      stop(paste0(deparse(substitute(args)), " must be a boolean."),
        call. = FALSE
      )
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







  # Character #################################################################
  if (type == "character") {
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

  # Numeric ###################################################################
  if (type == "numeric") {
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    }
  }

  # Positive numeric ##########################################################
  if (type == "positive_numeric") {
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

  # Strict positive numeric ###################################################
  if (type == "strict_positive_numeric") {
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
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

  # Integer ###################################################################
  if (type == "integer") {
    if (!is.numeric(args)) {
      stop(paste0(deparse(substitute(args)), " must be numeric."),
        call. = FALSE
      )
    } else {
      if (args %% 1 != 0) {
        stop(paste0(deparse(substitute(args)), " must be an integer."),
          call. = FALSE
        )
      }
    }
  }

  # Positive integer ##########################################################
  if (type == "positive_integer") {
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

  # Strict positive integer ###################################################
  if (type == "strict_positive_integer") {
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
}
