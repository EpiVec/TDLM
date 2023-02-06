controls <- function(args = NULL,
                     vectors = NULL,
                     matrices = NULL,
                     type = "vectors_positive") {
  # vectors_positive ############################################################
  if (type == "vectors_positive") {
    nbv <- length(vectors)
    for (k in 1:nbv) {
      vector <- vectors[[k]]
      namevect <- names(vectors)[k]

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
        stop(paste0(namevec, " should contain only positive 
                  values."),
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
        stop(paste0("The diagonal of", namemat, " should be null."),
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

  # vector_matrix_checknames ###################################################
  if (type == "vector_matrix_checknames") {



  }

  # boolean ####################################################################
  if (type == "boolean") {
    if (!is.logical(args)) {
      stop(paste0(deparse(substitute(args)), " must be a boolean."),
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
