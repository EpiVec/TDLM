controls <- function(args = NULL,
                     vector = NULL,
                     matrix = NULL,
                     mi = NULL,
                     mj = NULL,
                     Oi = NULL,
                     Dj = NULL,
                     dij = NULL,
                     sij = NULL,
                     od = NULL,
                     odsim = NULL,
                     type = "vector") {

  # vector_positive ############################################################
  if (type == "vector_positive") {
    if (!is.numeric(vector)) {
      stop(paste0(deparse(substitute(vector)), " must be a numeric vector."),
        call. = FALSE
      )
    }
    if (length(vector) < 2) {
      stop(paste0(deparse(substitute(vector)), " should contain at least two 
                  locations."),
        call. = FALSE
      )
    }
    if (sum(is.na(vector)) > 0) {
      stop(paste0("NA(s) detected in ", deparse(substitute(vector)), "."),
        call. = FALSE
      )
    }
    if (sum(vector < 0) > 0) {
      stop(paste0(deparse(substitute(vector)), " should contain only positive 
                  values."),
        call. = FALSE
      )
    }
  }

  # matrix #####################################################################
  if (type == "matrix") {
    if (!is.matrix(matrix)) {
      stop(paste0(deparse(substitute(matrix)), " must be a matrix."),
        call. = FALSE
      )
    }
    n <- dim(matrix)[1]
    m <- dim(matrix)[2]
    if (n != m) {
      stop(paste0(deparse(substitute(matrix)), " should be symmetrical."),
        call. = FALSE
      )
    }
    if (n < 2) {
      stop(paste0(deparse(substitute(matrix)), " should contain at least two 
                  locations."),
        call. = FALSE
      )
    }
    if (sum(is.na(matrix)) > 0) {
      stop(paste0("NA(s) detected in ", deparse(substitute(matrix)), "."),
        call. = FALSE
      )
    }
    if (sum(matrix < 0) > 0) {
      stop(paste0(deparse(substitute(matrix)), " should contain only positive 
                  values."),
        call. = FALSE
      )
    }
    if (sum(diag(matrix)) > 0) {
      stop(paste0("The diagonal of", deparse(substitute(matrix)), " should be 
                  null."),
        call. = FALSE
      )
    }
  }

  # vector_matrix ##############################################################
  if (type == "vector_matrix") {
    dimv <- length(vector)
    dimm <- dim(matrix)[1]
    if (dimv != dimm) {
      stop(paste0(
        deparse(substitute(vector)), " and ",
        deparse(substitute(matrix)), " should contain the same
                  number of locaions."
      ),
      call. = FALSE
      )
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
