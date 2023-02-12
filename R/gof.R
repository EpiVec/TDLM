#' Compute goodness-of-fit measures between observed and simulated OD matrices
#'
#' This function returns a data.frame where each row provides one or
#' several goodness-of-fit measures between a simulated and an observed 
#' Origin-Destination matrix.
#'
#' @param sim an object of class `TDLM` (output from [run_law_model()], 
#' [run_law()] or [run_model()]). 
#' A matrix or a list of matrix can also be used (see Note).
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
#' @param use_proba a boolean indicating if the proba matrix should be used 
#' instead of the simulated OD matrix to compute the measure(s). Only valid for
#'  the output from [run_law_model()] with argument `write_proba = TRUE` (see 
#'  Note).
#'
#' @param check_names a boolean indicating if the ID location are used as matrix
#'  rownames and colnames and if they should be checked (see Note).
#'  
#' @details
#' \loadmathjax
#' With \mjeqn{n}{n} the number of locations, \mjeqn{T_{ij}}{T_{ij}} the observed
#' flow between location \mjeqn{i}{i} and location \mjeqn{j}{j} 
#' (argument `obs`), \mjeqn{\tilde{T}_{ij}}{\tilde{T}_{ij}} a simulated flow 
#' between location \mjeqn{i}{i} and location \mjeqn{j}{j} (a matrix from 
#' argument `sim`), \mjeqn{N=\sum_{i,j=1}^n T_{ij}}{N=\sum_{i,j=1}^n T_{ij}} the 
#' total number of observed flows and 
#' \mjeqn{\tilde{N}=\sum_{i,j=1}^n \tilde{T}_{ij}}{\tilde{T}=\sum_{i,j=1}^n \tilde{T}_{ij}}. 
#' 
#' As described in \insertCite{Lenormand2016}{TDLM},
#' 
#' \mjeqn{\displaystyle CPC(T,\tilde{T}) = \frac{2\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}{\displaystyle CPC(T,\tilde{T}) = \frac{2\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}
#'
#' \mjeqn{\displaystyle CPC(T,\tilde{T}) = \frac{2\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}{\displaystyle CPC(T,\tilde{T}) = \frac{2\sum_{i,j=1}^n min(T_{ij},\tilde{T}_{ij})}{N + \tilde{N}}}
#'
#' @note By default, if `sim` is an output of [run_law_model()]
#' the measure(s) are computed only for the simulated OD matrices and 
#' not the proba matrix (included in the output when 
#' `write_proba = TRUE`). The argument `use_proba` can be used to compute the
#' measure(s) based on the proba matrix instead of the simulated 
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
#' simulated OD(s) and an observed OD. 
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [run_law_model()]
#'
#' @examples
#' data(mass)
#' data(distance)
#' ind <- sample(dim(distance)[1], 100)
#'
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' @export
gof <- function(sim, obs, measures = "all", distance,  
                use_proba =  FALSE, check_names = FALSE) {
  
  
  
  # Return output
  return(1)
}
