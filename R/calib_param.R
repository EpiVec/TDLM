#' Automatic calibration of trip distribution laws' parameter
#'
#' This function returns an estimate of the optimal parameter value based on
#' the average surface area of the locations (in square kilometers) according
#' to the law. This estimation has only been tested on commuting data
#' (in kilometers).
#'
#' @param av_surf A positive `numeric` value indicating the average surface
#' area of the locations (in square kilometers).
#'
#' @param law A `character` string indicating which law to use (see Details).
#' 
#' @return 
#' An estimate of the optimal parameter value based on
#' the average surface area of the locations.
#'
#' @details 
#' The estimation is based on Figure 8 in Lenormand \emph{et al.} 
#' (2016) for four types of laws: the normalized gravity law with an exponential
#' distance decay function (`law = "NGravExp"`), the normalized gravity law with
#'  a power distance decay function (`law = "NGravPow"`), Schneider's 
#'  intervening opportunities law (`law = "Schneider"`), and the extended 
#'  radiation law (`law = "RadExt"`).
#' 
#' @references
#' Lenormand M, Bassolas A, Ramasco JJ (2016) Systematic comparison of trip 
#' distribution laws and models. \emph{Journal of Transport Geography} 51, 
#' 158-169.
#' 
#' @seealso
#' Associated functions:  
#' [extract_opportunities()] [extract_spatial_information()]
#' [check_format_names()]

#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @examples
#' data(county)
#'
#' res <- extract_spatial_information(county, id = "ID")
#' av_surf <- mean(res$surface)
#'
#' calib_param(av_surf = av_surf, law = "NGravExp")
#' calib_param(av_surf = av_surf, law = "NGravPow")
#' calib_param(av_surf = av_surf, law = "Schneider")
#' calib_param(av_surf = av_surf, law = "RadExt")
#'
#' @export
calib_param <- function(av_surf, law = "NGravExp") {
  
  # Controls
  controls(args = av_surf, type = "strict_positive_numeric")

  laws <- c("NGravExp", "NGravPow", "Schneider", "RadExt")
  if (!(law %in% laws)) {
    stop(paste0("One or several laws chosen are not available.\n",
                "Please choose from the following:\n",
                "NGravExp, NGravPow, Schneider, or RadExt."),
         call. = FALSE)
  }

  # Parameter estimation
  if (law == "NGravExp") {
    res <- 0.3028016 * av_surf^(-0.1665103)
  }
  if (law == "NGravPow") {
    res <- 1.4280970 * av_surf^(0.1098316)
  }
  if (law == "Schneider") {
    res <- 0.000003022048 * av_surf^(-0.026206428187)
  }
  if (law == "RadExt") {
    res <- 0.01531479 * av_surf^(0.58317902)
  }

  # Return output
  return(res)
}
