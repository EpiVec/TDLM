#' Automatic calibration of trip distribution laws' parameter
#'
#' This function returns an estimation of the optimal parameter value based on
#' the average surface area of the locations (in square kilometer) according to
#'  the law. This estimation has only been tested on commuting data
#' (in kilometer).
#'
#' @param av_surf a positive numeric value indicating the average surface
#' area of the locations (in square kilometer).
#'
#' @param law a character indicating which law to use (see Details).
#'
#' @details The estimation is based on the Figure 8 in
#' \insertCite{Lenormand2016;textual}{TDLM} for four types of laws. The
#' normalized gravity law with an exponential distance decay function
#' (`law = "NGravExp"`), the normalized gravity law with a power distance
#' decay function (`law = "NGravPow"`), the Schneider's intervening
#' opportunities law (`law = "Schneider"`) and the extended radiation law
#' (`law = "RadExt"`).
#'
#' @return An estimation of the optimal parameter value based on
#' the average surface area of the locations.
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [extract_opportunities()] [extract_spatial_information()]
#' [check_format_names()]
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
#' @references
#' \insertRef{Lenormand2016}{TDLM}
#'
#' @export
calib_param <- function(av_surf, law = "NGravExp") {
  # Controls
  controls(args = av_surf, type = "strict_positive_numeric")

  laws <- c("NGravExp", "NGravPow", "Schneider", "RadExt")
  if (!(law %in% laws)) {
    stop("Please choose law among the followings values:
NGravExp, NGravPow, Schneider or RadExt",
      call. = FALSE
    )
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
