#' Extract the distance and average surface area from a spatial object
#'
#' This function returns a matrix of distances (in kilometer) along with the 
#' average surface area of the locations (in squared kilometer).
#'
#' @param geometry a spatial object that can be handled by the `sf` package.
#'
#' @param names .
#'
#' @param stdist a boolean indicating if the ... (see Details).
#'
#' @details
#' the [st_distance][sf::st_distance] functions from
#' the [sf](https://cran.rstudio.com/web/packages/sf/index.html) package.
#' 
#' #' the [st_area][sf::st_area] functions from
#' the [sf](https://cran.rstudio.com/web/packages/sf/index.html) package.
#'
#' @note By default
#'
#' @return
#' A list composed of two elements. The first element is a squared matrix 
#' representing the great-circle distance (in kilometer) between locations. The 
#' second element is the average surface area of the locations 
#' (in squared kilometer).
#'
#' @author
#' Maxime Lenormand (\email{maxime.lenormand@inrae.fr})
#'
#' @seealso [extract_opportunities()] [check_format_names()]
#'
#' @examples
#' data(county)
#'
#' @export
extract_spatial_information <- function(geometry, 
                                        names = NULL, 
                                        stdist = FALSE) {
  
  # Controls
  if(class(geometry)[1] != "sf"){
    stop("It seems that the geometry used is not an sf object.",
         call. = FALSE)
  }
  
  if(!is.null(names)){
    
  }
  
  controls(args = stdist, type = "boolean")
  
  # Extract distance
  
  dist = sf::st_distance(geometry)
  
  # Extract average surface area
  
  av_surf = mean(sf::st_area(geometry))
  
  # Return output
  res=list(distance = dist, av_surf = av_surf)
  return(res)
  
}
