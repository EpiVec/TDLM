#' @export
#' @method str TDLM
str.TDLM <- function(object, ...)
{
  args <- list(...)
  if(is.null(args$max.level))
  {
    args$max.level <- 2
  }
  NextMethod("str", object = object, max.level = args$max.level)
}

#' @export
#' @method print TDLM
print.TDLM <- function(x, ...)
{
  print(x$info)
}



