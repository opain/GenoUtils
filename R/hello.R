#' @title Hello
#' @description
#' Prints 'Hello' followed by a person's name
#'
#' @param x The name of the person to say hello to.
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("John")
#'
#' \dontrun{
#' hello("Steve")
#' }
hello <- function(x) {
  print(paste0("Hello ", x))
}
