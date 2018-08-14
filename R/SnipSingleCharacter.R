#' @title Trim a character vector.
#'
#' @description
#'
#' `SnipSingleCharacter` trims a character vector by 1 character for each vector element.
#'
#' @export
#' @examples SnipSingleCharacter(state.abb, side = 'back')
SnipSingleCharacter <- function(v, side = 'front') {
  stopifnot(class(v) == 'character' && is.vector(v))
  y <- strsplit(v, '')
  if (side == 'front') {
    y <- lapply(y, function(x) x[2:(length(x))])
  } else if (side == 'back') {
    y <- lapply(y, function(x) x[1:(length(x) - 1)])
  } else {
    stop('Side must be either front or back.')
  }
  y <- lapply(y, function(x) paste(x, collapse = ''))
  return(unlist(y))
}
