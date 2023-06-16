#' Only update data in an already existent `tidyabm` object
#'
#' @param prior_object a `tidyabm` object
#' @param new_data a [tibble]
#'
#' @return a `tidyabm` object, just like `prior_object`
retain_new_data_in_prior_object <- function(prior_object,
                                            new_data) {
  out <- structure(new_data)
  prior_attrs <- attributes(prior_object)
  prior_attrs[['row.names']] <- attr(out, 'row.names')
  prior_attrs[['names']] <- attr(out, 'names')
  attributes(out) <- prior_attrs
  return(out)
}

#' Round to integer, but round .5 up
#'
#' @description
#'  R's [round] function rounds .5 to the next even digit as specified in
#'  `IEEE 754`. However, this is not expected here but instead .5 should always
#'  be rounded up. See also http://andrewlandgraf.com/2012/06/15/rounding-in-r/
#'
#' @param n the number to round
#'
#' @return an integer
round_half_up <- function(n) {
  posneg = sign(n)
  z = trunc(abs(n) + 0.5)
  return(z*posneg)
}
