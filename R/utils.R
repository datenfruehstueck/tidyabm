#' Only update data in an already existent [tidyabm] object
#'
#' @param prior_object a [tidyabm] object
#' @param new_data a [tibble]
#'
#' @return a [tidyabm] object, just like `prior_object`
retain_new_data_in_prior_object <- function(prior_object,
                                            new_data) {
  out <- structure(new_data)
  prior_attrs <- attributes(prior_object)
  prior_attrs[['row.names']] <- attr(out, 'row.names')
  prior_attrs[['names']] <- attr(out, 'names')
  attributes(out) <- prior_attrs
  return(out)
}
