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
  for (attr_name in names(prior_attrs)) {
    if (!(attr_name %in% c('row.names',
                           'names'))) {
      attr(out, attr_name) <- prior_attrs[[attr_name]]
    }
  }
  return(out)
}
