# Exported functions ----

#' Create an agent
#'
#' @description
#'   This function is used to create agent blueprints that have to, in a
#'   second step, be added to an environment through [add_agents].
#'
#' @return a `tidyabm` object of subclass `tidyabm_agent`
#'
#' @examples
#' create_agent()
#'
#' @export
create_agent <- function() {
  tibble::tibble() %>%
    new_tidyabm(class_suffix = 'agent') %>%
    return()
}

#' Check if a provided object is of type `tidyabm_agent`
#'
#' @param x object to check
#'
#' @export
is_tidyabm_agent <- function(x) {
  inherits(x, 'tidyabm_agent')
}



# Formatting ----

#' @export
tbl_format_footer.tidyabm_agent <- function(x, ...) {
  default_footer <- NextMethod()

  return(c(default_footer,
           pillar::style_subtle('# ABM agent'),
           pillar::style_subtle(paste0('* ',
                                       length(attr(x, 'characteristics')),
                                       ' agent characteristic(s), ')),
           pillar::style_subtle(paste0('* ',
                                       length(attr(x, 'variables')),
                                       ' agent variable(s), ')),
           pillar::style_subtle(paste0('* ',
                                       length(attr(x, 'rules')),
                                       ' agent rule(s), '))
           ))
}


# Internal functions ----

#' @rdname update_values
update_values.tidyabm_agent <- function(.tidyabm,
                                        values,
                                        is_characteristic = FALSE) {
  stopifnot(is_tidyabm_agent(.tidyabm))
  stopifnot(is.list(values))

  new_data <- tibble::as_tibble(.tidyabm)

  for (value_name in names(values)) {
    if (is.vector(values[[value_name]]) &
        length(values[[value_name]]) > 1) {

      values[[value_name]] <- list(values[[value_name]])
    }
    if (nrow(new_data) == 0) {
      new_data <- tibble::tibble(!!value_name := values[[value_name]])
    } else {
      new_data <- new_data %>%
        dplyr::mutate(!!value_name := values[[value_name]])
    }
  }

  .tidyabm %>%
    retain_new_data_in_prior_object(new_data) %>%
    return()
}
