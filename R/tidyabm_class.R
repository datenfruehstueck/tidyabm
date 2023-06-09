# Exported functions ----

#' Set an initial characteristic
#'
#' @description
#'   A characteristic is appended to the respective object right from the start.
#'   It is accessible throughout runtime and can also change if a rule chooses
#'   to do so. However, compared to a variable, it is not calculated/determined
#'   in each iteration.
#'
#'   If you need to roll out a characteristic's distribution across a range of
#'   agents (e.g., a characteristic that's supposed to be normally distributed
#'   across a group or all of the agents), see
#'   [distribute_characteristic_across_agents].
#'
#' @param .tidyabm a `tidyabm` object
#' @param ... <[`data-masking`][rlang::args_data_masking]> name-value pairs.
#'   The name lets you identify the characteristic. To avoid misunderstandings
#'   use unique names, also between agents and environments. Try to also
#'   avoid starting your name with a point/dot (`.`) because some internally
#'   added names use that pattern. Good names also do not have any whitespace
#'   and are all lowercase; use underscores to separate any words. It is
#'   recommended to also avoid the use of minus signs. Hence, a good
#'   characteristic name looks like `this_is_a_great_name`.
#'   The value is treated as a single value (e.g., a character, numeric,
#'   logical, vector ...).
#' @param .overwrite if `FALSE` (the default), characteristics with the same
#'   name will not be overwritten (a warning will be issued)
#' @param .suppress_warnings if `TRUE`, `.overwrite` will not yield any
#'   warnings; default is FALSE, though
#'
#' @return a `tidyabm` object
#'
#' @examples
#' create_grid_environment(seed = 3, size = 5) %>%
#'   set_characteristic(condition = 'small')
#'
#' create_agent() %>%
#'   set_characteristic(age = 15)
#'
#' create_agent() %>%
#'   set_characteristic(age = 15,
#'                      gender = 'm')
#'
#' create_agent() %>%
#'   set_characteristic(embed_vector = c(23.2, 12.4, 4.2, -3.2, 13.2, -0.1))
#'
#' @export
set_characteristic <- function(.tidyabm,
                               ...,
                               .overwrite = FALSE,
                               .suppress_warnings = FALSE) {
  UseMethod('set_characteristic')
}

#' @rdname set_characteristic
#' @export
set_characteristic.tidyabm <- function(.tidyabm,
                                       ...,
                                       .overwrite = FALSE,
                                       .suppress_warnings = FALSE) {
  stopifnot(is_tidyabm(.tidyabm))

  new_characteristics_quo <- dplyr:::dplyr_quosures(...)
  new_characteristics <- list()
  new_characteristics_names <- c()
  for (new_c_name in names(new_characteristics_quo)) {
    new_characteristics <- append(
      new_characteristics,
      list(rlang::eval_tidy(new_characteristics_quo[[new_c_name]]))
    )
    new_characteristics_names <- c(new_characteristics_names, c(new_c_name))
  }
  new_characteristics <- stats::setNames(new_characteristics,
                                         new_characteristics_names)

  stopifnot(length(new_characteristics) > 0)

  new_names <- names(new_characteristics)
  current_characteristics <- attr(.tidyabm, 'characteristics')
  current_names <- names(current_characteristics)

  if (any(new_names %in% current_names)) {
    if (!.suppress_warnings) {
      warning(paste0('The following characteristics already existed. They were ',
                     ifelse(.overwrite, '', 'not '), 'overwritten: ',
                     paste(new_names[new_names %in% current_names],
                           collapse = ', ')),
              call. = FALSE)
    }

    if (.overwrite) {
      current_characteristics <- replace(current_characteristics,
                                         current_names %in% new_names,
                                         new_characteristics[new_names %in% current_names])
    }

    new_characteristics <- new_characteristics[!new_names %in% current_names]
  }

  attr(.tidyabm, 'characteristics') <- c(current_characteristics,
                                         new_characteristics)

  if (is_tidyabm_agent(.tidyabm) | is_tidyabm_env(.tidyabm)) {
    .tidyabm <-
      .tidyabm %>%
      update_values(attr(.tidyabm, 'characteristics'),
                    is_characteristic = TRUE)
  }

  return(.tidyabm)
}


#' Add a flexible variable
#'
#' @description
#'   A variable is expected to updated at various (if not all) iterations. It
#'   gets initiated at the start with the status quo of the model and is
#'   accessible throughout runtime. Compared to a characteristic, it thus
#'   requires runtime resources but is flexibly updated at every iteration.
#'
#' @param .tidyabm a `tidyabm` object
#' @param ... <[`data-masking`][rlang::args_data_masking]> Name-value pairs.
#'   The name gives the name of the variable. To avoid misunderstandings use
#'   unique variable names, also between agents and environments. Try to also
#'   avoid starting your name with a point/dot (`.`) because some internally
#'   added names use that pattern. Good variable names also do not have any
#'   whitespace and are all lowercase; use underscores to separate any words.
#'   It is recommended to also avoid the use of minus signs. Hence, a good
#'   variable name looks like `this_is_a_great_name`.
#'   The value has to be one of:
#'
#'   * a single value (e.g., a character, numeric, logical, vector ...)
#'   * a function to be run at every iteration of the modelling process
#'
#'   Keep in mind that for single values that are not expected to change,
#'   [set_characteristic] is a better-fitting method as it is not re-run at
#'   every iteration of the modelling process.
#'
#'   For functions you may use a function which gets called at every iteration
#'   with two arguments, `me` (which is the `tidyabm` object at that specific
#'   point in time) and `abm` (which is the whole `tidyabm` object). Note that
#'   for `tidyabm_env` objects these two arguments are the same but for
#'   `tidyabm_agent` objects they are different in that the first (`me`) is
#'   the current agent and the second (`abm`) is the whole environment model.
#'   If you write your own functions or need to provide additional arguments to
#'   functions, use the style of anonymized functions directly in-line (through
#'   `function(me, abm) ...` or `\(me, abm) ...`).
#'
#' @param .overwrite if `FALSE` (the default), variables with the same name
#'   will not be overwritten (a warning will be issued)
#'
#' @return a `tidyabm` object
#'
#' @examples
#' create_agent() %>%
#'   set_characteristic(age = 15) %>%
#'   add_variable(share_same_age = function(me, abm) {
#'                 neighbors <- get_neighbors(me)
#'                 return(sum(neighbors$age == me$age)/nrow(neighbors))
#'               }) %>%
#'   add_variable(feels_welcome = \(me, abm) share_same_age >= .50)
#'
#' create_grid_environment(seed = 9896, size = 5) %>%
#'   add_variable(m_age = \(me, abm) mean(convert_agents_to_tibble(me)$age, na.rm = T))
#'
#' @export
add_variable <- function(.tidyabm,
                         ...,
                         .overwrite = FALSE) {
  UseMethod('add_variable')
}

#' @rdname add_variable
#' @export
add_variable.tidyabm <- function(.tidyabm,
                                 ...,
                                 .overwrite = FALSE) {
  stopifnot(is_tidyabm(.tidyabm))
  stopifnot(is.logical(.overwrite))

  new_variables <- list(...)
  stopifnot(length(new_variables) > 0)

  new_names <- names(new_variables)
  current_variables <- attr(.tidyabm, 'variables')
  current_names <- names(current_variables)

  if (any(new_names %in% current_names)) {
    warning(paste0('The following variables already existed. They were ',
                   ifelse(.overwrite, '', 'not '), 'overwritten: ',
                   paste(new_names[new_names %in% current_names],
                         collapse = ', ')),
            call. = FALSE)

    if (.overwrite) {
      current_variables <- replace(current_variables,
                                   current_names %in% new_names,
                                   new_variables[new_names %in% current_names])
    }

    new_variables <- new_variables[!new_names %in% current_names]
  }

  attr(.tidyabm, 'variables') <- c(current_variables,
                                   new_variables)

  return(.tidyabm)

}

#' Add a rule (i.e., an action that is performed under certain conditions)
#'
#' @description
#'   Rules are checked and executed once per tick. Rules for agents are run
#'   before rules for environments. For each rule, the `...` conditions are
#'   checked and, if they prove valid, the rule's `.consequence` is run.
#'
#'   Think of rules as the code to be run within your ABM. Rules could establish
#'   connections or abort them, they could remove or add agents, they could
#'   stop the whole simulation or make agents move.
#'
#'   For helping utilities (i.e., readily implemented helpers to be used in your
#'   rules or as readymade consequences) and examples, see the full
#'   documentation, particularly per environment type.
#'
#' @param .tidyabm a `tidyabm` object
#' @param .label string describing the rule
#' @param ... <[`data-masking`][rlang::args_data_masking]> expressions that
#'   return a logical value and are defined in terms of all characteristics
#'   and variables of `me` (which is the `tidyabm` object to which the rule
#'   was applied to). If multiple expressions are included, they are combined
#'   with the `&` operator.
#' @param .consequence function to be executed if all conditions apply. Use
#'   a function with two arguments, `me` (which is the `tidyabm` object at that
#'   specific point in time) and `abm` (which is the whole `tidyabm` object).
#'   Note that for `tidyabm_env` objects these two arguments are the same but
#'   for `tidyabm_agent` objects they are different in that the first (`me`) is
#'   the current agent and the second (`abm`) is the whole environment model.
#'   **Importantly**, for any changes to apply the function has to return the
#'   updated `me` (i.e., a `tidyabm_agent` object for agent rules and a
#'   `tidyabm_env` object for environment rules). If the function misses to do
#'   so, any changes will be ignored and a warning will be issued during
#'   runtime. If you write your own functions or need to provide additional
#'   arguments to functions, use the style of anonymized functions directly
#'   in-line (through `function(me, abm) ... return(me)` or
#'   `\(me, abm) ... return(me)`).
#'
#' @return a `tidyabm` object
#'
#' @examples
#' create_agent() %>%
#'   set_characteristic(age = 15) %>%
#'   add_rule('minors move',
#'            age >= 18,
#'            .consequence = \(me, abm) {
#'              spot <- grid_get_free_neighboring_spots(me, abm) %>%
#'                dplyr::slice_sample(n = 1)
#'              grid_move(me, abm,
#'                        new_x = spot$.x,
#'                        new_y = spot$.y) %>%
#'                return()
#'            })
#'
#' create_grid_environment(seed = 1269, size = 5) %>%
#'   add_variable(n_agents = \(me, abm) nrow(convert_agents_to_tibble(me))) %>%
#'   add_rule('no more agents',
#'            n_agents == 0,
#'            .consequence = stop_abm)
#'
#' @export
add_rule <- function(.tidyabm,
                     .label,
                     ...,
                     .consequence) {
  UseMethod('add_rule')
}

#' @rdname add_rule
#' @export
add_rule.tidyabm <- function(.tidyabm,
                             .label,
                             ...,
                             .consequence) {
  stopifnot(is_tidyabm(.tidyabm))

  rules <- attr(.tidyabm, 'rules')
  if (.label %in% names(rules)) {
    stop(paste0('A rule with the same label already exists. Either choose a ',
                'different label or remove the existent rule first.'))
  }

  conditions <- dplyr::quos(...)
  dplyr:::check_filter(conditions)
  rules[[.label]] <- list(`if` = conditions,
                         `then` = .consequence)
  attr(.tidyabm, 'rules') <- rules

  return(.tidyabm)
}

#' Remove a certain rule
#'
#' @param .tidyabm a `tidyabm` object
#' @param label string used to describe the rule
#'
#' @return a `tidyabm` object
#'
#' @examples
#'   add_rule('minors move',
#'            age >= 18,
#'            .consequence = \(me, abm) {
#'              spot <- grid_get_free_neighboring_spots(me, abm) %>%
#'                dplyr::slice_sample(n = 1)
#'              grid_move(me, abm,
#'                        new_x = spot$.x,
#'                        new_y = spot$.y) %>%
#'                return()
#'            })
#'   remove_rule('minors move')
#'
#' create_grid_environment(seed = 1271, size = 5) %>%
#'   add_variable(n_agents = \(me, abm) nrow(convert_agents_to_tibble(me))) %>%
#'   add_rule('no more agents',
#'            n_agents == 0,
#'            .consequence = stop_abm) %>%
#'   remove_rule('no more agents')
#'
#' @export
remove_rule <- function(.tidyabm,
                        label) {
  UseMethod('remove_rule')
}

#' @rdname remove_rule
#' @export
remove_rule.tidyabm <- function(.tidyabm,
                                label) {
  stopifnot(is_tidyabm(.tidyabm))

  rules <- attr(.tidyabm, 'rules')
  stopifnot(label %in% names(rules))

  rules[[label]] <- NULL
  attr(.tidyabm, 'rules') <- rules

  return(.tidyabm)
}

#' Check if a provided object is of type `tidyabm`
#'
#' @param x object to check
#'
#' @export
is_tidyabm <- function(x) {
  inherits(x, 'tidyabm')
}


# Internal functions ----

#' General `tidyabm` output constructor
#'
#' @param data a [tibble]
#' @param class_suffix string to suffix sub class name with; has to be one of
#'   ` ` (empty), `env`, or `agent`.
#' @param particular_sub_class if set to character then this is the first class
#'   name to use, even before the suffixed sub class name (`NULL` meaning no
#'   particular sub class is the default)
#' @param class_params list with parameters initially relevant to store
#'
#' @return a `tidyabm` object with optional subclass of `particular_sub_class`
#'   and subclass of `tidyabm_*suffix*`
new_tidyabm <- function(data,
                        class_suffix = '',
                        particular_sub_class = NULL,
                        class_params = list()) {

  stopifnot(tibble::is_tibble(data))
  stopifnot(class_suffix %in% c('',
                                'env',
                                'agent'))
  stopifnot(is.null(particular_sub_class) | is.character(particular_sub_class))
  stopifnot(is.list(class_params))

  structure(data,
            class = c(particular_sub_class,
                      paste0('tidyabm_',
                             class_suffix),
                      'tidyabm',
                      class(data)),
            class_params = class_params,
            characteristics = list(),
            variables = list(),
            rules = list()) %>%
    return()
}


#' Update an actual value from a characteristic or a variable
#'
#' @param .tidyabm a `tidyabm` object
#' @param values a named list where names reference charactistics/variables
#' @param is_characteristic if `FALSE` (the default), values are assumed to
#'   belong to variables rather than characteristics
#'
#' @return a `tidyabm` object
update_values <- function(.tidyabm,
                          values,
                          is_characteristic = FALSE) {
  UseMethod('update_values')
}
