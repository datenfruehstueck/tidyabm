# Exported functions ----

#' Create an orthogonal grid environment
#'
#' @description
#'   Creates an environment with a specified two-dimensional grid. Either define
#'   the grid as a square by setting one side's `size` *or* define the grid as a
#'   rectangle by setting both `x` and `y`.
#'
#'   The seed is used to explicitly document random-number generation
#'   initiation in your script.
#'
#' @param seed a single number (integer) to initiate random-number generation
#' @param size number to indicate side length for square
#' @param x numeric side length of side x (omitted if `size` is set)
#' @param y numeric side length of side y (omitted if `size` is set)
#'
#' @return [tidyabm] object
#'
#' @examples
#' create_grid_environment(seed = 42, size = 25)
#'
#' create_grid_environment(seed = 123, x = 25, y = 5)
#'
#' @export
create_grid_environment <- function(seed,
                                    size = NULL,
                                    x = NULL,
                                    y = NULL) {
  if (is.null(size) & is.null(x) & is.null(y)) {
    stop('Either size or x/y are required.')
  }

  if (!is.null(size) & (!is.null(x) | !is.null(y))) {
    stop('Only size *or* x/y are required.')
  }

  if (is.null(size)) {
    if (!is.numeric(x) | !is.numeric(y) | x <= 0 | y <= 0) {
      stop('Both x and y must be numeric and greater than 0.')
    }
  } else {
    if (!is.numeric(size) | size <= 0) {
      stop('Size must be numeric and greater than 0.')
    }
  }

  stopifnot(is.numeric(seed))
  set.seed(seed)

  if (!is.null(size)) {
    x <- size
    y <- size
  }

  tibble::tibble() %>%
    new_tidyabm_env('grid',
                    class_params = list(seed = seed,
                                        x = x,
                                        y = y,
                                        n_fields = x*y)) %>%
    return()
}


#' @rdname add_agents
#' @param initial_position either `NULL` (the default) where agents are placed
#'   randomly and independent from each other or a function with two parameters
#'   `me` (the agent to be placed) and `abm` (the current environment with
#'   previous agents already placed) that gets called once per agent to place
#'   and which has to return a vector of length 2 that will be interpreted as
#'   `x` and `y` coordinates (i.e., `c(x, y)`).
#'   Here is an example to position agents in subsequent order via a function:
#'   coordinate_counter <- c(0, 1)
#'   create_grid_environment(seed = 892,
#'                           size = 3) %>%
#'     add_agents(create_agent(),
#'                n = 3,
#'                initial_position = \(me, abm) {
#'                  coordinate_counter[1] <- coordinate_counter[1] + 1
#'                  if (coordinate_counter[1] > 3) {
#'                    coordinate_counter[1] <- 1
#'                    coordinate_counter[2] <- coordiante_counter[2] + 1
#'                  }
#'                  return(coordinate_counter)
#'                })
#'
#' @export
add_agents.tidyabm_env_grid <- function(.tidyabm,
                                        agent,
                                        n,
                                        initial_position = NULL,
                                        ...) {
  .tidyabm <- NextMethod()

  if (!is.null(initial_position) & typeof(initial_position) != 'closure') {
    stop('initial_position has to be either NULL (default) to position agents ',
         'randomly or a function (with parameters me and abm) to be called for',
         ' for each agent')
  }

  if (length(attr(.tidyabm, 'agents')) >
      attr(.tidyabm, 'class_params')[['n_fields']]) {
    stop(paste0('This grid has ', attr(.tidyabm, 'class_params')[['n_fields']],
                ' fields but you are trying to add too many agents (a ',
                'then-total of ', length(attr(.tidyabm, 'agents')) + n, ')'))
  }

  agents <- attr(.tidyabm, 'agents')
  agents_new <- list()
  if (length(agents) > n) {
    agents_new <- agents[1:n]
  }

  cp <- attr(.tidyabm, 'class_params')
  coordinates_random <- expand.grid(x = 1:cp[['x']],
                                    y = 1:cp[['y']]) %>%
    dplyr::slice_sample(prop = 1)

  for (i in seq(length(agents) - n + 1,
                length(agents))) {
    agent_placed <- agents[[i]]
    if (is.null(initial_position)) {
      agent_placed <- agent_placed %>%
        set_characteristic(.x = coordinates_random[[i, 'x']],
                           .y = coordinates_random[[i, 'y']],
                           .overwrite = TRUE)
    } else {
      coordinates <- do.call(initial_position,
                             list(agent_placed, .tidyabm))
      if (!is.vector(coordinates) | length(coordinates) != 2) {
        stop(paste0('The provided function in initial_position is expected to ',
                    'return a numeric vector (length 2) but did not (returned ',
                    typeof(coordinates), ' with length ', length(coordinates),
                    ')'))
      }
      agent_placed <- agent_placed %>%
        set_characteristic(.x = coordinates[[1]],
                           .y = coordinates[[2]],
                           .overwrite = TRUE)
    }
    agents_new <- append(agents_new,
                         list(agent_placed))
  }

  attr(.tidyabm, 'agents') <- agents_new

  return(.tidyabm)
}


# External Utils ----

#' Get a [tibble] of direct neighbors
#'
#' @param agent the agent for whom the neighbors should be collected (`me`)
#' @param abm the whole environment model
#' @param which one of the following:
#'
#'   - `o` to get up to 8 neighbors surrounding `me` (default)
#'   - `+` to get up to 4 neighbors directly next to `me` (no diagonals)
#'   - `-` to get up to 2 x-axis neighbors, one on each side of `me`
#'   - `|` to get up to 2 y-axis neighbors, one above and one below `me`
#'   - `--` to get up to 4 x-axis neighbors, two on each side of `me`
#'     (this actually works up to `-----`)
#'   - `||` to get up to 4 y-axis neighbors, two above and two below `me`
#'     (this actually works up to `|||||`)
#'
#' @return a [tibble] of neighboring agents in no particular order with their
#'   characteristics and variables set
#' @family utilities
#' @export
grid_get_neighbors <- function(agent,
                               abm,
                               which = 'o') {
  if (!is_tidyabm_agent(agent) |
      !is_tidyabm_env(abm) |
      !(which %in% c('o',
                     '+',
                     '-', '--', '---', '----', '-----',
                     '|', '||', '|||', '||||', '|||||'))) {
    return(NULL)
  }

  if (which == 'o') {
    abm %>%
      convert_agents_to_tibble() %>%
      dplyr::filter(.data$.x %in% c(agent$.x, agent$.x - 1, agent$.x + 1),
                    .data$.y %in% c(agent$.y, agent$.y - 1, agent$.y + 1)) %>%
      return()
  }

  if (which == '+') {
    abm %>%
      convert_agents_to_tibble() %>%
      dplyr::filter(
        (.data$.y == agent$.y & .data$.x == agent$.x - 1 | .data$.x == agent$.x + 1) |
        (.data$.x == agent$.x & .data$.y == agent$.y - 1 | .data$.y == agent$.y + 1)
      ) %>%
      return()
  }

  if (substr(which, 1, 1) == '-') {
    offset <- length(which)
    abm %>%
      convert_agents_to_tibble() %>%
      dplyr::filter(.data$.y == agent$.y,
                    .data$.x == agent$.x - offset | .data$.x == agent$.x + offset) %>%
      return()
  }

  if (substr(which, 1, 1) == '|') {
    offset <- length(which)
    abm %>%
      convert_agents_to_tibble() %>%
      dplyr::filter(.data$.x == agent$.x,
                    .data$.y == agent$.y - offset | .data$.y == agent$.y + offset) %>%
      return()
  }
}

#' Get a [tibble] with two columns (.x and .y) of spots that are free
#'
#' @param agent the agent for whom the neighbors should be collected (`me`)
#' @param abm the whole environment model
#' @param which one of the following:
#'
#'   - `o` to get up to 8 neighboring spots surrounding `me` (default)
#'   - `+` to get up to 4 neighboring spots directly next to `me` (no diagonals)
#'   - `-` to get up to 2 x-axis neighboring spots, one on each side of `me`
#'   - `|` to get up to 2 y-axis neighboring spots, one above and one below `me`
#'   - `--` to get up to 4 x-axis neighboring spots, two on each side of `me`
#'     (this actually works up to `-----`)
#'   - `||` to get up to 4 y-axis neighboring spots, two above and two below
#'     `me` (this actually works up to `|||||`)
#'
#' @return a [tibble] of neighboring spots (with a `.x` and a `.y` column) in
#'   no particular order that are free
#' @family utilities
#' @export
grid_get_free_neighboring_spots <- function(agent,
                                            abm,
                                            which = 'o') {
  if (!is_tidyabm_agent(agent) |
      !is_tidyabm_env(abm) |
      !(which %in% c('o',
                     '+',
                     '-', '--', '---', '----', '-----',
                     '|', '||', '|||', '||||', '|||||'))) {
    return(NULL)
  }

  g <- expand.grid(.x = 1:attr(abm, 'class_params')[['x']],
                   .y = 1:attr(abm, 'class_params')[['x']]) %>%
    dplyr::left_join(grid_get_neighbors(agent,
                                        abm,
                                        which) %>%
                       dplyr::select(.data$.x, .data$.y, .data$.id),
                     by = c('.x', '.y')) %>%
    dplyr::filter(!is.null(.id)) %>%
    dplyr::select(.data$.x, .data$.y)
  if (which == 'o') {
    g %>%
      dplyr::filter(.data$.x %in% c(agent$.x, agent$.x - 1, agent$.x + 1),
                    .data$.y %in% c(agent$.y, agent$.y - 1, agent$.y + 1)) %>%
      return()
  }

  if (which == '+') {
    g %>%
      dplyr::filter(
        (.data$.y == agent$.y & .data$.x == agent$.x - 1 | .data$.x == agent$.x + 1) |
          (.data$.x == agent$.x & .data$.y == agent$.y - 1 | .data$.y == agent$.y + 1)
      ) %>%
      return()
  }

  if (substr(which, 1, 1) == '-') {
    offset <- length(which)
    g %>%
      dplyr::filter(.data$.y == agent$.y,
                    .data$.x == agent$.x - offset | .data$.x == agent$.x + offset) %>%
      return()
  }

  if (substr(which, 1, 1) == '|') {
    offset <- length(which)
    g %>%
      dplyr::filter(.data$.x == agent$.x,
                    .data$.y == agent$.y - offset | .data$.y == agent$.y + offset) %>%
      return()
  }
}

#' Move an agent to the proposed new x/y position (if it is empty)
#'
#' @description
#'   If the new position is not empty or does not exist, the agent will not
#'   move (obviously) and a warning will be issued.
#'
#' @param agent the agent for whom the neighbors should be collected (`me`)
#' @param abm the whole environment model (`abm`)
#'
#' @return a [tidyabm_agent] object
#' @family utilities
#' @export
grid_move <- function(agent,
                      abm,
                      new_x,
                      new_y) {
  if (!is_tidyabm_agent(agent)) {
    return(NULL)
  }

  if (new_x <= 0 | new_x > attr(abm, 'class_params')[['x']] |
      new_y <= 0 | new_y > attr(abm, 'class_params')[['y']]) {
    warning(paste0('The new position (', new_x, '/', new_y, ') for agent ',
                   agent$.id, ' does not exist. The agent has thus not moved.'),
            call. = FALSE)
    return(agent)
  }

  if (abm %>%
        convert_agents_to_tibble() %>%
        dplyr::filter(.data$.x == new_x,
                      .data$.y == new_y,
                      .data$.id != agent$.id) %>%
        nrow() > 0) {
    warning(paste0('The new position (', new_x, '/', new_y, ') for agent ',
                   agent$.id, ' is not empty. The agent has thus not moved.'),
            call. = FALSE)
    return(agent)
  }

  agent %>%
    set_characteristic(.x = new_x,
                       .y = new_y,
                       .overwrite = TRUE) %>%
    return()
}


# Internal functions ----

is_tidyabm_env_grid <- function(x) {
  inherits(x, 'tidyabm_env_grid')
}
