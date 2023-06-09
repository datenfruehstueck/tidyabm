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

  all_coordinates_in_random_order <- expand.grid(x = 1:x,
                                                 y = 1:y) %>%
    dplyr::slice_sample(prop = 1)

  tibble::tibble() %>%
    new_tidyabm_env('grid',
                    class_params = list(seed = seed,
                                        x = x,
                                        y = y,
                                        n_fields = x*y,
                                        all_coordinates_random =
                                          all_coordinates_in_random_order,
                                        footer_details = paste0(x, 'x', y))) %>%
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

  cp <- attr(.tidyabm, 'class_params')
  coordinates_random <- cp[['all_coordinates_random']]
  agents <- attr(.tidyabm, 'agents')
  n_agents <- length(agents)
  i <- n_agents - n + 1

  attr(.tidyabm, 'agents') <- purrr::map_if(
    agents,
    c(rep(FALSE, n_agents - n),
      rep(TRUE, n)),
    \(agent) {
      if (is.null(initial_position)) {
        i <<- i + 1
        agent %>%
          set_characteristic(.x = coordinates_random[[i-1, 'x']],
                             .y = coordinates_random[[i-1, 'y']],
                             .overwrite = TRUE)
      } else {
        coordinates <- do.call(initial_position,
                               list(agent, .tidyabm))
        if (!is.vector(coordinates) | length(coordinates) != 2) {
          stop(paste0('The provided function in initial_position is expected to ',
                      'return a numeric vector (length 2) but did not (returned ',
                      typeof(coordinates), ' with length ', length(coordinates),
                      ')'))
        }
        agent %>%
          set_characteristic(.x = coordinates[[1]],
                             .y = coordinates[[2]],
                             .overwrite = TRUE)
      }
    })

  return(.tidyabm)
}

#' @rdname visualize
#'
#' @param color specify an agent characteristic or variable by name to color
#'   agents according to this specific characteristic/variable (default is not
#'   to apply any colors)
#' @param shape specify an agent characteristic or variable by name to shape
#'   agents according to this specific characteristic/variable (default is to
#'   have all dots)
#'
#' @export
visualize.tidyabm_env_grid <- function(.tidyabm,
                                       color = NULL,
                                       shape = NULL) {
  stopifnot(is_tidyabm_env_grid(.tidyabm))

  cp <- attr(.tidyabm, 'class_params')
  point_size <- 4.5 - (0.5*floor(min(cp[['x']],
                                     cp[['y']])/10))
  .tidyabm %>%
    convert_agents_to_tibble() %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.x,
                                 y = .data$.y,
                                 color = {{ color }},
                                 shape = {{ shape }})) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_x_continuous('x\n',
                                limits = c(.5, cp[['x']] + .5),
                                breaks = 1:cp[['x']],
                                labels = 1:cp[['x']],
                                minor_breaks = seq(0.5, cp[['x']] + 0.5, 1),
                                sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_y_reverse('y\n',
                             limits = c(cp[['y']] + 0.5, 0.5),
                             breaks = cp[['y']]:1,
                             labels = cp[['y']]:1,
                             minor_breaks = seq(cp[['y']] + 0.5, 0.5, -1),
                             sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_color_brewer(palette = 'Paired') +
    ggplot2::scale_shape_discrete() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_line(color = '#c2c2c2'),
      axis.text = ggplot2::element_text(color = '#c2c2c2'),
      legend.position = 'bottom',
      legend.box = 'vertical'
    )
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
      !(which %in% c('o', '+', '-', '|'))) {
    return(NULL)
  }

  neighbors <- matrix_get_neighbors(abm, agent$.x, agent$.y)

  # if (which == 'o') {
  # }
  if (which == '+') {
    neighbors <- neighbors[c('N', 'E', 'S', 'W')]
  }
  if (which == '-') {
    neighbors <- neighbors[c('E', 'W')]
  }
  if (which == '|') {
    neighbors <- neighbors[c('N', 'S')]
  }

  return(dplyr::bind_rows(attr(abm, 'agents')[neighbors[neighbors > 0]]))
}

#' Get a [tibble] with two columns (.x and .y) of neighboring free spots
#'
#' @param agent the agent for whom the neighbors should be collected (`me`)
#' @param abm the whole environment model
#' @param which one of the following:
#'
#'   - `o` to get up to 8 neighboring spots surrounding `me` (default)
#'   - `+` to get up to 4 neighboring spots directly next to `me` (no diagonals)
#'   - `-` to get up to 2 x-axis neighboring spots, one on each side of `me`
#'   - `|` to get up to 2 y-axis neighboring spots, one above and one below `me`
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
      !(which %in% c('o', '+', '-', '|'))) {
    return(NULL)
  }

  x <- agent$.x
  y <- agent$.y
  neighbors <- matrix_get_neighbors(abm, x, y)
  free_spots <- names(neighbors[neighbors == 0])

  #if (which == 'o') {
  #}
  if (which == '+') {
    free_spots <- free_spots[free_spots %in% c('N', 'E', 'S', 'W')]
  }
  if (which == '-') {
    free_spots <- free_spots[free_spots %in% c('E', 'W')]
  }
  if (which == '|') {
    free_spots <- free_spots[free_spots %in% c('N', 'S')]
  }

  tibble::tibble(orientation = c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'),
                 .x = c(x, x + 1, x + 1, x + 1, x, x - 1, x - 1, x - 1),
                 .y = c(y - 1, y - 1, y, y + 1, y + 1, y + 1, y, y - 1)) %>%
    dplyr::filter(orientation %in% free_spots) %>%
    dplyr::select(-orientation) %>%
    return()
}

#' Get a [tibble] with two columns (.x and .y) of all free spots
#'
#' @param abm the whole environment model
#'
#' @return a [tibble] of spots (with a `.x` and a `.y` column) in no particular
#'   order that are free
#' @family utilities
#' @export
grid_get_free_spots <- function(abm) {
  if (!is_tidyabm_env(abm)) {
    return(NULL)
  }

  cp <- attr(abm, 'class_params')
  expand.grid(x = 1:cp[['x']],
              y = 1:cp[['y']]) %>%
    tibble::as_tibble() %>%
    dplyr::anti_join(attr(abm, 'agents') %>%
                       lapply(\(x) c(x = x$.x, y = x$.y)) %>%
                       rbind() %>%
                       dplyr::bind_rows(),
                     by = c('x', 'y')) %>%
    return()
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

  cp <- attr(abm, 'class_params')
  if (new_x <= 0 | new_x > cp[['x']] |
      new_y <= 0 | new_y > cp[['y']]) {
    warning(paste0('The new position (', new_x, '/', new_y, ') for agent ',
                   agent$.id, ' does not exist. The agent has thus not moved.'),
            call. = FALSE)
    return(agent)
  }

  if (any(sapply(attr(abm, 'agents'), \(a) all(a$.x == new_x,
                                               a$.y == new_y,
                                               a$.id != agent$.id)))) {
    warning(paste0('The new position (', new_x, '/', new_y, ') for agent ',
                   agent$.id, ' is not empty. The agent has thus not moved.'),
            call. = FALSE)
    return(agent)
  }

  return(agent %>%
           set_characteristic(.x = new_x,
                              .y = new_y,
                              .overwrite = TRUE,
                              .suppress_warnings = TRUE))
}


# Internal functions ----

is_tidyabm_env_grid <- function(x) {
  inherits(x, 'tidyabm_env_grid')
}

#' Get neighbors for a given position (but inside the matrix)
#'
#' @param .tidyabm the [tidyabm_env_grid] object
#' @param x numeric x of the middle
#' @param y numeric y of the middle
#'
#' @return a named length-8 vector with either -1 (field does not exist), 0
#'   (field is empty) or the index (!) of the corresponding agent (to address
#'   them directly via `attr(.tidyabm, 'agents')[[index]]`)
matrix_get_neighbors <- function(.tidyabm, x, y) {
  cp <- attr(.tidyabm, 'class_params')
  m <- convert_agents_to_padded_matrix(.tidyabm)
  ind <- 2:(nrow(m) - 2 + 1)
  neighbors <- rbind(N  = as.vector(m[ind - 1, ind    ]),
                     NE = as.vector(m[ind - 1, ind + 1]),
                     E  = as.vector(m[ind    , ind + 1]),
                     SE = as.vector(m[ind + 1, ind + 1]),
                     S  = as.vector(m[ind + 1, ind    ]),
                     SW = as.vector(m[ind + 1, ind - 1]),
                     W  = as.vector(m[ind    , ind - 1]),
                     NW = as.vector(m[ind - 1, ind - 1]))
  return(neighbors[, convert_xy_to_mpos(cp[['y']], x, y)])
}

convert_agents_to_padded_matrix <- function(.tidyabm) {
  cp <- attr(.tidyabm, 'class_params')

  indices <- tibble::tibble(mpos = 1:cp[['n_fields']]) %>%
    dplyr::left_join(dplyr::bind_rows(attr(.tidyabm, 'agents')) %>%
                       dplyr::mutate(mpos = convert_xy_to_mpos(cp[['y']],
                                                               .data$.x,
                                                               .data$.y),
                                     i = 1:dplyr::n()) %>%
                       dplyr::select(mpos, i),
                     by = 'mpos') %>%
    dplyr::mutate(i = tidyr::replace_na(i, 0)) %>%
    dplyr::pull(i)

  return(rbind(-1,
               cbind(-1,
                     matrix(indices,
                            ncol = cp[['x']],
                            nrow = cp[['y']]),
                     -1),
               -1))
}

convert_xy_to_mpos <- function(nrow_y, x, y) {
  return((x-1)*nrow_y + y)
}

convert_mpos_to_xy <- function(nrow_y, mpos) {
  x <- ceiling(mpos/nrow_y)
  y <- mpos - (x-1)*nrow_y
  return(c(x, y))
}
