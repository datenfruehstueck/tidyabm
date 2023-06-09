# Exported functions ----

#' Add agents to the environment
#'
#' @description
#'   Create an agent first (through [create_agent]). This agent can be seen as
#'   a blueprint. Then, use this function here to add as many replicates of this
#'   blueprint agent as necessary. They will be added independently so that they
#'   do not interfere with each other throughout runtime.
#'
#' @param .tidyabm the `tidyabm_env` object to which agents should be added
#' @param agent an `tidyabm_agent` object (the blueprint) to replicate from
#' @param n number of agents in the style of `agent` to add
#' @param ... other arguments to pass to sub functions
#'
#' @return `tidyabm` object
#'
#' @examples
#' a1 <- create_agent() %>%
#'   set_characteristic(age = 'young')
#' a2 <- create_agent() %>%
#'   set_characteristic(age = 'old')
#' m <- create_grid_environment(seed = 1024, size = 50) %>%
#'   add_agents(a1, 100) %>%
#'   add_agents(a2, 100)
#'
#' @export
add_agents <- function(.tidyabm,
                       agent,
                       n,
                       ...) {
  UseMethod('add_agents')
}

#' @rdname add_agents
#' @export
add_agents.tidyabm_env <- function(.tidyabm,
                                   agent,
                                   n,
                                   ...) {
  stopifnot(is_tidyabm_env(.tidyabm))
  stopifnot(is_tidyabm_agent(agent))
  stopifnot(n > 0)

  # add agent characteristic and variable names to respective vectors
  # to be easily able to generate tibbles later on
  cp <- attr(.tidyabm, 'class_params')
  cp[['agent_characteristics']] <- unique(c(cp[['agent_characteristics']],
                                            names(attr(agent,
                                                       'characteristics'))))
  if (!('agent_characteristics' %in% names(cp))) {
    cp <- append(cp,
                 list(agent_characteristics = c()))
  }
  cp[['agent_variables']] <- unique(c(cp[['agent_variables']],
                                            names(attr(agent,
                                                       'variables'))))
  if (!('agent_variables' %in% names(cp))) {
    cp <- append(cp,
                 list(agent_variables = c()))
  }
  attr(.tidyabm, 'class_params') <- cp

  # add agents
  agents <- attr(.tidyabm, 'agents')
  agents_new <- purrr::map(as.list(seq(length(agents) + 1,
                                       length(agents) + n)),
                           \(i) {
                             agent %>%
                               set_characteristic(.id = paste0('A', i))
                           })
  attr(.tidyabm, 'agents') <- append(agents, agents_new)
  return(.tidyabm)
}

#' Distribute a given characteristic across a range of agents
#'
#' @description
#'   This function allows you to set a characteristic for a set of agents that
#'   needs is initially dependent on each other (e.g., a normally distributed
#'   characteristic). If you just want to set an individual characteristic to
#'   agents, use [set_characteristic] instead as it is much faster.
#'
#' @param .tidyabm the `tidyabm_env` holding the (range of) agents
#' @param .name character string representing the name of the new characteristic
#' @param .value a vector of the same length as the range of agents or a
#'   (anonymous) function to be called for each agent.
#'   For functions you may use a function which gets called for every agent
#'   with three arguments: `i` (a numeric iterator ranging from 1 for the first
#'   agent to n for the last agent), `agent` (the respective `tidyabm` agent),
#'   and `agents` (a [tibble] resembling all filtered agents *before*
#'   distribution of this characteristic started but *after* filter expressions
#'   were applied, see `...`). The function should only return one value of
#'   the characteristic, namely the one for the current/respective agent.
#'   If you write your own functions or need to provide additional arguments to
#'   functions, use the style of anonymized functions directly in-line (through
#'   `function(i, agent, agents) ...` or `\(i, agent, agents) ...`).
#' @param ... <[`data-masking`][rlang::args_data_masking]> expressions that
#'   return a logical value and are defined in terms of all characteristics
#'   and variables of all added agents. This is to specify which agents get the
#'   new characteristic. If omitted, all present agents are receiving the new
#'   characteristic. If multiple expressions are included, they are combined
#'   with the `&` operator.
#' @param .overwrite if `FALSE` (the default), characteristics with the same
#'   name will not be overwritten (a warning will be issued)
#'
#' @return `tidyabm` object
#'
#' @examples
#' create_grid_environment(seed = 168, size = 20) %>%
#'   add_agents(create_agent(), 160) %>%
#'   distribute_characteristic_across_agents(
#'     'friend',
#'     \(i, agent, agents) ifelse(stats::runif(1) > 0.5, i-1, 0)
#'   )
#'
#' a1 <- create_agent() %>%
#'   set_characteristic(age = 'young')
#' a2 <- create_agent() %>%
#'   set_characteristic(age = 'old')
#' m <- create_grid_environment(seed = 486, size = 50) %>%
#'   add_agents(a1, 100) %>%
#'   add_agents(a2, 100) %>%
#'   distribute_characteristic_across_agents(
#'     'opinion',
#'     stats::rnorm(100),
#'     age == 'old'
#'   )
#'
#' @export
distribute_characteristic_across_agents <- function(.tidyabm,
                                                    .name,
                                                    .value,
                                                    ...,
                                                    .overwrite = FALSE) {
  UseMethod('distribute_characteristic_across_agents')
}

#' @rdname distribute_characteristic_across_agents
#' @export
distribute_characteristic_across_agents.tidyabm_env <-
  function(.tidyabm,
           .name,
           .value,
           ...,
           .overwrite = FALSE) {
  stopifnot(is_tidyabm_env(.tidyabm))
  stopifnot(is.character(.name))
  stopifnot(is.vector(.value) | typeof(.value) == 'closure')
  stopifnot(is.logical(.overwrite))

  agents_filtered <- convert_agents_to_tibble(.tidyabm) %>%
    dplyr::filter(...)

  if (is.vector(.value)) {
    if (nrow(agents_filtered) != length(.value)) {
      stop('`.value` has size ', length(.value),
           ' but must match the number of filtered agents which is ',
           nrow(agents_filtered))
    }
  }

  agents <- attr(.tidyabm, 'agents')
  counter <- 1
  for (id in agents_filtered$.id) {
    i <- as.numeric(substring(id, 2))
    value_rendered <- ifelse(is.vector(.value),
                             .value[counter],
                             do.call(.value,
                                     list(counter,
                                          agents[[i]],
                                          agents_filtered)))
    agents[[i]] <- agents[[i]] %>%
      set_characteristic(!!.name := {{value_rendered}},
                         .overwrite = .overwrite)
    counter <- counter + 1
  }
  attr(.tidyabm, 'agents') <- agents

  # add new characteristic to set of characteristics
  cp <- attr(.tidyabm, 'class_params')
  cp[['agent_characteristics']] <- unique(c(cp[['agent_characteristics']],
                                            .name))
  attr(.tidyabm, 'class_params') <- cp

  return(.tidyabm)
}

#' Initialize the environment and prepare everything for take-off
#'
#' @param .tidyabm the `tidyabm_env` object
#'
#' @return a `tidyabm_env` object
#'
#' @examples
#' create_grid_environment(seed = 46444, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init()
#'
#' @export
init <- function(.tidyabm) {
  UseMethod('init')
}

#' @rdname init
#' @export
init.tidyabm_env <- function(.tidyabm) {
  stopifnot(is_tidyabm_env(.tidyabm))

  if (!is.null(attr(.tidyabm, 'runtime'))) {
    stop(paste0('Environment has already been initiated. If you want to reset ',
                'it, use "reset".'))
  }

  attr(.tidyabm, 'runtime') <- list(initiated = lubridate::now(),
                                    next_tick = 1)

  return(.tidyabm)
}

#' Reset an already initiated environment
#'
#' @description
#'  This function erases any model runs, runtime information, and any results.
#'  It works a bit like [init] but for already started/run/initiated
#'  environments. After a reset, an additional [init] is necessary.
#'
#' @param .tidyabm the `tidyabm_env` object
#'
#' @return a `tidyabm_env` object
#'
#' @examples
#' create_grid_environment(seed = 46444, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   tick() %>%
#'   reset() %>%
#'   init()
#'   tick()
#'
#' @export
reset <- function(.tidyabm) {
  UseMethod('reset')
}

#' @rdname reset
#' @export
reset.tidyabm_env <- function(.tidyabm) {
  stopifnot(is_tidyabm_env(.tidyabm))

  if (is.null(attr(.tidyabm, 'runtime'))) {
    stop('Environment has not yet been initiated. Nothing to reset.')
  }

  attr(.tidyabm, 'runtime') <- NULL

  return(.tidyabm)
}

#' Perform one single iteration
#'
#' @description
#'   A single iterations does the following (in this order):
#'
#'   1. randomize agent order
#'   2. go through randomized agents and ...
#'      a. re-calculate agent variables (in the order they were added)
#'      b. check agent rules (in the order they were added)
#'   3. re-calculate environment variables (in the order they were added)
#'   4. check environment rules (in the order they were added)
#'
#' @param .tidyabm the `tidyabm_env` object
#' @param verbose if TRUE (the default), a status message is printed at the end
#' @param visualize if TRUE, each tick ends with a visualization if this has
#'   been implemented for the respective environment; default is FALSE
#' @param ... further arguments to pass on to visualize
#'
#' @return a `tidyabm_env` object
#' @seealso [iterate]
#'
#' @examples
#' create_grid_environment(seed = 46444, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   tick()
#'
#' @export
tick <- function(.tidyabm,
                 verbose = TRUE,
                 visualize = FALSE,
                 ...) {
  UseMethod('tick')
}

#' @rdname tick
#' @export
tick.tidyabm_env <- function(.tidyabm,
                             verbose = TRUE,
                             visualize = FALSE,
                             ...) {
  stopifnot(is_tidyabm_env(.tidyabm))
  if (is.null(attr(.tidyabm, 'runtime'))) {
    stop('Environment has not yet been initiated. Call "init" first.')
  }

  if (!is_tickable(.tidyabm)) {
    warning('Environment is not iterable at this point. Has it ended already?',
            call. = FALSE)
    return(.tidyabm)
  }

  tick_start <- lubridate::now()
  cp <- attr(.tidyabm, 'class_params')
  rt <- attr(.tidyabm, 'runtime')

  # 1. randomize agent order
  agents <- attr(.tidyabm, 'agents')
  agents_length <- length(agents)
  agents_randomized_indices <- sample(1:agents_length,
                                      size = agents_length)

  # 2a. go through agents and ... re-calculate agent variables (in added order)
  if (length(cp[['agent_variables']]) > 0) {
    for (agent_variable_name in cp[['agent_variables']]) {
      purrr::map(agents_randomized_indices,
                 \(i) {
                   agent_variables <- attr(agents[[i]], 'variables')
                   if (agent_variable_name %in% names(agent_variables)) {
                     a_variable <- agent_variables[[agent_variable_name]]
                     if (typeof(a_variable) == 'closure') {
                       a_variable <- do.call(a_variable,
                                             list(agents[[i]],
                                                  .tidyabm))
                     }
                     agents[[i]] <<- agents[[i]] %>%
                       update_values(stats::setNames(list(a_variable),
                                                     c(agent_variable_name)))
                   }
                 })
    }
  }

  # 2b. go through agents and ... check agent rules (in added order)
  purrr::map(agents_randomized_indices,
             \(i) {
               agent_rules <- attr(agents[[i]], 'rules')
               purrr::map(
                 names(agent_rules),
                 \(agent_rule_label) {
                   agent_rule_if <- agent_rules[[agent_rule_label]][['if']]
                   agent_rule_then <- agent_rules[[agent_rule_label]][['then']]
                   if (nrow(dplyr::filter(agents[[i]],
                                          !!!agent_rule_if)) == 1) {
                     agent_temp <- do.call(agent_rule_then,
                                           list(agents[[i]],
                                                .tidyabm))
                     if (is_tidyabm_agent(agent_temp)) {
                       agents[[i]] <<- agent_temp
                     } else {
                       warning(paste0('Agent ', agent_temp$.id, ': Rule "',
                                      agent_rule_label, '" was initiated but ',
                                      'did not return a valid agent object. ',
                                      'It has thus been ignored.'),
                               call. = FALSE)
                     }
                   }
                 }
               )
             })

  ## intermediate agent save for environment to be able to do things
  attr(.tidyabm, 'agents') <- agents

  # 3. re-calculate environment variables (in the order they were added)
  env_variables <- attr(.tidyabm, 'variables')
  for (env_variable_name in names(env_variables)) {
    env_variable <- env_variables[[env_variable_name]]
    if (typeof(env_variable) == 'closure') {
      env_variable <- do.call(env_variable,
                              list(.tidyabm,
                                   .tidyabm))
    }
    .tidyabm <- .tidyabm %>%
      update_values(stats::setNames(list(env_variable),
                                    c(env_variable_name)))
  }

  ## prepare an intermediate .tidyabm so that environment rules can build on it
  .tidyabm_temp <- .tidyabm %>%
    retain_new_data_in_prior_object(
      .tidyabm %>%
        dplyr::bind_rows(tibble::tibble(.tick = rt[['next_tick']]) %>%
                           dplyr::bind_cols(attr(.tidyabm,
                                                 'characteristics')) %>%
                           dplyr::bind_cols(attr(.tidyabm,
                                                 'variables_current_values')))
    )


  # 4. check environment rules (in the order they were added)
  env_rules <- attr(.tidyabm, 'rules')
  purrr::map(
    names(env_rules),
    \(env_rule_label) {
      env_rule_if <- env_rules[[env_rule_label]][['if']]
      env_rule_then <- env_rules[[env_rule_label]][['then']]
      if (nrow(dplyr::filter(.tidyabm_temp,
                             .data$.tick == rt[['next_tick']],
                             !!!env_rule_if)) == 1) {
        env_temp <- do.call(env_rule_then,
                            list(.tidyabm_temp,
                                 .tidyabm_temp))
        if (is_tidyabm_env(env_temp)) {
          .tidyabm <<- env_temp
        } else {
          warning(paste0('Environment rule "', env_rule_label, '" was ',
                         'initiated but did not return a valid environment ',
                         'object. It has thus been ignored.'),
                  call. = FALSE)
        }
      }
    })

  # wrap-up (add data, set next tick, end if adequate ...)
  cp <- attr(.tidyabm, 'class_params')
  rt <- attr(.tidyabm, 'runtime')

  tick_end <- lubridate::now()
  tick_diff <- tick_end - tick_start

  new_data <- .tidyabm
  if (nrow(new_data) > 0) {
    # remove any intermediate leftovers
    new_data <- new_data %>%
      dplyr::filter(.data$.tick < rt[['next_tick']])
  }

  .tidyabm <- .tidyabm %>%
    retain_new_data_in_prior_object(
      new_data %>%
        dplyr::bind_rows(
          tibble::tibble(.tick = rt[['next_tick']],
                         .runtime = tick_diff,
                         .n_agents_after_tick = length(attr(.tidyabm,
                                                            'agents')),
                         .finished_after_tick = is_ended(.tidyabm)) %>%
            dplyr::bind_cols(attr(.tidyabm,
                                  'characteristics')) %>%
            dplyr::bind_cols(attr(.tidyabm,
                                  'variables_current_values'))
        )
    )

  if (verbose) {
    print(paste0('Tick ', rt[['next_tick']], ' finished in ',
                 round(tick_diff, 3), ' ', attr(tick_diff, 'unit'),
                 ifelse(length(colnames(.tidyabm)) > 4, ':', '')))
    for (var_name in colnames(.tidyabm)) {
      if (substr(var_name, 1, 1) != '.') {
        print(paste0('  ', var_name, ': ',
                     .tidyabm[[nrow(.tidyabm), var_name]]))
      }
    }
  }

  rt[['next_tick']] <- rt[['next_tick']] + 1
  attr(.tidyabm, 'runtime') <- rt

  if (visualize) {
    print(visualize(.tidyabm, ...))
  }

  return(.tidyabm)
}

#' Take off and iterate
#'
#' @description
#'   This is the main function to run through your model (i.e., the
#'   environment with all the agents in it). This function is just a loop
#'   that calls [tick] again and again until a stop signal is sent or until
#'   the `max_iterations` limit has been reached.
#'
#'   **CAUTION**: This may take a while!
#'
#'   A freshly [init]'ed environment starts with the first iteration, of course.
#'   However, you can also continue iterating a model. The model remembers the
#'   tick it stopped and proceeds there the next time (see examples).
#'
#'   For more information on how an individual [tick] is structured, see the
#'   documentation there.
#'
#' @param .tidyabm the `tidyabm_env` object
#' @param max_iterations number of iterations after which iterations stop (if
#'   it was not stopped earlier by other means of convergence, e.g. through
#'   environment rules).
#' @param verbose if TRUE (the default), a status message is printed at the end
#' @param visualize if TRUE, each tick ends with a visualization if this has
#'   been implemented for the respective environment; default is FALSE
#' @param ... further arguments to pass on to visualize
#'
#' @return a `tidyabm_env` object
#' @seealso [tick]
#'
#' @examples
#' create_grid_environment(seed = 4583, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   iterate()
#'
#' m <- create_grid_environment(seed = 321456, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   iterate(max_iterations = 10)
#'
#' # continue with the 11th iteration and iterate all the way through to #15
#' m %>%
#'   iterate(max_iterations = 5)
#'
#' @export
iterate <- function(.tidyabm,
                    max_iterations = 50,
                    verbose = TRUE,
                    visualize = FALSE,
                    ...) {
  UseMethod('iterate')
}

#' @rdname iterate
#' @export
iterate.tidyabm_env <- function(.tidyabm,
                                max_iterations = 50,
                                verbose = TRUE,
                                visualize = FALSE,
                                ...) {
  stopifnot(is_tidyabm_env(.tidyabm))
  if (is.null(attr(.tidyabm, 'runtime'))) {
    stop('Environment has not yet been initiated. Call "init" first.')
  }
  if (!is_tickable(.tidyabm)) {
    stop('Environment is not iterable at this point. Has it ended already?')
  }
  if (!is.numeric(max_iterations) |
      max_iterations <= 0 |
      is.na(max_iterations)) {

    stop(paste0('"max_iterations" has to be set and it has to be a positive ',
                'number. You may set it to a very high number, though. ',
                'You can also continue your iteration process by just calling ',
                'this function again (without any reset in between).'))
  }

  starting_tick <- attr(.tidyabm, 'runtime')[['next_tick']]
  max_ending_tick <- starting_tick + max_iterations - 1

  purrr::map(starting_tick:max_ending_tick,
             \(current_tick) {
               if (is_tickable(.tidyabm)) {
                 .tidyabm <<- .tidyabm %>%
                   tick(verbose = verbose,
                        visualize = visualize,
                        ...)
               }
             })

  return(.tidyabm)
}

#' Visualize an environment's current state
#'
#' @description
#'   Visualization highly depends on the particular type of environment.
#'   For a grid environment, for example, visualization resembles a rectangle
#'   with agents placed in it.
#'   The function returns a [ggplot2::ggplot] object, ready to be further
#'   visualized/styled/manipulated.
#'
#' @param .tidyabm the `tidyabm_env` object
#' @param ... other arguments passed to particular types of environment
#'
#' @return a [ggplot2::ggplot] object
#'
#' @examples
#' create_grid_environment(seed = 4583, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   tick() %>%
#'   visualize()
#'
#' @export
visualize <- function(.tidyabm, ...) {
  UseMethod('visualize')
}


#' Extract compact information for an ODD protocol
#'
#' @description
#'   ODD (Overview, Design concepts and Details) protocols are a suggested
#'   standard for describing agent-based models (Grimm et al. 2006, 2010, 2020).
#'   As Grimm et al. (2020) state, "ODD model descriptions [...] are based on
#'   written text and intended to be read by humans." This function provides all
#'   the details the environment and agent specifications contain that might be
#'   relevant for the ODD to be written. It returns the 2020 ODD framework with
#'   all the collected information pre-filled into the respective category.
#'
#' @param .tidyabm the `tidyabm_env` object
#' @param ... other arguments passed to particular types of environment
#'
#' @return a [tibble] of length 7 (the ODD categories)
#'
#' @examples
#' create_grid_environment(seed = 4583, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   init() %>%
#'   iterate() %>%
#'   odd()
#'
#' @references Grimm, V., U. Berger, F. Bastiansen, S. Eliassen, V. Ginot, J.
#'   Giske, J. Goss-Custard, T. Grand, S. Heinz, G. Huse, A. Huth, J. U. Jepsen,
#'   C. Jørgensen, W. M. Mooij, B. Müller, G. Pe’er, C. Piou, S. F. Railsback,
#'   A. M. Robbins, M. M. Robbins, E. Rossmanith, N. Rüger, E. Strand, S.
#'   Souissi, R. A. Stillman, R. Vabø, U. Visser, & D. L. DeAngelis (2006). A
#'   standard protocol for describing individual-based and agent-based models.
#'   Ecological Modelling, 198, 115-296. 10.1016/j.ecolmodel.2006.04.023
#'
#'   Grimm, V., U. Berger, D. L. DeAngelis, J. G. Polhill, J. Giske, & S. F.
#'   Railsback (2010). The ODD protocol: A review and first update. Ecological
#'   Modelling 221, 2760-2768. 10.1016/j.ecolmodel.2010.08.019.
#'
#'   Grimm, V., S. F. Railsback, C. E. Vincenot, U. Berger, C. Gallagher, D. L.
#'   DeAngelis, B. Edmonds, J. Ge, J. Giske, J. Groeneveld, A. S. A. Johnston,
#'   A. Milles, J. Nabe-Nielsen, G. Polhill, V. Radchuk, M.-S. Rohwäder, R. A.
#'   Stillman, J. C. Thiele, & D. Ayllón (2020). The ODD Protocol for describing
#'   agent-based and other simulation models: A second update to improve
#'   clarity, replication, and structural realism. Journal of Artificial
#'   Societies and Social Simulation, 23(2), 7. 10.18564/jasss.4259
#'
#' @export
odd <- function(.tidyabm, ...) {
  UseMethod('odd')
}

#' @rdname odd
#' @export
odd.tidyabm_env <- function(.tidyabm, ...) {
  stopifnot(is_tidyabm_env(.tidyabm))

  tibble::tibble(`ODD category` = c('Purpose and patterns',
                                    'Entities, state variables, and scales',
                                    'Process overview and scheduling',
                                    'Design concepts',
                                    'Initialization',
                                    'Input data',
                                    'Submodels'),
                 `tidyABM information` = c('# todo',
                                           '# todo',
                                           '# todo',
                                           '# todo',
                                           '# todo',
                                           '# todo',
                                           '# todo'))
}

#' Check if a provided object is of type `tidyabm_env`
#'
#' @param x object to check
#'
#' @export
is_tidyabm_env <- function(x) {
  inherits(x, 'tidyabm_env')
}

#' Convert the list of agent `tidyabm_agent` objects to a [tibble]
#'
#' @description
#'   Returns a tibble with all agents in the same order as the agent objects
#'   were initially added. Columns represent a unique identifier named `.id`,
#'   all agent characteristics, and all agent variables. Not that, if not at
#'   least one tick has passed, agent variables are `NA`. Any characteristics
#'   that some agents have while others do not will be `NA` for those agents
#'   who do not share this particular characteristic. Typically,
#'   characteristics/variables added internally through a particular
#'   environment are prefixed with `.`.
#'
#' @param .tidyabm the `tidyabm_env` object holding some agents
#'
#' @return a [tibble]
#'
#' @examples
#' create_grid_environment(seed = 4583, size = 4) %>%
#'   add_agents(create_agent(), 2) %>%
#'   convert_agents_to_tibble()
#'
#' @export
convert_agents_to_tibble <- function(.tidyabm) {
  agents <- attr(.tidyabm, 'agents')
  stopifnot(is.list(agents))
  stopifnot(length(agents) > 0)

  out <- dplyr::bind_rows(agents) %>%
    dplyr::relocate(.id)

  # check for missing variables
  all_variables <- attr(.tidyabm, 'class_params')[['agent_variables']]
  missing_variables <- all_variables[!all_variables %in% colnames(out)]

  out %>%
    dplyr::bind_cols(stats::setNames(as.list(rep(NA,
                                                 length(missing_variables))),
                                     missing_variables)) %>%
    tibble::as_tibble() %>%
    return()
}


# External Utils ----

#' Get an environment's characteristic value
#'
#' @param me the `tidyabm_env` environment object
#' @param characteristic character string naming the characteristic
#'
#' @return the characteristic value (or NULL if not found)
#' @family utilities
#' @export
get_characteristic <- function(me, characteristic) {
  if (!is_tidyabm_env(me)) {
    return(NULL)
  }
  cc <- attr(me, 'characteristics')
  if (characteristic %in% names(cc)) {
    return(cc[[characteristic]])
  } else {
    return(NULL)
  }
}

#' Get an environment's variable value
#'
#' @param me the `tidyabm_env` environment object
#' @param variable character string naming the variable
#'
#' @return the variable value (or NULL if variable is not found)
#' @family utilities
#' @export
get_variable <- function(me, variable) {
  if (!is_tidyabm_env(me)) {
    return(NULL)
  }
  vcv <- attr(me, 'variables_current_values')
  if (variable %in% names(vcv)) {
    return(vcv[[variable]])
  } else {
    return(NULL)
  }
}

#' Get a random agent from all agents in the environment
#'
#' @param abm the whole environment model (`abm`)
#' @param me if provided as an agent then the returned (random) agent will not
#'   be this one (i.e., `me` is omitted)
#'
#' @return a `tidyabm_agent` object
#' @family utilities
#' @export
get_random_agent <- function(abm,
                             me = NULL) {
  if (!is_tidyabm_env(abm)) {
    return(NULL)
  }

  agent <- sample(attr(abm, 'agents'),
                  size = 1)[[1]]

  if (!is.null(me) & is_tidyabm_agent(me)) {
    while (agent$.id == me$.id) {
      agent <- sample(attr(abm, 'agents'),
                      size = 1)[[1]]
    }
  }

  return(agent)
}

#' Claim a simulation to end
#'
#' @param me unnecessary but provided here for ease of use
#' @param abm the whole environment model (`abm`)
#'
#' @return a `tidyabm_env` object
#' @family utilities
#' @export
stop_abm <- function(me, abm) {
  if (!is_tidyabm_env(abm)) {
    return(NULL)
  }

  abm %>%
    end() %>%
    return()
}

# Formatting ----

#' @export
tbl_format_footer.tidyabm_env <- function(x, ...) {
  default_footer <- NextMethod()

  .tidyabm <- x
  abm_type <- substring(class(x)[[1]], nchar('tidyabm_env_') + 1)
  rt <- attr(.tidyabm, 'runtime')
  cp <- attr(.tidyabm, 'class_params')
  abm_status <- ifelse(is.null(rt),
                       'not initiated',
                       ifelse(is_ended(.tidyabm),
                              paste0('ended after ',
                                     rt[['next_tick']] - 1,
                                     ' ticks'),
                              paste0('simulating (',
                                     rt[['next_tick']] - 1,
                                     ' ticks passed)')))
  return(c(default_footer,
           pillar::style_subtle(paste0('# ABM ', abm_type, ' environment')),
           pillar::style_subtle(paste0(
             '* ',
             ifelse('footer_details' %in% names(cp),
                    ifelse(cp[['footer_details']] == '', '',
                           paste0(cp[['footer_details']], ', ')), ''),
             length(attr(.tidyabm, 'agents')), ' agents'
           )),
           pillar::style_subtle(paste0('* ',
                                       length(attr(.tidyabm, 'characteristics')),
                                       ' environment characteristic(s), ')),
           pillar::style_subtle(paste0('* ',
                                       length(attr(.tidyabm, 'variables')),
                                       ' environment variable(s), ')),
           pillar::style_subtle(paste0('* ',
                                       length(attr(.tidyabm, 'rules')),
                                       ' environment rule(s), ')),
           pillar::style_subtle(paste0('* ', abm_status))))
}


# Internal functions ----

#' Output constructor for `tidyabm_env` objects
#'
#' @param particular_suffix string to append to main class `tidyabm_env_`
#' @rdname new_tidyabm
new_tidyabm_env <- function(data,
                            particular_suffix,
                            class_params = list()) {

  stopifnot(is.character(particular_suffix) & particular_suffix != '')

  class_params <- append(class_params,
                         list(agent_characteristics = c(),
                              agent_variables = c()))

  .tidyabm <- new_tidyabm(data,
                          class_suffix = 'env',
                          particular_sub_class = paste0('tidyabm_env_',
                                                        particular_suffix),
                          class_params = class_params)

  attr(.tidyabm, 'agents') <- list()
  attr(.tidyabm, 'variables_current_values') <- list()
  return(.tidyabm)
}

#' Check if a given environment is ready to [tick]/[iterate]
#'
#' @param .tidyabm a `tidyabm_env` object
#'
#' @return logical
is_tickable <- function(.tidyabm) {
  stopifnot(is_tidyabm_env(.tidyabm))
  return(all(!is.null(attr(.tidyabm, 'runtime')),
             !is_ended(.tidyabm)))
}

#' End the simulation
#'
#' @param .tidyabm a `tidyabm_env` object
#'
#' @return a `tidyabm_env` object
end <- function(.tidyabm) {
  stopifnot(is_tidyabm_env(.tidyabm))
  rt <- attr(.tidyabm, 'runtime')
  rt <- append(rt,
               list(ended = lubridate::now()))
  attr(.tidyabm, 'runtime') <- rt
  return(.tidyabm)
}

#' Check if a given environment has ended
#'
#' @param .tidyabm a `tidyabm_env` object
#'
#' @return logical
is_ended <- function(.tidyabm) {
  stopifnot(is_tidyabm_env(.tidyabm))
  return('ended' %in% names(attr(.tidyabm, 'runtime')))
}

#' @rdname update_values
update_values.tidyabm_env <- function(.tidyabm,
                                      values,
                                      is_characteristic = FALSE) {
  stopifnot(is_tidyabm_env(.tidyabm))
  stopifnot(is.list(values))

  if (is_characteristic) {
    return(.tidyabm)
  }

  current_values <- attr(.tidyabm, 'variables_current_values')
  current_names <- names(current_values)
  new_names <- names(values)

  current_values <- replace(current_values,
                            current_names %in% new_names,
                            values[new_names %in% current_names])
  new_values <- values[!new_names %in% current_names]

  attr(.tidyabm, 'variables_current_values') <- c(current_values,
                                                  new_values)

  return(.tidyabm)
}
