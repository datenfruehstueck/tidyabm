# Exported functions ----

#' Create a network environment
#'
#' @description
#'   Creates an environment without any space limitations. However, agents
#'   therein can connect with each other and form a network.
#'
#'   The seed is used to explicitly document random-number generation
#'   initiation in your script.
#'
#' @param seed a single number (integer) to initiate random-number generation
#' @param is_directed if true, connections/links in the network have a direction
#'   (i.e., they are arrows); default is FALSE
#'
#' @return `tidyabm` object
#'
#' @examples
#' create_network_environment(seed = 43)
#'
#' create_network_environment(seed = 124, is_directed = TRUE)
#'
#' @export
create_network_environment <- function(seed,
                                       is_directed = FALSE) {
  stopifnot(is.logical(is_directed))
  stopifnot(is.numeric(seed))
  set.seed(seed)

  tibble::tibble() %>%
    new_tidyabm_env(
      'network',
      class_params = list(seed = seed,
                          is_directed = is_directed,
                          footer_details = ifelse(is_directed,
                                                  'directed',
                                                  'undirected'))) %>%
    add_rule('.mirror_links',
             .consequence = rule_mirror_links) %>%
    add_rule('.spread',
             .consequence = rule_spread) %>%
    return()
}

#' @rdname add_agents
#' @export
add_agents.tidyabm_env_network <- function(.tidyabm,
                                           agent,
                                           n,
                                           ...) {
  .tidyabm <- NextMethod()
  n <- round_half_up(n)

  agents <- attr(.tidyabm, 'agents')
  n_agents <- length(agents)
  i <- n_agents - n + 1

  attr(.tidyabm, 'agents') <- purrr::map_if(
    agents,
    c(rep(FALSE, n_agents - n),
      rep(TRUE, n)),
    \(agent) {
      agent %>%
        set_characteristic(.indegree = list(NULL),
                           .outdegree = list(NULL),
                           .overwrite = TRUE)
    })

  return(.tidyabm)
}

#' @rdname visualize
#' @param network_ggraph_layout graphical layout to use; see package `ggraph`
#'   for details, fittingly/usually this is one of 'kk', 'stress', 'fr', 'lgl',
#'   or 'graphopt'
#'
#' @export
visualize.tidyabm_env_network <- function(.tidyabm,
                                          network_ggraph_layout = 'kk',
                                          ...) {
  stopifnot(is_tidyabm_env_network(.tidyabm))

  g <- .tidyabm %>%
    convert_network_to_tidygraph() %>%
    ggraph::ggraph(layout = network_ggraph_layout)

  if (is_directed(.tidyabm)) {
    g <- g +
      ggraph::geom_edge_link(show.legend = FALSE,
                             arrow = ggplot2::arrow(length = ggplot2::unit(3, 'mm')),
                             end_cap = ggraph::circle(3, 'mm'))
  } else {
    g <- g +
      ggraph::geom_edge_link(show.legend = FALSE)
  }
  g <- g +
    ggraph::geom_node_point(size = 9,
                            color = '#c2c2c2') +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name),
                           size = 3) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = 'bottom'
    )

  return(g)
}

#' Convert current environment to [tidygraph] object
#'
#' @param .tidyabm the current `tidyabm_env_network` object
#'
#' @return a `tbl_graph` object
#'
#' @examples
#' a <- create_agent() %>%
#'   add_rule('connect to a random other agent',
#'            .consequence = \(me, abm) {
#'              me %>%
#'                network_connect(get_random_agent(abm, me)) %>%
#'                return()
#'            })
#' create_network_environment(seed = 183468,
#'                            is_directed = TRUE) %>%
#'   add_agents(a,
#'              n = 5) %>%
#'   init() %>%
#'   tick() %>%
#'   convert_network_to_tidygraph()
#'
#' @export
convert_network_to_tidygraph <- function(.tidyabm) {
  stopifnot(is_tidyabm_env_network(.tidyabm))

  agents <- .tidyabm %>%
    convert_agents_to_tibble()

  nodes <- agents %>%
    dplyr::rename(name = .id) %>%
    dplyr::select(-dplyr::starts_with('.'))

  edges <- agents %>%
    tidyr::unnest(.indegree) %>%
    dplyr::select(from = .indegree,
                  to = .id) %>%
    dplyr::filter(!is.na(.data$from),
                  !is.na(.data$to))

  tidygraph::tbl_graph(nodes = nodes,
                       edges = edges,
                       directed = is_directed(.tidyabm)) %>%
    return()
}


# External Utils ----

#' Add a link between source and target agents
#'
#' @description
#'   If the network is directed, then the arrow points from source to target.
#'   If the network is undirected, the link does not contain any arrow.
#'
#' @param source_agent the agent from whom the link should start
#' @param target_agent the agent where the link should end
#'
#' @return a `tidyabm_agent` object
#' @family utilities
#' @export
network_connect <- function(source_agent,
                            target_agent) {
  if (!is_tidyabm_agent(source_agent) | !is_tidyabm_agent(target_agent)) {
    return(NULL)
  }
  new_outdegree <- c(source_agent$.outdegree,
                     target_agent$.id)
  new_outdegree <- new_outdegree[!sapply(new_outdegree, is.null)]
  new_outdegree <- unique(as.character(stats::na.omit(new_outdegree)))
  source_agent %>%
    set_characteristic(.outdegree = list(new_outdegree),
                       .overwrite = TRUE,
                       .suppress_warnings = TRUE) %>%
    return()
}

#' Spread a characteristic between source and its links
#'
#' @description
#'   Spreading means that a certain information travels from the `source_agent`
#'   to any other agents linked to this source. In an undirected environment,
#'   this means that all agents linked to `source_agent` will receive a
#'   characteristic with the given name (`characteristic_name`) and the given
#'   value (`characteristic_value`). In a directed environment, by default,
#'   only agents who point to the current `source_agent` will receive the
#'   characteristic; this behavior can be changed via the two arguments
#'   `spread_via_indegree_links` and `spread_via_outdegree_links`.
#'
#' @param source_agent the agent from whom the information should be spread
#' @param characteristic_name character string name of the characteristic to
#'   be set for the linked agent
#' @param characteristic_value value of the spread information (i.e., the
#'   characteristic to be set)
#' @param spread_via_indegree_links only applicable in directed graphs; if true
#'   (the default) then the information spreads via links where `source_agent`
#'   is the target of the link (i.e., to whom others points)
#' @param spread_via_outdegree_links only applicable in directed graphs; if true
#'   then the information spreads via links where `source_agent` is also the
#'   source of the link (i.e., the starting point of an arrow); default here is
#'   false
#' @param overwrite argument to be passed on to `set_characteristic` (default
#'   is true meaning that the target characteristic will be overwritten)
#' @param suppress_warnings argument to be passed on to `set_characteristic`
#'   (default is true meaning that any overwritten characteristic will be
#'   done so silently/without raising any warnings)
#'
#' @return a `tidyabm_agent` object
#' @family utilities
#' @export
network_spread <- function(source_agent,
                           characteristic_name,
                           characteristic_value,
                           spread_via_indegree_links = TRUE,
                           spread_via_outdegree_links = FALSE,
                           overwrite = TRUE,
                           suppress_warnings = TRUE) {

  if (!is_tidyabm_agent(source_agent)) {
    return(NULL)
  }

  cp <- attr(source_agent, 'class_params')
  if (! 'spreads' %in% names(cp)) {
    cp[['spreads']] <- list()
  }

  cp[['spreads']] <- append(cp[['spreads']],
                            list(list(name = characteristic_name,
                                      value = characteristic_value,
                                      via_in = spread_via_indegree_links,
                                      via_out = spread_via_outdegree_links,
                                      overwrite = overwrite,
                                      suppress = suppress_warnings)))

  attr(source_agent, 'class_params') <- cp
  return(source_agent)
}



# Internal functions ----

#' Check if a provided object is of type `tidyabm_env_network`
#'
#' @param x object to check
is_tidyabm_env_network <- function(x) {
  inherits(x, 'tidyabm_env_network')
}

#' Check if given `tidyabm_env_network` is directed
#'
#' @param x object to check
is_directed <- function(x) {
  cp <- attr(x, 'class_params')
  return(cp[['is_directed']])
}

#' Fill up .indegree reflected on those newly set .outdegree links
#'
#' @param me the `tidyabm_env_network` object pushed from the .mirror_links rule
#' @param abm unused
#'
#' @return the updated `tidyabm_env_network` object
rule_mirror_links <- function(me, abm) {
  if (!is_tidyabm_env_network(me)) {
    return(NULL)
  }

  agents <- me %>%
    convert_agents_to_tibble()

  agents_already_in <- agents %>%
    tidyr::unnest(.indegree) %>%
    dplyr::select(source = .indegree,
                  target = .id) %>%
    dplyr::filter(!is.na(.data$source),
                  !is.na(.data$target))

  agents_out <- agents %>%
    tidyr::unnest(.outdegree) %>%
    dplyr::select(source = .id,
                  target = .outdegree) %>%
    dplyr::filter(!is.na(.data$source),
                  !is.na(.data$target))

  agents_newly_in <- agents_out %>%
    dplyr::anti_join(agents_already_in,
                     by = c('source',
                            'target')) %>%
    dplyr::arrange(.data$target)

  attr(me, 'agents') <- purrr::map(attr(me, 'agents'), \(agent) {
    if (agent$.id %in% agents_newly_in$target) {
      new_ids <- agents_newly_in %>%
        dplyr::filter(.data$target == agent$.id) %>%
        dplyr::pull(.data$source)
      new_indegree <- c(agent$.indegree,
                        new_ids)
      new_indegree <- new_indegree[!sapply(new_indegree, is.null)]
      new_indegree <- unique(as.character(stats::na.omit(new_indegree)))
      agent <- agent %>%
        set_characteristic(.indegree = list(new_indegree),
                           .overwrite = TRUE,
                           .suppress_warnings = TRUE)
    }
    return(agent)
  })

  return(me)
}

#' Distribute information as requested by `network_spread`
#'
#' @param me the `tidyabm_env_network` object pushed from the .spread rule
#' @param abm unused
#'
#' @return the updated `tidyabm_env_network` object
rule_spread <- function(me, abm) {
  if (!is_tidyabm_env_network(me)) {
    return(NULL)
  }

  purrr::map(attr(me, 'agents'), \(agent) {
    cp <- attr(agent, 'class_params')
    if ('spreads' %in% names(cp)) {
      purrr::map(cp[['spreads']], \(spread) {
        recipients <- c()
        if (is_directed(me)) {
          if (spread[['via_in']]) {
            recipients <- c(recipients, unlist(agent$.indegree))
          }
          if (spread[['via_out']]) {
            recipients <- c(recipients, unlist(agent$.outdegree))
          }
        } else {
          recipients <- c(unlist(agent$.indegree),
                          unlist(agent$.outdegree))
        }
        if (length(recipients) > 0) {
          me <<- me %>% distribute_characteristic_across_agents(
            .name = spread[['name']],
            .value = spread[['value']],
            .data$.id %in% recipients,
            .overwrite = spread[['overwrite']],
            .suppress_warnings = spread[['suppress']]
          )
        }
      })
    }
  })

  attr(me, 'agents') <- purrr::map(attr(me, 'agents'), \(agent) {
    cp <- attr(agent, 'class_params')
    if ('spreads' %in% names(cp)) {
      cp[['spreads']] <- NULL
      attr(agent, 'class_params') <- cp
    }
    return(agent)
  })

  return(me)
}
