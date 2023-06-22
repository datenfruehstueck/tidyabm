context('environments: network')

test_that('class', {
  expect_equal(class(create_network_environment(seed = 334))[1],
               'tidyabm_env_network')
  expect_true(is_tidyabm_env_network(create_network_environment(seed = 1386)))
  expect_true(
    is_tidyabm_env_network(create_network_environment(seed = 1386,
                                                      is_directed = TRUE))
  )
  expect_false(is_tidyabm_env_network(create_agent()))

  expect_type(attr(create_network_environment(seed = 23),
                   'agents'),
              'list')

  expect_true(is_directed(create_network_environment(seed = 2,
                                                     is_directed = TRUE)))
  expect_false(is_directed(create_network_environment(seed = 2,
                                                      is_directed = FALSE)))
})

test_that('add_agents', {
  e <- create_network_environment(seed = 8)
  a <- create_agent()

  expect_true(is_tidyabm_env_network(add_agents(e, a,
                                                n = 3)))

  e1 <- e %>%
    add_agents(a, n = 3) %>%
    convert_agents_to_tibble()

  expect_equal(nrow(e1),
               nrow(dplyr::distinct(e1,
                                    .id)))
  expect_type(e1$.outdegree,
              'list')
  expect_type(e1$.outdegree[[1]],
              'NULL')
  expect_length(e1$.outdegree[[1]],
                0)
})

test_that('utils: connect', {
  # one connection per agent, undirected
  a <- create_agent() %>%
    add_rule('initial connect',
             .consequence = \(me, abm) {
               me %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             })

  e <- create_network_environment(seed = 83135,
                                  is_directed = FALSE) %>%
    add_agents(a,
               n = 5) %>%
    init()

  expect_equal(convert_agents_to_tibble(e)$.indegree,
               list(NULL, NULL, NULL, NULL, NULL))
  expect_equal(convert_agents_to_tibble(e)$.outdegree,
               list(NULL, NULL, NULL, NULL, NULL))

  e <- e %>%
    tick(verbose = FALSE)

  expect_equal(convert_agents_to_tibble(e)$.indegree,
               list('A5', c('A1', 'A4'), NULL, c('A2', 'A3'), NULL))
  expect_equal(convert_agents_to_tibble(e)$.outdegree,
               list('A2', 'A4', 'A4', 'A2', 'A1'))


  # one connection per agent, directed
  e <- create_network_environment(seed = 83153,
                                  is_directed = TRUE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE)

  expect_equal(convert_agents_to_tibble(e)$.indegree,
               list(c('A4', 'A5'), NULL, NULL, c('A1', 'A2', 'A3'), NULL))
  expect_equal(convert_agents_to_tibble(e)$.outdegree,
               list('A4', 'A4', 'A4', 'A1', 'A1'))


  # two connections per agent, undirected
  a <- create_agent() %>%
    add_rule('initial connect',
             .consequence = \(me, abm) {
               me %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             })

  e <- create_network_environment(seed = 83136,
                                  is_directed = FALSE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE)

  expect_equal(convert_agents_to_tibble(e)$.indegree,
               list(NULL,
                    c('A1', 'A3', 'A4'),
                    c('A2', 'A4', 'A5'),
                    c('A1', 'A2'),
                    NULL))
  expect_equal(convert_agents_to_tibble(e)$.outdegree,
               list(c('A2', 'A4'),
                    c('A4', 'A3'),
                    c('A2'),
                    c('A3', 'A2'),
                    c('A3')))
})

test_that('utils: spread', {
  a <- create_agent() %>%
    set_characteristic(informed = FALSE) %>%
    set_characteristic(connected = FALSE) %>%
    add_rule('initial connect',
             !connected,
             .consequence = \(me, abm) {
               me %>%
                 set_characteristic(connected = TRUE,
                                    .overwrite = TRUE,
                                    .suppress_warnings = TRUE) %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             }) %>%
    add_rule('spread information',
             informed == TRUE,
             .consequence = \(me, abm) {
               me %>%
                 network_spread('informed',
                                TRUE,
                                overwrite = TRUE,
                                suppress_warnings = TRUE) %>%
                 return()
             })

  b <- a %>%
    set_characteristic(informed = TRUE,
                       .overwrite = TRUE,
                       .suppress_warnings = TRUE)


  e <- create_network_environment(seed = 835,
                                  is_directed = FALSE) %>%
    add_agents(a,
               n = 10) %>%
    add_agents(b,
               n = 2) %>%
    init() %>%
    tick(verbose = FALSE)

  # informed initially: A11 & A12
  # connected with them: A7 (A11) and A5 (both)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, T, F, T, F, F, F, T, T))

  # connected with A7: A9 (and A11)
  # connected with A5: A6 (and A11 and A12)
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, T, T, T, F, T, F, T, T))

  # connected with A9: A2, A10 (and A7)
  # connected with A6: A1 (and also A2 and A5)
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(T, T, F, F, T, T, T, F, T, T, T, T))

  # connected with A1: A4
  # connected with A2:
  # connected with A10: A3, A8 (and A9)
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(T, T, T, T, T, T, T, T, T, T, T, T))

  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(T, T, T, T, T, T, T, T, T, T, T, T))



  a <- create_agent() %>%
    set_characteristic(informed = FALSE) %>%
    set_characteristic(connected = FALSE) %>%
    add_rule('initial connect',
             !connected,
             .consequence = \(me, abm) {
               me %>%
                 set_characteristic(connected = TRUE,
                                    .overwrite = TRUE,
                                    .suppress_warnings = TRUE) %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             }) %>%
    add_rule('spread information',
             informed == TRUE,
             .consequence = \(me, abm) {
               me %>%
                 network_spread('informed',
                                TRUE,
                                spread_via_indegree_links = TRUE,
                                spread_via_outdegree_links = FALSE,
                                overwrite = TRUE,
                                suppress_warnings = TRUE) %>%
                 return()
             })

  b <- a %>%
    set_characteristic(informed = TRUE,
                       .overwrite = TRUE,
                       .suppress_warnings = TRUE)


  e <- create_network_environment(seed = 935,
                                  is_directed = TRUE) %>%
    add_agents(a,
               n = 10) %>%
    add_agents(b,
               n = 2) %>%
    init() %>%
    tick(verbose = FALSE)

  # informed initially: A11 & A12
  # connected with them: A7, A8
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, F, F, T, T, F, F, T, T))

  # connected with A7: A6
  # connected with A8: A4
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, T, F, T, T, T, F, F, T, T))

  # connected with A6: A9
  # connected with A4: A5
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, T, T, T, T, T, T, F, T, T))

  # connected with A5: A2
  # connected with A9: A1
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(T, T, F, T, T, T, T, T, T, F, T, T))

  # connected with A2: A10
  # connected with A1: A3
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(T, T, T, T, T, T, T, T, T, T, T, T))



  a <- create_agent() %>%
    set_characteristic(informed = FALSE) %>%
    set_characteristic(connected = FALSE) %>%
    add_rule('initial connect',
             !connected,
             .consequence = \(me, abm) {
               me %>%
                 set_characteristic(connected = TRUE,
                                    .overwrite = TRUE,
                                    .suppress_warnings = TRUE) %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             }) %>%
    add_rule('spread information',
             informed == TRUE,
             .consequence = \(me, abm) {
               me %>%
                 network_spread('informed',
                                TRUE,
                                spread_via_indegree_links = FALSE,
                                spread_via_outdegree_links = TRUE,
                                overwrite = TRUE,
                                suppress_warnings = TRUE) %>%
                 return()
             })

  b <- a %>%
    set_characteristic(informed = TRUE,
                       .overwrite = TRUE,
                       .suppress_warnings = TRUE)


  e <- create_network_environment(seed = 18035,
                                  is_directed = TRUE) %>%
    add_agents(a,
               n = 10) %>%
    add_agents(b,
               n = 2) %>%
    init() %>%
    tick(verbose = FALSE)

  # informed initially: A11 & A12
  # connected with them: A6, A7
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, F, T, T, F, F, F, T, T))

  # connected with A6: -
  # connected with A7: -
  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, F, T, T, F, F, F, T, T))

  e <- e %>%
    tick(verbose = FALSE)
  expect_equal(convert_agents_to_tibble(e)$informed,
               c(F, F, F, F, F, T, T, F, F, F, T, T))
})

test_that('utils: get_neighbors', {
  e <- create_network_environment(seed = 13836,
                                  is_directed = FALSE) %>%
    add_agents(create_agent() %>%
                 add_rule('connect',
                          .consequence = \(me, abm) {
                            for (i in 1:5) {
                              random_agent <- get_random_agent(abm, me)
                              me <- network_connect(me, random_agent)
                            }
                            return(me)
                          }),
               n = 10) %>%
    init() %>%
    tick(verbose = FALSE)

  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[1]], e)),
               3)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[2]], e)),
               3)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[3]], e)),
               3)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[4]], e)),
               4)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[5]], e)),
               3)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[6]], e)),
               5)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[7]], e)),
               4)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[8]], e)),
               5)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[9]], e)),
               4)
  expect_equal(nrow(network_get_neighbors(attr(e, 'agents')[[10]], e)),
               3)


})

test_that('tidygraph', {
  a <- create_agent() %>%
    add_rule('connect to a random other agent',
             .consequence = \(me, abm) {
               me %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             })


  # directed
  g <- create_network_environment(seed = 183468,
                                  is_directed = TRUE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE) %>%
    convert_network_to_tidygraph()

  expect_equal(class(g)[1],
               'tbl_graph')
  expect_true(tidygraph::is.tbl_graph(g))

  # valid (and necessary) test but printing a tbl_graph yields a deprecation error
  #expect_output(print(g),
  #              '.+A directed simple graph.+')

  # undirected
  g <- create_network_environment(seed = 186834,
                                  is_directed = FALSE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE) %>%
    convert_network_to_tidygraph()

  expect_equal(class(g)[1],
               'tbl_graph')
  expect_true(tidygraph::is.tbl_graph(g))

  # valid (and necessary) test but printing a tbl_graph yields a deprecation error
  #expect_output(print(g),
  #              '.+An undirected multigraph.+')
})

test_that('visualize', {
  a <- create_agent() %>%
    set_characteristic(team = 'one') %>%
    add_rule('initial connect',
             .consequence = \(me, abm) {
               me %>%
                 network_connect(get_random_agent(abm, me)) %>%
                 return()
             })

  e <- create_network_environment(seed = 3135,
                                  is_directed = FALSE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE)

  expect_s3_class(visualize(e),
                  'gg')


  # one connection per agent, directed
  e <- create_network_environment(seed = 83153,
                                  is_directed = TRUE) %>%
    add_agents(a,
               n = 5) %>%
    init() %>%
    tick(verbose = FALSE)

  expect_s3_class(visualize(e),
                  'gg')
})
