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
  expect_output(print(g),
                '.+A directed simple graph.+')

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
  expect_output(print(g),
                '.+An undirected multigraph.+')
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
  expect_s3_class(visualize(e, color = 'team'),
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
  expect_s3_class(visualize(e, color = 'team'),
                  'gg')
})
