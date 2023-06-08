context('environments: grid')

test_that('class', {
  expect_error(create_grid_environment(seed = 967))
  expect_error(create_grid_environment(seed = 1368,
                                       x = 2))
  expect_error(create_grid_environment(seed = 15,
                                       y = 2))
  expect_error(create_grid_environment(seed = 135,
                                       size = 2,
                                       x = 2))
  expect_error(create_grid_environment(seed = 12,
                                       size = 2,
                                       x = 2,
                                       y = 2))
  expect_error(create_grid_environment(size = 2))
  expect_error(create_grid_environment(x = 2,
                                       y = 2))

  expect_equal(class(create_grid_environment(seed = 333,
                                             size = 2))[1],
               'tidyabm_env_grid')
  expect_true(is_tidyabm_env_grid(create_grid_environment(seed = 1385,
                                                          size = 2)))
  expect_false(is_tidyabm_env_grid(create_agent()))

  expect_type(attr(create_grid_environment(seed = 22,
                                           size = 2),
                   'agents'),
              'list')
  expect_type(attr(create_grid_environment(seed = 22,
                                           size = 2),
                   'variables_current_values'),
              'list')

  expect_equal(attr(create_grid_environment(seed = 22,
                                            size = 2),
                    'class_params')[['n_fields']],
               2*2)
  expect_equal(attr(create_grid_environment(seed = 22,
                                            x = 10,
                                            y = 3),
                    'class_params')[['n_fields']],
               10*3)
})

test_that('matrix operations', {
  # inspiration from https://stackoverflow.com/a/29105929
  n <- 4

  expect_equal(convert_xy_to_mpos(n, 2, 2), 6)
  expect_equal(convert_xy_to_mpos(n, 1, 4), 4)
  expect_equal(convert_xy_to_mpos(n, 3, 1), 9)
  expect_equal(convert_xy_to_mpos(n, 4, 4), 16)

  expect_equal(convert_mpos_to_xy(n, 6), c(2, 2))
  expect_equal(convert_mpos_to_xy(n, 4), c(1, 4))
  expect_equal(convert_mpos_to_xy(n, 9), c(3, 1))
  expect_equal(convert_mpos_to_xy(n, 16), c(4, 4))

  e <- create_grid_environment(7328, size = n) %>%
    add_agents(create_agent(), 5)
  m <- e %>%
    convert_agents_to_padded_matrix()

  expect_type(m, 'double')
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(n+2, n+2))
  expect_equal(m[,1], rep(-1, 6))
  expect_equal(m[,6], rep(-1, 6))
  expect_equal(m[1,], rep(-1, 6))
  expect_equal(m[6,], rep(-1, 6))

  neighbors <- e %>%
    matrix_get_neighbors(1, 2)

  expect_length(neighbors, 8)
  expect_equal(names(neighbors),
               c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'))
  expect_gte(neighbors[['N']], 0)
  expect_gte(neighbors[['NE']], 0)
  expect_gte(neighbors[['E']], 0)
  expect_gte(neighbors[['SE']], 0)
  expect_gte(neighbors[['S']], 0)
  expect_equal(neighbors[['SW']], -1)
  expect_equal(neighbors[['W']], -1)
  expect_equal(neighbors[['NW']], -1)
})

test_that('add_agents', {
  e <- create_grid_environment(seed = 7,
                               x = 3,
                               y = 4)
  a <- create_agent()

  expect_error(add_agents(e, a,
                          n = 3*4+1))

  expect_true(is_tidyabm_env_grid(add_agents(e, a,
                                             n = 3*4)))

  e1 <- e %>%
    add_agents(a, n = 3) %>%
    convert_agents_to_tibble()
  expect_equal(nrow(e1),
               nrow(dplyr::distinct(e1,
                                    .x, .y)))
  expect_equal(sum(e1$.x > 0),
               nrow(e1))
  expect_equal(sum(e1$.y > 0),
               nrow(e1))

  coordinate_counter <- c(0, 1)
  e2 <- e %>%
    add_agents(a, n = 3,
               initial_position = \(me, abm) {
                   coordinate_counter[1] <- coordinate_counter[1] + 1
                   if (coordinate_counter[1] > 3) {
                     coordinate_counter[1] <- 1
                     coordinate_counter[2] <- coordiante_counter[2] + 1
                   }
                   return(coordinate_counter)
                 }) %>%
    convert_agents_to_tibble()
  expect_equal(nrow(e2),
               nrow(dplyr::distinct(e1,
                                    .x, .y)))
  expect_equal(sum(e2$.x > 0),
               nrow(e2))
  expect_equal(sum(e2$.y > 0),
               nrow(e2))
})

test_that('utils: move', {
  e <- create_grid_environment(seed = 14,
                               size = 3) %>%
    add_agents(create_agent() %>%
                 add_rule('move',
                          .consequence = \(me, abm) grid_move(me,
                                                              abm,
                                                              2,
                                                              2)),
               n = 1,
               initial_position = \(me, abm) return(c(1, 1))) %>%
    init()
  expect_equal(convert_agents_to_tibble(e)$.x, c(1))
  expect_equal(convert_agents_to_tibble(e)$.y, c(1))

  suppressWarnings({
    e <- e %>%
      tick(verbose = FALSE)
  })
  expect_equal(convert_agents_to_tibble(e)$.x, c(2))
  expect_equal(convert_agents_to_tibble(e)$.y, c(2))
})

test_that('utils: grid_get_neighbors and grid_get_free_neighboring_spots', {
  e <- create_grid_environment(seed = 12,
                               size = 5) %>%
    add_agents(create_agent(),
               n = 1,
               initial_position = \(me, abm) return(c(3, 3))) %>%
    add_agents(create_agent(),
               n = 1,
               initial_position = \(me, abm) return(c(3, 4))) %>%
    add_agents(create_agent(),
               n = 1,
               initial_position = \(me, abm) return(c(2, 2)))

  agent <- attr(e, 'agents')[[1]]

  expect_equal(nrow(grid_get_neighbors(agent, e, which = 'o')),
               2)
  expect_equal(nrow(grid_get_neighbors(agent, e, which = '+')),
               1)
  expect_equal(nrow(grid_get_neighbors(agent, e, which = '-')),
               0)
  expect_equal(nrow(grid_get_neighbors(agent, e, which = '|')),
               1)

  expect_equal(nrow(grid_get_free_neighboring_spots(agent, e, which = 'o')),
               6)
  expect_equal(nrow(grid_get_free_neighboring_spots(agent, e, which = '+')),
               3)
  expect_equal(nrow(grid_get_free_neighboring_spots(agent, e, which = '-')),
               2)
  expect_equal(nrow(grid_get_free_neighboring_spots(agent, e, which = '|')),
               1)
})

test_that('visualize', {
  e <- create_grid_environment(seed = 12,
                               size = 5) %>%
    add_agents(create_agent() %>%
                 set_characteristic(party = 'a',
                                    voter = TRUE),
               n = 2) %>%
    add_agents(create_agent() %>%
                 set_characteristic(party = 'a',
                                    voter = FALSE),
               n = 1) %>%
    add_agents(create_agent() %>%
                 set_characteristic(party = 'b',
                                    voter = TRUE),
               n = 2) %>%
    add_agents(create_agent() %>%
                 set_characteristic(party = 'b',
                                    voter = FALSE),
               n = 3)

  expect_s3_class(visualize(e),
                  'gg')
  expect_s3_class(visualize(e, color = party),
                  'gg')
  expect_s3_class(visualize(e, shape = voter),
                  'gg')
  expect_s3_class(visualize(e, color = party, shape = voter),
                  'gg')
})
