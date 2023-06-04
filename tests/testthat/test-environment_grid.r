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

test_that('add_agents', {
  e <- create_grid_environment(seed = 7,
                               x = 3,
                               y = 4)
  a <- create_agent()

  expect_error(add_agents(e, a,
                          n = 3*4+1))

  expect_true(is_tidyabm_env_grid(add_agents(e, a,
                                             n = 3*4)))
})
