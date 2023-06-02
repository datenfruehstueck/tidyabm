context('generic environment')

test_that('class', {
  expect_error(new_tidyabm_env(tibble::tibble()))
  expect_equal(class(new_tidyabm_env(tibble::tibble(),
                                     'grid'))[1],
               'tidyabm_env_grid')
  expect_true(is_tidyabm_env(create_grid_environment(seed = 386,
                                                     size = 2)))
  expect_false(is_tidyabm_env(create_agent()))
})

test_that('add_agents', {
  a <- create_agent()
  e <- create_grid_environment(seed = 5523,
                               size = 10) %>%
    add_agents(a,
               n = 100)

  expect_type(attr(e, 'agents'),
              'list')
  expect_length(attr(e, 'agents'),
                100)

  e <- e %>%
    add_agents(a,
               n = 50)

  expect_type(attr(e, 'agents'),
              'list')
  expect_length(attr(e, 'agents'),
                100 + 50)
})

test_that('convert_agents_to_tibble', {
  t <- create_grid_environment(seed = 1658,
                               size = 10) %>%
    add_agents(create_agent() %>%
                 set_characteristic(age = 12,
                                    vehicle = 'bike',
                                    birthday_fever = 3),
               n = 100) %>%
    add_agents(create_agent() %>%
                 set_characteristic(age = 17,
                                    vehicle = 'scooter',
                                    speed = 25) %>%
                 add_variable(got_ticketed = FALSE),
               n = 25) %>%
    convert_agents_to_tibble()

  expect_true(tibble::is_tibble(t))
  expect_equal(nrow(t),
               100 + 25)
  expect_equal(colnames(t),
               c('i',
                 'age',
                 'vehicle',
                 'birthday_fever',
                 'speed',
                 'got_ticketed'))
  expect_equal(sum(is.na(t$speed)),
               100)
})

test_that('distribute_characteristic_across_agents', {
  t1 <- create_grid_environment(seed = 5,
                                    size = 20) %>%
    add_agents(create_agent(),
               n = 160) %>%
    distribute_characteristic_across_agents(
      'friend',
      \(i, agent, agents) ifelse(stats::runif(1) > 0.5, i-1, 0)
    ) %>%
    convert_agents_to_tibble()
  expect_equal(dim(t1),
               c(160, 2))
  expect_true(all(t1$friend >= 0))

  a1 <- create_agent() %>%
    set_characteristic(age = 'young')
  a2 <- create_agent() %>%
    set_characteristic(age = 'old')
  t2 <- create_grid_environment(seed = 486, size = 50) %>%
    add_agents(a1, 100) %>%
    add_agents(a2, 100) %>%
    distribute_characteristic_across_agents(
      'opinion',
      stats::rnorm(100),
      age == 'old'
    ) %>%
    convert_agents_to_tibble()
  expect_equal(dim(t2),
               c(200, 3))
  expect_equal(sum(is.na(t2$opinion)),
               100)
})

test_that('init and reset', {
  expect_error(init(create_agent()))

  e <- create_grid_environment(seed = 635,
                               size = 2) %>%
    init()
  expect_true(is_tidyabm_env(e))
  expect_false(is.null(attr(e, 'runtime')))
  expect_true(is_tickable(e))
  expect_false(is_ended(e))

  e <- e %>%
    reset()
  expect_true(is_tidyabm_env(e))
  expect_true(is.null(attr(e, 'runtime')))
  expect_false(is_tickable(e))

  expect_error(reset(e))

  e <- e %>%
    init()
  expect_true(is_tidyabm_env(e))
  expect_false(is.null(attr(e, 'runtime')))
  expect_true(lubridate::is.POSIXct(attr(e, 'runtime')[['initiated']]))
  expect_equal(attr(e, 'runtime')[['next_tick']], 1)
  expect_true(is_tickable(e))

  expect_error(init(e))
})

test_that('tick and iterate', {
  # todo
})
