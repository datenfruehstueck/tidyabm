context('environment generic class')

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
                               size = 30) %>%
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
                               size = 40) %>%
    add_agents(create_agent() %>%
                 set_characteristic(age = 12,
                                    vehicle = 'bike',
                                    birthday_fever = 3),
               n = 100) %>%
    add_agents(create_agent() %>%
                 set_characteristic(age = 17,
                                    vehicle = 'scooter',
                                    speed = 25) %>%
                 add_variable(got_ticketed = FALSE) %>%
                 add_variable(ran = \(me, abm) runif(1)>.5 ),
               n = 25) %>%
    convert_agents_to_tibble()

  expect_true(tibble::is_tibble(t))
  expect_equal(nrow(t),
               100 + 25)
  expect_equal(colnames(t),
               c('.id',
                 'age',
                 'vehicle',
                 'birthday_fever',
                 '.x',
                 '.y',
                 'speed',
                 'got_ticketed',
                 'ran'))
  expect_equal(sum(is.na(t$speed)),
               100)
  expect_type(t$ran,
              'logical')
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
               c(160, 4))
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
               c(200, 5))
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
  a <- create_agent() %>%
    set_characteristic(bla = 2) %>%
    add_variable(age = \(me, abm) round(runif(1, 18, 40))) %>%
    add_rule('23-year-olds talk more',
             age == 23,
             .consequence = \(me, abm) {
               me %>%
                 set_characteristic(bla = 3,
                                    .overwrite = TRUE) %>%
                 return()
             })
  e <- create_grid_environment(seed = 90,
                               size = 4) %>%
    add_agents(a,
               n = 10) %>%
    set_characteristic(bla = 2*2) %>%
    add_variable(mean_age = \(me, abm) {
      agents <- convert_agents_to_tibble(me)
      mean(agents$age)
    }) %>%
    add_rule('stop after 10 ticks',
             .tick == 10,
             .consequence = stop_abm) %>%
    init()
  expect_true(is_tickable(e))

  expect_output(
    suppressWarnings({
      e <- e %>%
        tick()
    })
  )
  expect_equal(dim(e), c(1, 6))
  expect_equal(e$.tick, c(1))
  expect_equal(e$.n_agents_after_tick, c(10))
  expect_equal(e$bla, c(4))
  expect_type(e$mean_age, 'double')
  expect_true(all(e$mean_age > 0))
  talkatives <- e %>%
    convert_agents_to_tibble() %>%
    dplyr::filter(age == 23)
  expect_true(all(talkatives$bla == 3))

  expect_output(
    suppressWarnings({
      e <- e %>%
        tick(verbose = FALSE)
    }),
    regexp = NA
  )
  expect_equal(dim(e), c(2, 6))
  expect_equal(e$.tick, c(1, 2))
  expect_equal(e$.n_agents_after_tick, c(10, 10))
  expect_equal(e$bla, c(4, 4))
  expect_type(e$mean_age, 'double')
  expect_true(all(e$mean_age > 0))
  talkatives <- e %>%
    convert_agents_to_tibble() %>%
    dplyr::filter(age == 23)
  expect_true(all(talkatives$bla == 3))

  create_grid_environment(seed = 93,
                          size = 4) %>%
    add_agents(create_agent() %>%
                 add_rule('no return',
                          .consequence = \(me, abm) {}),
               n = 1) %>%
    init() %>%
    tick(verbose = FALSE) %>%
    expect_warning()

  suppressWarnings({
    e <- e %>%
      iterate(verbose = FALSE)
  })
  expect_equal(dim(e),
               c(10, 6))
  expect_equal(e$.finished_after_tick,
               c(rep(FALSE, 9),
                 TRUE))
  expect_equal(e$.tick, 1:10)

  a <- create_agent() %>%
    set_characteristic(bla = 2) %>%
    add_variable(bla_squared = \(me, abm) me$bla*me$bla,
                 bla_sqrt = \(me, abm) sqrt(me$bla_squared))
  e <- create_grid_environment(seed = 77,
                               size = 4) %>%
    add_agents(a,
               n = 2) %>%
    set_characteristic(blubb = 2) %>%
    add_variable(blubb_squared = \(me, abm) get_characteristic(me, 'blubb')^2,
                 blubb_sqrt = \(me, abm) sqrt(get_variable(me, 'blubb_squared'))) %>%
    init()

  e <- e %>%
    tick(verbose = FALSE) %>%
    tick(verbose = FALSE)

  expect_equal(dim(e),
               c(2, 7))
  expect_equal(e$.finished_after_tick,
               c(FALSE, FALSE))
  expect_equal(e$.tick, 1:2)

  ta <- convert_agents_to_tibble(e)
  expect_equal(dim(ta),
               c(2, 6))
  expect_equal(ta$bla^2,
               ta$bla_squared)
  expect_equal(ta$bla,
               ta$bla_sqrt)
})

test_that('odd', {
  e_odd <- create_grid_environment(seed = 1,
                                   size = 2) %>%
    odd()
  expect_equal(dim(e_odd),
               c(7, 2))
})

test_that('utils: get_random_agent', {
  e <- create_grid_environment(seed = 8,
                               size = 4) %>%
    add_agents(create_agent(),
               n = 5)
  me <- attr(e, 'agents')[[1]]

  expect_true(is_tidyabm_agent(get_random_agent(e)))
  expect_true(is_tidyabm_agent(get_random_agent(e, me)))
  expect_false(get_random_agent(e, me)$.id == me$.id)
})

test_that('utils: stop_abm', {
  e <- create_grid_environment(seed = 99,
                               size = 4) %>%
    add_agents(create_agent(),
               n = 4) %>%
    add_rule('stop after 5 ticks',
             .tick == 5,
             .consequence = stop_abm) %>%
    init() %>%
    iterate(verbose = FALSE)
  expect_equal(dim(e), c(5, 4))
  expect_equal(e$.tick, 1:5)
  expect_equal(e$.finished_after_tick, c(F, F, F, F, T))
})

test_that('utils: get_characteristic', {
  e <- create_grid_environment(seed = 999,
                               size = 4) %>%
    set_characteristic(foo = 'bar')
  expect_equal(get_characteristic(e, 'foo'),
               'bar')
})

test_that('utils: get_variable', {
  e <- create_grid_environment(seed = 999,
                               size = 4) %>%
    add_variable(foo = \(me, abm) 'bar') %>%
    init() %>%
    tick(verbose = FALSE)
  expect_equal(get_variable(e, 'foo'),
               'bar')
})
