context('tidyABM generic class')

test_that('class', {
  expect_equal(class(new_tidyabm(tibble::tibble()))[1],
               'tidyabm_')

  expect_equal(class(new_tidyabm(tibble::tibble()))[2],
               'tidyabm')

  expect_equal(class(new_tidyabm(tibble::tibble(),
                                 class_suffix = 'env'))[1],
               'tidyabm_env')

  expect_equal(class(new_tidyabm(tibble::tibble(),
                                 class_suffix = 'agent'))[1],
               'tidyabm_agent')

  expect_error(new_tidyabm(NULL))

  expect_error(new_tidyabm(tibble::tibble(),
                           class_suffix = 'foo'))

  expect_type(attr(new_tidyabm(tibble::tibble()), 'class_params'),
              'list')

  expect_type(attr(new_tidyabm(tibble::tibble()), 'characteristics'),
              'list')

  expect_type(attr(new_tidyabm(tibble::tibble()), 'variables'),
              'list')

  expect_type(attr(new_tidyabm(tibble::tibble()), 'rules'),
              'list')
})

test_that('characteristics', {
  t <- new_tidyabm(tibble::tibble()) %>%
    set_characteristic(a = 3,
                       b = 'abc',
                       c = c(T, F, T, T))

  expect_length(attr(t, 'characteristics'),
                3)
  expect_type(attr(t, 'characteristics')$a,
              'double')
  expect_type(attr(t, 'characteristics')$b,
              'character')
  expect_type(attr(t, 'characteristics')$c,
              'logical')
  expect_true(is.vector(attr(t, 'characteristics')$c))
  expect_length(attr(t, 'characteristics')$c,
                4)

  expect_warning(set_characteristic(t,
                                    b = 3))

  suppressWarnings({
    t <- t %>%
      set_characteristic(b = 3)
  })

  expect_length(attr(t, 'characteristics'),
                3)
  expect_type(attr(t, 'characteristics')$b,
              'character')

  suppressWarnings({
    t <- t %>%
      set_characteristic(b = 3,
                         d = -1.3,
                         .overwrite = TRUE)
  })

  expect_length(attr(t, 'characteristics'),
                4)
  expect_type(attr(t, 'characteristics')$b,
              'double')
  expect_type(attr(t, 'characteristics')$d,
              'double')
})

test_that('variables', {
  t <- new_tidyabm(tibble::tibble()) %>%
    add_variable(a = 3,
                 b = 'abc',
                 c = c(T, F, T, T))

  expect_length(attr(t, 'variables'),
                3)
  expect_type(attr(t, 'variables')$a,
              'double')
  expect_type(attr(t, 'variables')$b,
              'character')
  expect_type(attr(t, 'variables')$c,
              'logical')
  expect_true(is.vector(attr(t, 'variables')$c))
  expect_length(attr(t, 'variables')$c,
                4)

  expect_warning(add_variable(t,
                              b = 3))

  suppressWarnings({
    t <- t %>%
      add_variable(b = 3)
  })

  expect_length(attr(t, 'variables'),
                3)
  expect_type(attr(t, 'variables')$b,
              'character')

  suppressWarnings({
    t <- t %>%
      add_variable(b = 3,
                   d = \(x) mean(x),
                   e = function(x) mean(x),
                   f = mean,
                   .overwrite = TRUE)
  })

  expect_length(attr(t, 'variables'),
                6)
  expect_type(attr(t, 'variables')$b,
              'double')
  expect_type(attr(t, 'variables')$d,
              'closure')
  expect_type(attr(t, 'variables')$e,
              'closure')
  expect_type(attr(t, 'variables')$f,
              'closure')
})

test_that('rules', {
  t <- new_tidyabm(tibble::tibble()) %>%
    add_rule('check something',
             age < 18,
             .consequence = \(me) print('checked!'))

  expect_length(attr(t, 'rules'),
                1)
  expect_type(attr(t, 'rules')$`check something`,
              'list')
  expect_equal(names(attr(t, 'rules')$`check something`),
               c('if', 'then'))
  expect_type(attr(t, 'rules')$`check something`$`if`,
              'list')
  expect_type(attr(t, 'rules')$`check something`$`then`,
              'closure')
  expect_error(add_rule(t, 'check something',
                        .consequence = mean))

  t <- t %>%
    add_rule('check something else',
             age < 18,
             gender == 'm',
             .consequence = \(me) print('checked!'))

  expect_length(attr(t, 'rules'),
                2)
  expect_length(attr(t, 'rules')$`check something else`$`if`,
                2)

  expect_error(add_rule(t, 'foo',
                        gender = 'm',
                        .consequence = mean))
})

test_that('retaining new data in prior objects', {
  t1 <- new_tidyabm(tibble::tibble())
  t1 <- t1 %>%
    set_characteristic(a = 2)
  expect_equal(class(t1)[1],
               'tidyabm_')
  expect_length(attr(t1, 'characteristics'),
                1)

  t2 <- t1 %>%
    retain_new_data_in_prior_object(tibble::tibble(b = 4))

  expect_equal(class(t2)[1],
               'tidyabm_')
  expect_length(attr(t2, 'characteristics'),
                1)

})
