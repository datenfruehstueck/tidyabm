context('agent')

test_that('class', {
  expect_equal(class(create_agent())[1],
               'tidyabm_agent')
  expect_true(is_tidyabm_agent(create_agent()))
  expect_false(is_tidyabm_agent(create_grid_environment(seed = 153,
                                                        size = 2)))
})

test_that('values', {
  a <- create_agent()
  expect_equal(dim(a),
               c(0, 0))

  a <- a %>%
    set_characteristic(foo = 'bar')
  expect_equal(dim(a),
               c(1, 1))

  a <- a %>%
    set_characteristic(a = 2,
                       b = 14)
  expect_equal(dim(a),
               c(1, 3))

  a <- a %>%
    add_variable(c = TRUE)
  expect_equal(dim(a),
               c(1, 3))
})
