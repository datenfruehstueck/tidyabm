context('utils')

test_that('retain_new_data_in_prior_object', {
  a <- create_agent()
  expect_equal(dim(a),
               c(0, 0))

  a_new <- a %>%
    retain_new_data_in_prior_object(tibble::tibble(foo = 'bar'))
  expect_equal(dim(a_new),
               c(1, 1))
})

test_that('round_half_up', {
  tests <- seq(0.5, 6.5, 1)
  expect_equal(round(tests, digits = 0),
               c(0, 2, 2, 4, 4, 6, 6))
  expect_equal(round_half_up(tests),
               c(1, 2, 3, 4, 5, 6, 7))
})
