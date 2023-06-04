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
