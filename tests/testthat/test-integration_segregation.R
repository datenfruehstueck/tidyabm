context('Integration test: Segregation')

test_that("convergence and all", {
  agent_a <- create_agent() %>%
    set_characteristic(color = 'red') %>%
    add_variable(unhappy = function(me, abm) {
      neighbors <- grid_get_neighbors(me, abm, which = '+')
      if (nrow(neighbors) == 0) {
        return(FALSE)
      } else {
        return(sum(neighbors$color == me$color) == 0)
      }
    }) %>%
    add_rule('move',
             unhappy == TRUE,
             .consequence = \(me, abm) {
               spot <- grid_get_free_spots(abm) %>%
                 dplyr::slice_sample(n = 1)
               grid_move(me, abm,
                         new_x = spot$x,
                         new_y = spot$y) %>%
                 return()
             })

  suppressWarnings({
    agent_b <- agent_a %>%
      set_characteristic(color = 'blue',
                         .overwrite = TRUE)
  })

  e <- create_grid_environment(seed = 5381,
                               size = 10) %>%
    add_agents(agent_a,
               n = 10) %>%
    add_agents(agent_b,
               n = 10) %>%
    add_variable(share_unhappy = \(me, abm) {
      abm %>%
        convert_agents_to_tibble() %>%
        dplyr::summarise(share_unhappy = sum(unhappy)/dplyr::n()) %>%
        dplyr::pull(share_unhappy) %>%
        return()
    }) %>%
    add_rule('stop when all are happy',
             share_unhappy <= 0,
             .consequence = stop_abm) %>%
    init()

  e <- e %>%
    iterate(verbose = FALSE,
            max_iterations = 15)

  expect_true(is_tidyabm_env_grid(e))
  expect_true(any(e$.finished_after_tick))
  expect_lte(length(e$.tick), 15)
})
