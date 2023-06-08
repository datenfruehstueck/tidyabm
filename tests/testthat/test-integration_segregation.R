context('Integration test: Segregation')

test_that("Schelling's Segregation", {

  agent_a <- create_agent() %>%
    set_characteristic(color = 'red') %>%
    add_variable(similar = \(me, abm) {
      neighbors <- grid_get_neighbors(me, abm, which = 'o')
      return(sum(neighbors$color == me$color)/nrow(neighbors))
    }) %>%
    add_variable(unhappy = \(me, abm) {
      return(me$similar < .15)
    }) %>%
    add_rule('move',
             unhappy == TRUE,
             .consequence = \(me, abm) {
               spot <- grid_get_free_neighboring_spots(me, abm, which = 'o') %>%
                 dplyr::slice_sample(n = 1)
               grid_move(me, abm,
                         new_x = spot$.x,
                         new_y = spot$.y) %>%
                 return()
             })

  suppressWarnings({
    agent_b <- agent_a %>%
      set_characteristic(color = 'blue',
                         .overwrite = TRUE)
  })

  e <- create_grid_environment(seed = 5381,
                               size = 20) %>%
    add_agents(agent_a,
               n = 20*20* 0.4) %>%
    add_agents(agent_b,
               n = 20*20* 0.4) %>%
    add_variable(mean_similar = \(me, abm) {
      abm %>%
        convert_agents_to_tibble() %>%
        dplyr::summarise(M = mean(similar, na.rm = TRUE)) %>%
        dplyr::pull(M) %>%
        return()
    }) %>%
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

  #todo
  #e <- e %>%
  #  iterate()

  expect_true(is_tidyabm_env_grid(e))

})
