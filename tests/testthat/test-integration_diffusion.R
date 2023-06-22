context('Integration test: Diffusion')

test_that("convergence and all", {
  agent_a <- create_agent() %>%
    set_characteristic(informed = FALSE,
                       connected = FALSE) %>%
    add_rule('initial connect',
             connected == FALSE,
             .consequence = \(me, abm) {
               for (i in 1:10) {
                 random_agent <- get_random_agent(abm, me)
                 me <- network_connect(me, random_agent)
               }
               me %>%
                 set_characteristic(connected = TRUE,
                                    .overwrite = TRUE,
                                    .suppress_warnings = TRUE) %>%
                 return()
             }) %>%
    add_rule('share information',
             informed == TRUE,
             .consequence = \(me, abm) {
               me %>%
                 network_spread('informed',
                                TRUE,
                                overwrite = TRUE,
                                suppress_warnings = TRUE) %>%
                 return()
             })

  agent_b <- agent_a %>%
    set_characteristic(informed = TRUE,
                       .overwrite = TRUE,
                       .suppress_warnings = TRUE)

  e <- create_network_environment(seed = 5382,
                                  is_directed = TRUE) %>%
    add_agents(agent_a,
               n = 90) %>%
    add_agents(agent_b,
               n = 10) %>%
    add_variable(share_informed = \(me, abm) {
      abm %>%
        convert_agents_to_tibble() %>%
        dplyr::summarise(share_informed = sum(informed)/dplyr::n()) %>%
        dplyr::pull(share_informed) %>%
        return()
    }) %>%
    add_rule('stop when all are informed',
             share_informed == 1,
             .consequence = stop_abm) %>%
    init()

  e <- e %>%
    iterate(verbose = FALSE,
            max_iterations = 5)

  expect_true(is_tidyabm_env_network(e))
  expect_true(any(e$.finished_after_tick))
})
