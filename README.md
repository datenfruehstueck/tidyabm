
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyABM

<!-- badges: start -->

[![R-CMD-check](https://github.com/datenfruehstueck/tidyabm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/datenfruehstueck/tidyabm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*tidyABM* is designed to streamline the process of setting up, running,
and documenting Agent-Based Models (ABM) using the principles of [tidy
data](https://r4ds.had.co.nz/tidy-data.html). It thus allows researchers
to easily organize, manipulate, and visualize ABMs in a structured,
coherent, and intuitive manner, built on
[tidyverse](https://www.tidyverse.org/) conventions to efficiently build
and explore ABMs while ensuring readability, reproducibility, and
transparency.

## Installation

Currently, you can only install the development version of *tidyABM*
like so:

``` r
remotes::install_github('datenfruehstueck/tidyabm')
```

## Usage

At its core, *tidyABM* is organized into environments and agents. Both
can have characteristics (specified constants), variables (flexible
attributions), and rules (planned actions). Once set up, many agents can
be added to an environment which can then be simulated. Simulations work
over time where, for every tick (i.e., one point in time), variables and
rules for agents and the overall environment are being evaluated.

To get started, we set up a grid environment (i.e., a 2D rectangle). In
that, we also have to set/document the
[seed](https://en.wikipedia.org/wiki/Random_seed) as random-number
generation is quite a common thing to do with ABMs.

``` r
library(tidyabm)

e <- create_grid_environment(seed = 826347,
                             x = 10,
                             y = 10)
```

Next, we create two kinds of agents, the orange ones and the violet
ones. We (or, more specifically: they) enjoy neighborhoods where there
are similar agents. Specifically, they get unhappy if not one of their
four direct neighbors is similar to themselves. As such, they tend to
check their hood and, if there’s not enough similarity, move to other
places.

``` r
agent_o <- create_agent() %>% 
  set_characteristic(agent_group = 'orange') %>% 
  add_variable(is_unhappy = function(me, abm) {
    neighbors <- grid_get_neighbors(me, abm, which = '+')
    if (nrow(neighbors) == 0) {
      return(FALSE)
    } else {
      return(sum(neighbors$agent_group == me$agent_group) == 0)
    }
  }) %>%
  add_rule('move',
           is_unhappy == TRUE,
           .consequence = function(me, abm) {
               spot <- grid_get_free_neighboring_spots(me, abm) %>%
                 dplyr::slice_sample(n = 1)
               grid_move(me, abm,
                         new_x = spot$.x,
                         new_y = spot$.y) %>%
                 return()
           })

agent_v <- agent_o %>% 
  set_characteristic(agent_group = 'violet',
                     .overwrite = TRUE)
#> Warning: The following characteristics already existed. They were overwritten:
#> agent_group
```

Now let’s add those agents to our environment. We want several orange
and several violet ones. In fact, we’d like ten each. By default, they
are thereby randomly distributed across our grid environment.

``` r
e <- e %>% 
  add_agents(agent_o,
             n = 10) %>% 
  add_agents(agent_v,
             n = 10)
```

Finally, we need to tell the environment when to stop. Ideally, this
would be the case when every agent is happy. If that’s not really
possible, we stop the simulation after 50 iterations (see below) at the
latest and see where it took us.

``` r
e <- e %>% 
  add_variable(share_unhappy = function(me, abm) {
    abm %>%
      convert_agents_to_tibble() %>%
      dplyr::summarise(share_unhappy = sum(is_unhappy)/dplyr::n()) %>%
      dplyr::pull(share_unhappy) %>%
      return()
    }) %>%
    add_rule('stop when all are happy',
             share_unhappy <= 0,
             .consequence = stop_abm)
```

Now we can initialize the environment and look at the seminal
distribution of agents.

``` r
e <- e %>% 
  init()

e %>% 
  visualize(color = agent_group)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Next, let’s run the simulation. We can simply run one iteration (i.e.,
one tick).

``` r
e <- e %>% 
  tick()
#> [1] "Tick 1 finished in 0.413 secs:"
#> [1] "  share_unhappy: 0.3"
```

Or, we can iterate the whole thing. In this case, iteration starts at
the second tick as we have already manually just done the first one. The
iteration step takes a while. We get some status messages in between. If
we also want to take a look at the visual development, we can tell
*tidyABM* to also visualize each iteration.

``` r
e <- e %>% 
  iterate(max_iterations = 30,
          visualize = TRUE,
          color = agent_group,
          shape = is_unhappy)
#> [1] "Tick 2 finished in 0.337 secs:"
#> [1] "  share_unhappy: 0.25"
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

    #> [1] "Tick 3 finished in 0.267 secs:"
    #> [1] "  share_unhappy: 0.05"

<img src="man/figures/README-unnamed-chunk-9-2.png" width="100%" />

    #> [1] "Tick 4 finished in 0.268 secs:"
    #> [1] "  share_unhappy: 0.05"

<img src="man/figures/README-unnamed-chunk-9-3.png" width="100%" />

    #> [1] "Tick 5 finished in 0.249 secs:"
    #> [1] "  share_unhappy: 0"

<img src="man/figures/README-unnamed-chunk-9-4.png" width="100%" />

When it’s done, *tidyABM* provides us with statistics about each
iteration. Oh, and since it’s a tidy package, of course these statistics
come in the form of [tibbles](https://tibble.tidyverse.org/).

``` r
e
#> # A tibble: 5 × 5
#>   .tick .runtime       .n_agents_after_tick .finished_after_tick share_unhappy
#> * <dbl> <drtn>                        <int> <lgl>                        <dbl>
#> 1     1 0.4128311 secs                   20 FALSE                         0.3 
#> 2     2 0.3370681 secs                   20 FALSE                         0.25
#> 3     3 0.2674410 secs                   20 FALSE                         0.05
#> 4     4 0.2684209 secs                   20 FALSE                         0.05
#> 5     5 0.2488461 secs                   20 TRUE                          0
```

We can also take a look at all the agents (not only the two blueprints
from the beginning but all of the added ones), also in a tidy format.

``` r
e %>% 
  convert_agents_to_tibble()
#> # A tibble: 20 × 5
#>    .id   agent_group    .x    .y is_unhappy
#>    <chr> <chr>       <dbl> <dbl> <lgl>     
#>  1 A1    orange         10     9 FALSE     
#>  2 A2    orange          8     1 FALSE     
#>  3 A3    orange          4     4 FALSE     
#>  4 A4    orange          5     4 FALSE     
#>  5 A5    orange          2    10 FALSE     
#>  6 A6    orange         10    10 FALSE     
#>  7 A7    orange         10     1 FALSE     
#>  8 A8    orange          9     8 FALSE     
#>  9 A9    orange          4     3 FALSE     
#> 10 A10   orange          7     1 FALSE     
#> 11 A11   violet          9     2 FALSE     
#> 12 A12   violet          8     9 FALSE     
#> 13 A13   violet          3     5 FALSE     
#> 14 A14   violet         10     7 FALSE     
#> 15 A15   violet          6     1 FALSE     
#> 16 A16   violet          5     1 FALSE     
#> 17 A17   violet          9     4 FALSE     
#> 18 A18   violet          1     8 FALSE     
#> 19 A19   violet          6     2 FALSE     
#> 20 A20   violet          1     9 FALSE
```

There’s of course more to explore and the possibilities are huge,
particularly with the flexible triangle of characteristics, variables,
and rules. For example, *tidyABM* is not only capable of handling grid
environments but also others and each environment comes with their own
helping utilities to inform variables and rules. Also, *tidyABM* can
help write-up your [ODD protocols](https://doi.org/10.18564/jasss.4259).
And it comes with quite a few vignettes for common ABM introductory
simulations such as Schelling’s seggregation we kind-of looked into
here.

``` r
browseVignettes('tidyabm')
```

## Issues and Citation

Feel free to contribute to this package, either by providing feedback
(via the [issues](https://github.com/datenfruehstueck/tidyabm/issues))
or by providing code (via the [pull
requests](https://github.com/datenfruehstueck/tidyabm/pulls)).

If you use this package I would appreciate you citing it, for example
like so:

[Haim, Mario](https://haim.it/) (2023). tidyABM: Set up, run, and
document agent-based models (ABM) the tidy way. Source code and releases
available at <https://github.com/datenfruehstueck/tidyABM/>.
