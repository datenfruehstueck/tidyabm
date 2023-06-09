% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment_grid.r
\name{grid_get_free_spots}
\alias{grid_get_free_spots}
\title{Get a \link{tibble} with two columns (.x and .y) of all free spots}
\usage{
grid_get_free_spots(abm)
}
\arguments{
\item{abm}{the whole environment model}
}
\value{
a \link{tibble} of spots (with a \code{.x} and a \code{.y} column) in no particular
order that are free
}
\description{
Get a \link{tibble} with two columns (.x and .y) of all free spots
}
\seealso{
Other utilities: 
\code{\link{get_characteristic}()},
\code{\link{get_random_agent}()},
\code{\link{get_variable}()},
\code{\link{grid_get_free_neighboring_spots}()},
\code{\link{grid_get_neighbors}()},
\code{\link{grid_move}()},
\code{\link{network_connect}()},
\code{\link{network_get_neighbors}()},
\code{\link{network_spread}()},
\code{\link{remove_agent}()},
\code{\link{stop_abm}()}
}
\concept{utilities}
