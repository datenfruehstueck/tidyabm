% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{remove_agent}
\alias{remove_agent}
\title{Remove an agent from an environment}
\usage{
remove_agent(abm, agent)
}
\arguments{
\item{abm}{a \code{tidyabm_env} object}

\item{agent}{either a \code{tidyabm_agent} object or a character string that
specifies the agent ID (e.g., 'A2')}
}
\value{
a \code{tidyabm_env} object
}
\description{
The agent can either be specified as a \code{tidyabm_agent} object or as an
agent ID (i.e., a character string).
}
\seealso{
Other utilities: 
\code{\link{get_characteristic}()},
\code{\link{get_random_agent}()},
\code{\link{get_variable}()},
\code{\link{grid_get_free_neighboring_spots}()},
\code{\link{grid_get_free_spots}()},
\code{\link{grid_get_neighbors}()},
\code{\link{grid_move}()},
\code{\link{network_connect}()},
\code{\link{network_get_neighbors}()},
\code{\link{network_spread}()},
\code{\link{stop_abm}()}
}
\concept{utilities}
