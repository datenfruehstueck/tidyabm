% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment_network.R
\name{network_spread}
\alias{network_spread}
\title{Spread a characteristic between source and its links}
\usage{
network_spread(
  source_agent,
  characteristic_name,
  characteristic_value,
  spread_via_indegree_links = TRUE,
  spread_via_outdegree_links = FALSE,
  overwrite = TRUE,
  suppress_warnings = TRUE
)
}
\arguments{
\item{source_agent}{the agent from whom the information should be spread}

\item{characteristic_name}{character string name of the characteristic to
be set for the linked agent}

\item{characteristic_value}{value of the spread information (i.e., the
characteristic to be set)}

\item{spread_via_indegree_links}{only applicable in directed graphs; if true
(the default) then the information spreads via links where \code{source_agent}
is the target of the link (i.e., to whom others points)}

\item{spread_via_outdegree_links}{only applicable in directed graphs; if true
then the information spreads via links where \code{source_agent} is also the
source of the link (i.e., the starting point of an arrow); default here is
false}

\item{overwrite}{argument to be passed on to \code{set_characteristic} (default
is true meaning that the target characteristic will be overwritten)}

\item{suppress_warnings}{argument to be passed on to \code{set_characteristic}
(default is true meaning that any overwritten characteristic will be
done so silently/without raising any warnings)}
}
\value{
a \code{tidyabm_agent} object
}
\description{
Spreading means that a certain information travels from the \code{source_agent}
to any other agents linked to this source. In an undirected environment,
this means that all agents linked to \code{source_agent} will receive a
characteristic with the given name (\code{characteristic_name}) and the given
value (\code{characteristic_value}). In a directed environment, by default,
only agents who point to the current \code{source_agent} will receive the
characteristic; this behavior can be changed via the two arguments
\code{spread_via_indegree_links} and \code{spread_via_outdegree_links}.
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
\code{\link{remove_agent}()},
\code{\link{stop_abm}()}
}
\concept{utilities}
