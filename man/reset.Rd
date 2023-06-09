% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{reset}
\alias{reset}
\alias{reset.tidyabm_env}
\title{Reset an already initiated environment}
\usage{
reset(.tidyabm)

\method{reset}{tidyabm_env}(.tidyabm)
}
\arguments{
\item{.tidyabm}{the \code{tidyabm_env} object}
}
\value{
a \code{tidyabm_env} object
}
\description{
This function erases any model runs, runtime information, and any results.
It works a bit like \link{init} but for already started/run/initiated
environments. After a reset, an additional \link{init} is necessary.
}
\examples{
create_grid_environment(seed = 46444, size = 4) \%>\%
  add_agents(create_agent(), 2) \%>\%
  init() \%>\%
  tick() \%>\%
  reset() \%>\%
  init() \%>\%
  tick()

}
