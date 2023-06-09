% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{round_half_up}
\alias{round_half_up}
\title{Round to integer, but round .5 up}
\usage{
round_half_up(n)
}
\arguments{
\item{n}{the number to round}
}
\value{
an integer
}
\description{
R's \link{round} function rounds .5 to the next even digit as specified in
\verb{IEEE 754}. However, this is not expected here but instead .5 should always
be rounded up. See also http://andrewlandgraf.com/2012/06/15/rounding-in-r/
}
\keyword{internal}
