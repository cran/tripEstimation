\name{satellite.model}
\alias{satellite.model}
\title{ Function to create a satellite model object for metropolis location sampler }
\description{
 A model to manage likelihood functions, environmental masks and behavioural 
likelihood functions for pre-derived satellite locations. There are some options for configuration,
but this
may be considered a template for any given model. The model \emph{function} exists
simply to make the object construction simple. 
}
\arguments{
  \item{day}{ vector of date-times for each light level }
  \item{X}{ matrix of pre-derived satellite locations }
  \item{proposal.x}{ function from object managing X proposals }
    \item{proposal.z}{ function from object managing Z proposals }
    \item{mask.x}{ lookup function for X's against masks }
    \item{mask.z}{ lookup function for Z's against masks }
    \item{fix.release}{ logical - is the release point known? }
  \item{fix.recapture}{ logical - is the recapture point known? }
  \item{start.x}{ ~~Describe \code{start.x} here~~ }
  \item{start.z}{ ~~Describe \code{start.z} here~~ }
  \item{posn.sigma}{ variance for locations }
  \item{speed.dist}{ distribution to use for behavioural constraint }
  \item{speed.mean}{ mean to use for behavioural distribution }
  \item{speed.sd}{ variance for behavioural distribution}
}
\details{
\code{posn.sigma} may be a single value for all estimates, or a vector of values for each position
estimate. 
}
\value{
}
\references{[Argos manual]}
\author{ Michael D. Sumner }
\note{

This function is not completely consistent with its solar counterpart \code{solar.model}. 
These are working functions that are in constant development, but they are structurally 
simple and may be easily edited as required. 
}
\seealso{ See also \code{\link{solar.model}} for the counterpart model for solar problems}
\examples{
}
\keyword{ manip }