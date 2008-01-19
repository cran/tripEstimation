\name{metropolis}
\alias{metropolis}
\alias{metropolis0}
\title{ Metropolis-Hastings sampler for location estimation for archival and satellite tag }
\description{
  These functions provide a direct implementation of the Metropolis-Hastings algorithm, for
  calculating marginal posterior (locations and full-track estimates) 
  properties using Markov Chain Monte Carlo. The
  sampler is written completely in \R, vectorized to be as fast as possible. The sampler can
  include likelihood functions for large data records (including light and water temperature), as
  well as \emph{mask} functions for simpler rejection sources. Behavioural constraints are
  implemented using a red/black update, so that location estimates \emph{X} and \emph{Z} may
  be estimated in an efficient manner. The parameter estimates may be cached and later
  queried arbitrarily. 
}
\usage{
metropolis(model, iters = 1000, thin = 10, start.x = NULL, start.z = NULL)

metropolis0(model, iters = 1000, thin = 10, start.x = NULL, start.z = NULL)
}

\arguments{
%% metropolis
  \item{model}{ ~~Describe \code{model} here~~ }
  \item{iters}{ ~~Describe \code{iters} here~~ }
  \item{thin}{ ~~Describe \code{thin} here~~ }
  \item{start.x}{ ~~Describe \code{start.x} here~~ }
  \item{start.z}{ ~~Describe \code{start.z} here~~ }


}
\details{
  \code{\link{metropolis0}} is a slightly different version of \code{\link{metropolis}}
  that enables an initialization step, required to find parameter estimates that are
  consistent with any masks used. It is difficult to make this step more elegant, and so
  we live with the two versions. 
  
  In terms of the estimates, X's have \code{m} records with \code{n} parameters, where \code{m}
  is the number of data records in time (twilights for archival tags, Argos estimates for satellite 
  tags) and \code{n} is at least x-coordinate, y-coordinate and maybe k-attenuation for light. 
  Z's have \code{m-1} records with 2 parameters for `x` and `y` (which are usually Longitude and
  Latitude). These parameters may be increased or changed, they are tied only to the likelihood 
  functions used, not the sampler itself. Also, coordinate transformations may be used inside the model
  and likelihood functions, in order to use an appropriate map projection. Solar calculations rely on
  lon/lat and so this step does slow down light level geo-location. 
}
\value{
A MCM \emph{Chain} stored as a list containing 
  \item{model }{The model object used by the sampler}
  \item{x }{The last \code{iters} X-samples accepted, stored as an \code{c(m, n, iters)} array }
  \item{z }{The last \code{iters} Z-samples accepted, stored as an \code{c(m - 1, 2, iters)}}
  \item{last.x }{The last accepted X-sample, stored as a \code{c(m, n)} matrix}
  \item{last.z }{The last accepted Z-sample, stored as a \code{c(m, 2)} matrix}
}
\references{[Ekstrom, Hill, Gilks, etc.]}
\author{ Michael D. Sumner }
\note{ 
}

\seealso{\code{\link{solar.model}}, \code{\link{satellite.model}} }
\examples{

}
\keyword{manip }
