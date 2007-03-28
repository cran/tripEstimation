\name{old.metropolis}
\alias{old.metropolis}
\alias{old.dist.gc}
\alias{old.find.init}
\alias{old.mkLookup}
\alias{mkElevationSeg}
\alias{mkNLPosterior}
\title{Older versions of solar location estimation}
\description{
  Some deprecated functions, originally used purely for light level estimation before
  the sampling algorithm was generalized for satellite models as well.
  
}
\usage{
mkElevationSeg(segments, day)

mkNLPosterior(segments, day, light)

old.dist.gc(x1, x2 = NULL)

old.find.init(mask, nseg, nlpost, pars = c("Lon", "Lat", "k"))

old.metropolis(nlpost, lookup, p0, cov0, start, end, iter = 1000, step = 100)

old.mkLookup(x, binArray = TRUE)
}

\arguments{
  %% mkElevationSeg
  \item{segments}{ ~~Describe \code{segments} here~~ }
  \item{day}{ ~~Describe \code{day} here~~ }
  
  %% mkNLPosterior

  \item{light}{vector of light data }
  
  %% mkElevationSeg
  
  %% old.dist.gc
  \item{x1}{matrix of track locations}
  \item{x2}{ matrix of track locations (optional second part) }
  
  %% old.find.init
  \item{mask}{ image object of masked areas }
    \item{nseg}{ number of (twilight) segments }
    \item{nlpost}{ negative log posterior function}
  \item{pars}{ names of parameters }
  
  %% old.metropolis
  
    \item{lookup}{ lookup function for masked areas }
    \item{p0}{ initial locations for sampler }
    \item{cov0}{ covariance matrix for sampler }
    \item{start}{ known start parameters}
    \item{end}{ known end parameters}
    \item{iter}{ number of iterations }
  \item{step}{ number of thinning iterations per \code{iter}}
  
  %% old.mkLookup
   \item{x}{ image-like object of matrix or array of binary masks }
  \item{binArray}{ logical: are the masks compressed into bits? }
}
\details{
  These functions are included for archival purposes and are not fully supported.

}
\value{
  %~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}

}
\references{  }
\author{Michael D. Sumner}

\seealso{   Please use the supported function \code{\link{metropolis}}, with the \code{\link{solar.model}} or
  \code{\link{satellite.model}}. }
\examples{

}
\keyword{ misc }
