\name{initialize.x}
\alias{initialize.x}
\alias{position.logp}
\alias{show.segment}
\alias{light.quantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Diagnose and initialize light level estimation. }
\description{
  Primarily for the purposes of initializing the estimation, these functions
  can also be used for diagnostic purposes.   \code{position.logp} both produces
  diagnostic images for each twilight and uses those to initialize position. 
}
\usage{
position.logp(model, x1, x2, xrest = NULL, subset = 1:model$n, initialize.x = TRUE, start = NULL, end = NULL)

initialize.x(model, x1, x2, xrest = NULL)

light.quantile(model, chain, day, seg, prob = c(0.025, 0.5, 0.975))

show.segment(model, chain, segment, day, light, k, n = 50, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  %% position.logp
  \item{model}{ estimation model object }
  \item{x1}{ vector of x-coordinates defining the prior grid }
  \item{x2}{ vector of y-coordinates defining the prior grid }
  \item{xrest}{ value for remaining parameters - default is light attenuation }
  \item{subset}{ evaluate subset of segments - default uses all }
  \item{initialize.x}{logical - create initial points for x? }
 
%% initialize.x

  %% light.quantile
  \item{chain}{ chain object from estimation }
  \item{day}{ POSIXct vector of date-times }
  \item{seg}{ desired segment }
  \item{prob}{ probability level for quantile }
  \item{start}{known position of release  }
  \item{end}{known position of recapture }
  %% show.segment
  
  \item{segment}{ vector of segment data }
  \item{light}{ vector of light data }
  \item{k}{ desired segment to show }
  \item{n}{ length of vector to evaluate  }
   \item{...}{additional arguments to be passed to plot}
}
\details{
 The primary function here is \code{position.logp}, for initializing the estimation
 for \code{\link{solar.model}} and \code{\link{metropolis0}}. 
}
\value{

}
\references{ }
\author{Michael D. Sumner }
\note{

}
\seealso{ }
\examples{

}
\keyword{manip }
\keyword{dplot }