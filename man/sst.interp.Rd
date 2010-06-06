\name{sst.interp}
\alias{sst.interp}
\alias{sst.interp1}

\title{
Spatial smoothing of grid data. 
}
\description{
Smooth gridded data, such as SST, to a new grid resolution.  
}
\usage{
sst.interp(x, resolution = NULL, alpha = 0.05)

sst.interp1(x, resolution = NULL, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 Object containing data, must have components "x", "y", "z" similar to
 \code{\link{image}} format. 
}
  \item{resolution}{
 Cell size (x, y) for resulting grids. 
}
  \item{alpha}{
 Value between 0 and 1 determining the degree of
          smoothing used. The default is 5\%, which means that the
          smoothed field at any point is calculated from the 5\% nearest
          points in the observed data.

}
}
\details{
  The smoothing is done by the locfit package. 
The concept for this was originally taken from the function
"write.sst.field" in kfsst package. This interface is faster and
slightly easier to use for estimation in the tripEstimation package, but
not implemented in as general form as that used by the kfsst package. 
}
\value{
 A list object with the same structure as \code{x}. 
}
\references{
  http://www.soest.hawaii.edu/tag-data/tracking/kfsst/
}
\author{
Michael D. Sumner
}
\seealso{
\code{\link{get.sst}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ spatial }% __ONLY ONE__ keyword per line
