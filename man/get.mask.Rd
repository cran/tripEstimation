\name{get.mask}
\alias{get.mask}
\alias{mkDepthMask}
\alias{mkSmall}
\alias{mkTempMask}

\title{Create, access and manipulate spatial masks}
\description{
 Spatial masks are stored using the xyz-list structure used by 
 \code{\link{image}} or as a series of masks stored as bits in the z-component
 as matrix or array object.     \code{get.mask} is used to extract a specific
 mask from the binary storage, and mkSmall can be used to quickly down-sample
 an existing mask or image. The two "make" functions are included as examples
 of creating such a mask from a local database. 
}
\usage{
get.mask(masks, k)

mkSmall(lst, thin = 10)

mkDepthMask(depth.mins, depthdata = NULL)

mkTempMask(times, temp.ranges, xlim = c(0.5, 359.5), ylim = c(-89.5, 89.5), sstdata = "reynolds", interp = NULL) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{

%% get.mask
  \item{masks}{ A list object with components x, y, and z containing spatial masks }
  \item{k}{ specifies the k-th mask}

  %% mkSmall
  \item{lst}{ an xyz-list structure with z containing either a matrix or array }
  \item{thin}{ integer factor to down-sample grid }

%% mkDepthMask

  \item{depth.mins}{ depth values to compare to database of topography }
  \item{depthdata}{ xyz-list structure of topography }


%% mkTempMask

  \item{times}{ date-times at which temperatures are to be compared }
  \item{temp.ranges}{ matrix of two columns - min/max of temperature for given period }
  \item{xlim}{ x-range to query from temperature database }
  \item{ylim}{ y-range to query from temperature database }
  \item{sstdata}{ character describing sst database }
  \item{interp}{ logical - should gappy data be interpolated?}

}
\details{
 \code{mkDepthMask} and \code{mkTempMask} are included to show operational examples
 used by our group.  Future publications will refer to this code, or to similar 
 functions. 
}
\value{

}
\references{  }
\author{ Michael D. Sumner}
\note{
}
\seealso{ \code{\link{mkLookup}} for the use of these masks to query individual
locations and locations measured over time.                      
See \code{\link{bits}} for the underlying mechanism to set and get mask bits.  

For the use of the xyz-list structure see \code{\link{image}}.  }
\examples{
      
      data(volcano)
      d <- list(x = seq(-10, 10, length = nrow(volcano)), 
                y = seq(-5, 5, length = ncol(volcano)),
                z = array(0, c(nrow(volcano), ncol(volcano), 2)) )
      mv <- min(volcano)
      
      for (i in 0:61) {
        blk <- (i \%/\% 31) + 1
        bit <- (i - 1) \%\% 31
        bits(d$z[,,blk], bit) <- volcano > (mv + i*1.6 )
      }
      for (i in 0:61) image(get.mask(d, i))
      
      ## an object with 62 masks is only twice the size of the source data
      object.size(d) / object.size(volcano)
      
      \dontrun{
      ## plot a smaller version
      image(get.mask(d, 20), 5)
      
       ## pretend we have only one masks
      lookup <- mkLookup(get.mask(d, 30), by.segment = FALSE)
      
      ## interactive to show use of lookup function
      image(get.mask(d, 30), main = "Click on the red (FALSE) and cream (TRUE) areas")
      for (i in 1:10) {x <- matrix(unlist(locator(1)), ncol = 2);text(x[1], x[2], lookup(x) > 0)}
      }
      
}
\keyword{manip }% at least one, from doc/KEYWORDS

