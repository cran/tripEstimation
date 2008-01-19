\name{bin.pimg}
\alias{bin.pimg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin.pimg(pimg, xy, w = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pimg}{ ~~Describe \code{pimg} here~~ }
  \item{xy}{ ~~Describe \code{xy} here~~ }
  \item{w}{ ~~Describe \code{w} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(pimg,xy,w=1) {

  xbnd <- pimg$xbound
  ybnd <- pimg$ybound
  
  ## Bin the locations into the global image coords
  i <- ceiling(xbnd[3]*(xy[,1]-xbnd[1])/(xbnd[2]-xbnd[1]))
  j <- ceiling(ybnd[3]*(xy[,2]-ybnd[1])/(ybnd[2]-ybnd[1]))
  ## Delete those points outside the global image
  keep <- (i >= 1 & i <= xbnd[3] & j >= 1 & j <= ybnd[3])
  if(any(keep)) {
    i <- i[keep]
    j <- j[keep]
    
    ## Expand image to new size
    if(is.null(pimg$image)) {
      irange <- range(i)
      jrange <- range(j)
      off <- c(irange[1],jrange[1])
      img <- matrix(0,diff(irange)+1,diff(jrange)+1)
    } else {
      irange0 <- pimg$offset[1]+c(0,nrow(pimg$image)-1)
      jrange0 <- pimg$offset[2]+c(0,ncol(pimg$image)-1)
      irange <- range(i,irange0)
      jrange <- range(j,jrange0)
      off <- c(irange[1],jrange[1])
      if(all(irange==irange0) && all(jrange==jrange0)) {
        ## Keep original image
        img <- pimg$image
      } else {
        ## Expand image
        img <- matrix(0,diff(irange)+1,diff(jrange)+1)
        img[(irange0[1]-off[1]+1):(irange0[2]-off[1]+1),
            (jrange0[1]-off[2]+1):(jrange0[2]-off[2]+1)] <- pimg$image
      }
    }
    
    ## Add binned points to new image
    img <- img + w*tabulate(nrow(img)*(j-off[2])+i+(1-off[1]),nbins=prod(dim(img)))
    
    pimg <- list(xbound=xbnd,
                 ybound=ybnd,
                 offset=off,
                 image=img)
    class(pimg) <- c("pimg", "list")
  }
  pimg
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}