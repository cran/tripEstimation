"satellite.model" <-
function(day,X,
                           proposal.x,proposal.z,
                           mask.x,mask.z,
                           fix.release=T,fix.recapture=T,
                           start.x,start.z,
                           posn.sigma=1,
                           speed.dist="gamma",speed.mean,speed.sd) {

if (length(day) != nrow(X)) stop("length of times should match number of locations")

  ##
  ## Utility functions
  ##

  ## (Great Circle) Distance
  dist <- function(a,b) {
    r <- cos(pi/180*a[,2])*cos(pi/180*b[,2])*
      cos(pi/180*(b[,1]-a[,1]))+sin(pi/180*a[,2])*sin(pi/180*b[,2])
    6378.137*acos(fast.pmin(r))
  }

## faster pmin/pmax 
## used by logp.behavioural
fast.pmax <- 
function(x) {z <- x<1e-06; x[z] <- 1e-06; x}
## used by dist
fast.pmin <- 
function(x) {z <- x>1; x[z] <- 1; x}


  ##
  ## Positional contribution to the log posterior
  ##
  logp.position <- function(x) {
    rowSums(dnorm(X,x,posn.sigma,log=T))
  }


  ##
  ## Behavioural contribution to the log posterior
  ##
  
  ## Times between observations
  dt <- diff(unclass(day)/3600)

  if(speed.dist=="gamma") {
    alpha <- speed.mean^2/speed.sd^2
    beta <- speed.mean/speed.sd^2
    logp.behavioural <- function(k,x1,z,x2) {
      ## Average speed from x1 to z to x2
      spd <- fast.pmax(dist(x1,z)+dist(z,x2))/dt[k]
      dgamma(spd,alpha,beta,log=T)
    }
  } else {
    log.sigma <- sqrt(log(1+speed.mean^2/speed.sd^2))
    log.mu <- log(speed.mean)-log.sigma^2/2
    logp.behavioural <- function(k,x1,z,x2) {
      ## Average speed from x1 to z to x2
      spd <- (dist(x1,z)+dist(z,x2))/dt[k]
      dnorm(spd,log.mu,log.sigma,log=T)
    }
  }

  ##
  ## Locations to be held fixed
  ##
  n <- length(day)
  fixed.x <- rep(FALSE,n)
  fixed.x[1] <- fix.release
  fixed.x[n] <- fix.recapture

  
  ## Return a list with all model components
  list(## Number of locations
       n = n,
       ## The function for generating proposal x's
       proposal.x=proposal.x,
       ## The function for generating proposal z's
       proposal.z=proposal.z,
       ## The mask for the x's
       mask.x=mask.x,
       ## The mask for the z's
       mask.z=mask.z,
       ## Positional contribution to the log posterior 
       logp.position=logp.position,
       ## Behavioural contribution to the log posterior 
       logp.behavioural=logp.behavioural,
       ## Locations to be held fixed
       fixed.x=fixed.x,
       ## Suggested starting points
       start.x=start.x,
       start.z=start.z)
}

