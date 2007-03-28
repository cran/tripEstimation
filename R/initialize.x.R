"initialize.x" <-
function(model, x1,x2,xrest=NULL) {

  ## Extract model components
  n <- model$n
  logp.position <- model$logp.position
  mask.x <- model$mask.x

  ## x1, x2 and logp at max
  x1.max <- as.double(rep(NA,n))
  x2.max <- as.double(rep(NA,n))
  logp.max <- as.double(rep(NA,n))

  print(paste("i in", length(x1)))

  
  ## Loop over grid
  for(i in 1:length(x1)) {
    print(i)
    for(j in 1:length(x2)) {
      x <- cbind(rep(x1[i],n),rep(x2[j],n),xrest)
      logp <- logp.position(x)
      ## Keep point with max logp that satisfies the mask
      keep <- mask.x(x[ , 1:2]) & (is.na(logp.max) | (logp > logp.max))
      logp.max[keep] <- logp[keep]
      x1.max[keep] <- x1[i]
      x2.max[keep] <- x2[j]
    }
  }

  ## The full x at the maximum
  x <- cbind(x1.max,x2.max,xrest)
  ## Replace fixed points
  ##x[model$fixed.x,] <- model$start.x[model$fixed.x,]
  x
}

"position.logp" <- 
function (model, x1, x2, xrest = NULL, subset = 1:model$n, initialize.x = TRUE,
	start = NULL, end = NULL) 
{
    n <- model$n
    logp.position <- model$logp.position
    mask.x <- model$mask.x
    logp <- array(0, c(length(x1), length(x2), length(subset)))
    mask <- array(0, c(length(x1), length(x2), length(subset)))
    print(paste("i in ", length(x1)))
    for (i in 1:length(x1)) {
        print(i)
        for (j in 1:length(x2)) {
            x <- cbind(rep(x1[i], n), rep(x2[j], n), xrest)
            logp[i, j, ] <- logp.position(x)[subset]
            mask[i, j, ] <- mask.x(x)[subset]
        }
    }
    res <- list(x = x1, y = x2, logp = logp, mask = mask)
    
    

    if (initialize.x) {
	#logp.init <- function(d.model, logp, ) {
	nq = 6
	xy <- expand.grid(x = x1, y = x2)
	logp.quantile <- logp > 0 
    	for (i in 2:(n - 1)) logp.quantile[, , i] <- 
		logp[, , i] > quantile(logp[, , i], seq(0, 1, length = nq), na.rm = TRUE)[nq - 1]
	logp <- logp.quantile
    	xx <- matrix(0, nrow = n, ncol = 3)
    	for (i in 2:(n - 1)) {
    	
    	
        	logp.quantile[,,i] <- ((logp[, , i - 1] * logp[, 
        	    , i]) & (logp[, , i] * logp[, , i + 1]) == 1) * mask[,,i]
        	 
        	#this <- this * mask[,,i]
        	xx[i, 1:2] <- apply(xy[as.logical(logp.quantile[,,i] ), ], 2, mean, na.rm = T)
    	}
    
#browser()
	res$logp.quantile <- logp.quantile
	X <- xx
	if (!is.null(start)) X[1,1:2] <- start
	if (!is.null(start)) X[nrow(X), 1:2] <- end

	require(zoo)
	res$naX <- X
	X <- as.matrix(na.approx(X, na.rm = FALSE))
	res[["X"]] <- X
	}
	class(res) <- c("diag", class(res))
	res
    }	


 

 "light.quantile" <-
 function(model,chain,day,seg,prob=c(0.025,0.5,0.975)) {
   n <- length(day)
   ## Parameters for this segment
   xs <- t(chain$x[seg,,])
   ## Matrix of quantiles
   qs <- matrix(0,n,length(prob))
   for(i in 1:n) {
     ## Predictions for this time over all xs
     lgt <- model$light.predict(xs,day[i])
     qs[i,] <- quantile(lgt,prob=prob)
   }
   qs
 }

"show.segment" <- function (model, chain, segment, day, light, k, n = 50, ...) 
{
    segment <- factor(segment)
    day.seg <- day[segment == k]
    plot(day.seg, light[segment == k], ...)
     text(mean(day.seg), mean(light[segment == k])-50, k, font = 2)
    ds <- seq(min(day.seg), max(day.seg), length = n)
    qs <- light.quantile(model, chain, ds, k)
    matlines(ds, qs, lty = 1, col = "blue", lwd = 2)
   
}