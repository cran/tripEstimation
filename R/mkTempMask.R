mkTempMask <- function (times, temp.ranges, xlim = c(0.5, 359.5), ylim = c(-89.5, 
    89.5), sstdata = "reynolds", interp = NULL) 
{
    nsegs <- length(times)
    if (nrow(temp.ranges) != nsegs) 
        stop("mismatch in length of times and temperature ranges")
	sst <- reynolds(time = times[1], xlim = xlim, ylim = ylim)
	xx <- sst$x
	yy <- sst$y
	if (!is.null(interp)) {
		#require(fields)
		#xx <- seq(min(sst$x), max(sst$x), by = interp)
		#yy <- seq(min(sst$y), max(sst$y), by = interp)
		#mat <- expand.grid(x = xx, y = yy)
		stop("interpolation not supported")
	}

    for (k in 1:nsegs) {
	   sst <- reynolds(time = times[k], xlim = xlim, ylim = ylim)
        if (k == 1) {
           
            nblocks <- (nsegs%/%31) + 1
            nlon <- length(xx)
            nlat <- length(yy)
            Mask <- list(x = xx, y = yy, z = array(integer(nblocks * 
                nlat * nlon), c(nlon, nlat, nblocks)))
        }
     

	if (!is.null(interp)) {
	   sst$z[is.na(sst$z)] <- min(sst$z, na.rm = TRUE)
	   int <- interp.surface(sst, mat)
	   sst <- list(x = xx, y = yy, z = matrix(int, nrow = length(xx)))
	
	}
        trange <- temp.ranges[k, ]
        mask <- sst$z >= trange[1] & sst$z <= trange[2]
        mask[is.na(sst$z)] <- FALSE
        blk <- (k%/%31) + 1
        bit <- (k - 1)%%31
        cat(k, "\n")

#plot(sstrange[,1], type = "l", col = "blue")
#lines(sstrange[,2], type = "l", col = "red")
#abline(v = k)
#image(xx, yy, mask, main = k)
#browser()
        bits(Mask$z[, , blk], bit) <- mask
    }
    Mask
}

