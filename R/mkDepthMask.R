"mkDepthMask" <-
function( depth.mins, depthdata = NULL) {
  topo <- fetch.topo(xlim = c(lon.min, lon.max), ylim = c(lat.min, lat.max), tfile = depthdata)
  nsegs <- length(depth.mins)
  nblocks <- (nsegs%/%31) + 1
  nlon <- dim(topo$z)[1]
  nlat <- dim(topo$z)[2]
  depthMask <- list(x = topo$x, y = topo$y,
              z = array(integer(nblocks*nlat*nlon),c(nlon,nlat,nblocks)))


  for (k in 1:length(depth.mins)) {

    dmin <- depth.mins[k]
  
    mask <- topo$z <= dmin
 
    ## The block and the bit within the block that contain the k-th mask
    blk <- (k%/%31)+1
    bit <- (k-1)%%31
    
    ## Pack into array of blocks
    bits(depthMask$z[,,blk],bit) <- mask
        cat(k, "\n")
  }
  depthMask
}

