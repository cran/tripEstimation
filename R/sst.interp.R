sst.interp <-
function(x, resolution = NULL, alpha = 0.05) {
    if (!length(resolution) == 2) stop("resolution must be specified, and be of length 2")
    if (length(dim(x$z)) == 2) {
        return( sst.interp1(x, resolution = resolution, alpha = alpha))

    } else {
        i1 <- list(x = x$x, y = x$y, z = x$z[,,1])
        i1 <- sst.interp1(i1, resolution = resolution, alpha = alpha)
        res <- list(x = i1$x, y = i1$y, z = array(i1$z, c(dim(i1$z), dim(x$z)[3])), t = x$t)
        for (i in 2:dim(x$z)[3]) res$z[,,i] <- sst.interp1(list(x = x$x, y = x$y, z = x$z[,,i]),
                                                           resolution = resolution, alpha = alpha)$z
    }
    #class(res) <- c("genimg", class(res))
    res

}

