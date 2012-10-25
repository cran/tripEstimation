sst.interp1 <-
function(x, resolution = NULL,  alpha = 0.05) {

    ## removed by addition of namespace, MDS 2011-10-06
    ## cannot remove until 2.14 MDS 2011-10-10
    require(locfit)
    if(is.null(resolution)) stop("resolution must be specified")
    new.x <- seq(min(x$x), max(x$x), by = resolution[1])
    new.y <- seq(min(x$y), max(x$y), by = resolution[2])
    newgrid <- expand.grid(x = new.x,  y = new.y)

    x0 <- data.frame(expand.grid(x = x$x, y = x$y), z = as.vector(x$z))
    fit <- locfit(z ~ x:y, data = x0,  alpha = alpha)


    pred <- predict(fit, newdata = newgrid)

    list(x = new.x, y = new.y, z = matrix(pred, ncol = length(new.y), nrow = length(new.x)))

}

