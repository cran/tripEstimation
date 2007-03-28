"get.mask" <-
function (masks, k) 
{
    list(x = masks$x, y = masks$y, z = bits(masks$z[, , (k%/%31) + 1], (k - 1)%%31))
}

