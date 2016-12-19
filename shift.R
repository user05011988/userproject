shift <-
function(seg, lag)
{
if ((lag == 0) || (lag >= length(seg)))
{
    shiftedSeg <- seg
    return(shiftedSeg)
}

if (lag > 0)
{
    ins <- rep(1,lag) * seg[1]
    shiftedSeg <-c(ins,seg[1:(length(seg) - lag)])
}else{ if (lag < 0)
    {
    lag <- abs(lag)
    ins <- rep(1,lag) * seg[length(seg)]
    shiftedSeg <-c(seg[(lag+1):length(seg)],ins)
    }
}

return(shiftedSeg)
}
