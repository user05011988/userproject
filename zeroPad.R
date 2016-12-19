zeroPad <-
function(peak, maxLen)
{
if(!is.vector(peak))
 {
    stop('!isvector(peak)')
 }

if(!is.null(dim(peak)))
{
    stop('peak should be a single row vector!')
}

zerosCnt <- maxLen - length(peak)
if(zerosCnt > 0)
 {
    leftPaddCnt <- floor(zerosCnt /2 )
    rightPaddCnt <- zerosCnt - leftPaddCnt
    peak <- c(rep(0,leftPaddCnt),peak,rep(0,rightPaddCnt))
 }
return(peak)

}
