localRecurAlign <-
function(testSegment, refSegment,recursion,isSegment,lookahead)
{
# Input: recursion$minSegWidth
#                  minInbetweenWidth
#                  resamblance
#                  acceptance
#                  segShift
#                  inbetweenShift
#       isSegment == true takes segment parameters
# Author: L.Hedjazi, ICAN Paris 2013

if (!is.vector(testSegment)) {stop('!is.vector(testSegment)')}

if (!is.vector(refSegment)) {stop('!is.vector(refSegment)')}


if (length(refSegment) != length(testSegment))
 {
    stop('Reference and testSegment of unequal lengths')

}else {if (length(refSegment)== 1)
    {
    stop('Reference cannot be of length 1')
    }
}

recursion$minWidth<-recursion$minSegWidth

if (isSegment==TRUE)
{
    recursion$shift<-recursion$segShift
}else{
    recursion$shift<-recursion$inbetweenShift
}


info<-recurAlign(testSegment,refSegment,recursion,lookahead)

return(info)

}
