recurAlign <-
function(testSegment, refSegment, recursion, lookahead)
{

if (length(testSegment) < recursion$minWidth)
{
    alignedSegment <- testSegment
    return(alignedSegment)
}

if (var(testSegment)==0 | var(refSegment)==0)
{
    alignedSegment<-testSegment
    return(alignedSegment)
}


lag <- FFTcorr(testSegment,refSegment,recursion$shift)

## stop if the segment is perfectly aligned and there is no need to lookahead

alignedSegment <- testSegment


if (abs(lag) < length(testSegment))
{
    alignedTemp <- shift(testSegment,lag)

    if (var(alignedTemp)<0.000001) 
   {
       return(alignedSegment)
    }

    CorrCoef<-corrcoef_aligned(refSegment,alignedTemp,recursion$step)
if (is.na(CorrCoef)) CorrCoef=0
        if (CorrCoef>=recursion$acceptance)
     {
  
        alignedSegment <-alignedTemp
    }else{
        if (var(testSegment)==0 || var(refSegment)==0)
         {

            alignedSegment<-testSegment
            return(alignedSegment)
        }
        CorrCoef<-corrcoef_aligned(refSegment,alignedSegment,recursion$step)
    }
}

CorrCoef<-corrcoef_aligned(refSegment,alignedSegment,recursion$step)
if (is.na(CorrCoef)) CorrCoef=0


# Can be adjusted the recursion stops if the resemblance between the
# referebce and the segment of interest is e.g. 98%

if (CorrCoef>=recursion$resamblance) 
{return(alignedSegment)}

# If the middle point is not the local min then divide


mid <- findMid(alignedSegment,refSegment)



if (is.na(mid)){
return(alignedSegment)
}

firstSH<- alignedSegment[1 : mid]
firstRH<- refSegment[1 : mid]
secSH <- alignedSegment[(mid+1):(length(alignedSegment))]
secRH <- refSegment[(mid+1):(length(refSegment))]
alignedSeg1 <- recurAlign(firstSH,firstRH,recursion, lookahead)
alignedSeg2 <- recurAlign(secSH,secRH,recursion, lookahead)
alignedSegment <- c(alignedSeg1,alignedSeg2)


return(alignedSegment)

}
