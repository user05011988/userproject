matchSegments <-
function(refSp,intSp,intSegments,refSegments,MAX_DIST_FACTOR, MIN_RC)
{
# Matching of the segment of interest to the corresponding reference using
# Fuzzy logic approach
# Algorithm: - take segment of interest
#            - take reference segments
#            - calculate relative distance between them
#            - calculate relative resamblance between them
#            - find min value of relative distance and resamblance 
#            - use it as representative of similiarity between target...
#              and reference segments 
#            - find the segment that has the highest value of both relative
#            distance and resamblance
# Input:  intSegments - segments of spectrum of interest
#         refSegments- segments of reference spectrum
#         intSp - spectrum of interest
#         refSp - reference spectrum 
# Output: intsegment$refInd - reference segment or []
# Author: L. Hedjazi, ICAN Paris 2013
# 
intSegLength<-length(intSegments$Peaks)
rC1<-vector()

for (index in 1:intSegLength)
 {
        testSeg<-list()
        testSeg$start<- intSegments$start[index]
        testSeg$PeakLeftBoundary<- intSegments$PeakLeftBoundary[[index]]
        testSeg$PeakRightBoundary<- intSegments$PeakRightBoundary[[index]]
        testSeg$Peaks<- intSegments$Peaks[[index]]
        testSeg$end<- intSegments$end[index]
        testSeg$centre<- intSegments$centre[index]

    peaksCompared<-comparePeaks(refSp, refSegments, intSp, testSeg, MAX_DIST_FACTOR, FALSE)
	if (is.na(peaksCompared$rC)) peaksCompared$rC=0
    if ((peaksCompared$rC>MIN_RC) && (peaksCompared$rC!=0))
    {
        intSegments$refIndex[index]<-peaksCompared$iSimilarPeakInd
        rC1<-cbind(rC1,peaksCompared$rC)
        refSegments$used[peaksCompared$iSimilarPeakInd]<-index
    }else{
        intSegments$refIndex[index]<-NA
    }
}

startPos<-numeric(0)
endPos<-numeric(0)

return(list(testSegs=intSegments,refSegs=refSegments))

}
