comparePeaks <-
function(dpReference,refPeaks, dpSpectr, curIPeak, MAX_DIST_FACTOR, tryDoubleReference)
{
# function iSimilarPeakInd <- comparePeaks(SPeakCurrent, refPeaks)
# compare current peak SPeakCurrent with all the peaks in refPeaks array and
# return index of similar peak
# dpReference - reference spectrum (whole!)
# dpSpectr - 'input' spectrum (whole!)
# curPeak - structure (.start, .end, .centre) of the current peak
# which we're aligning to our reference (one peak)
# refPeaks cell array of reference peak structures. We're looking for the
# best matching peak in these reference peak structures.
# MAX_DIST_FACTOR - 'zero' of distance
# Author: L. Hedjazi, ICAN Paris 2013

if (nargs()< 5)
{
    debug<-TRUE
    print('comparePeaks in debug mode!!!')
    MAX_DIST_FACTOR <- 50
    #load('../data/testPeaks')
    dpReference <- dpSpectr
}

iPeaksCount <- length(refPeaks$Peaks)
if (iPeaksCount<1)
{
    stop('Not enought peaks to compare')
}

#evluate comparison parameters

maxDistDeviation <- (curIPeak$end - curIPeak$start) * MAX_DIST_FACTOR
dMinParam   <- rep(0, iPeaksCount)
corCoeffs   <- rep(0, iPeaksCount)
distCoeffs  <- rep(0, iPeaksCount)

for (peakInd in 1:iPeaksCount)
{
refPeak<-list()
    if("used" %in% names(refPeaks)&& !(is.na(refPeaks$used[peakInd])))
    {
        #no matter to check this reference sicne it has been used already
        next
    }

    if((refPeaks$centre[peakInd] - curIPeak$centre) < -maxDistDeviation)
     {  next
     }
  
    if((refPeaks$centre[peakInd] - curIPeak$centre) > maxDistDeviation)
     {   break
     }

    if(tryDoubleReference && (peakInd < iPeaksCount))
     {
        refPeak$start   <- refPeaks$start[peakInd]
        refPeak$end     <- refPeaks$end[peakInd+1]
        refPeak$centre <- mean(refPeak$start, refPeak$end)
    }else{
        refPeak$start<- refPeaks$start[peakInd]
        refPeak$PeakLeftBoundary<- refPeaks$PeakLeftBoundary[[peakInd]]
        refPeak$PeakRightBoundary<- refPeaks$PeakRightBoundary[[peakInd]]
        refPeak$Peaks<- refPeaks$Peaks[[peakInd]]
        refPeak$end<- refPeaks$end[peakInd]
        refPeak$centre<- refPeaks$centre[peakInd]
    }
    #evaluate relative distance

    dDistCurr <- 1 - abs(refPeak$centre - curIPeak$centre) / maxDistDeviation
    distCoeffs[peakInd] <- dDistCurr
    
    ##-- little optimisation: if we got negative 'dDistCurr' value (our
    ##peaks are TOO far from each other, no matter to do FFT
    ##cross-correlation for them since we're interested only in positive rC
    ##values
    if(dDistCurr < 0)
     {
        dMinParam[peakInd] <- dDistCurr
        next
    }
    
    #evaluate cross-correlation
    refPeakShape <- dpReference[refPeak$start : refPeak$end]

    if (curIPeak$start<=0||curIPeak$end>length(dpSpectr))
     {
        dMinParam[peakInd]<-0
    }else{
        targetPeak <- dpSpectr[curIPeak$start : curIPeak$end]

        maxLen <- max(length(refPeakShape), length(targetPeak))
        fCorrCurr <- getCorellation(zeroPad(refPeakShape, maxLen), zeroPad(targetPeak, maxLen), maxDistDeviation)

        #get minimal parameter
        dMinParam[peakInd] <- min(dDistCurr,fCorrCurr)

        corCoeffs[peakInd]  <- fCorrCurr
    }
    }

#Get simillar peak index
rC <- max(dMinParam)
iSimilarPeakInd<-which.max(dMinParam)
distCoeff <- distCoeffs[iSimilarPeakInd]
corrCoeff <- corCoeffs[iSimilarPeakInd]


return(list(rC=rC, iSimilarPeakInd=iSimilarPeakInd, distCoeff=distCoeff, corrCoeff=corrCoeff))

}
