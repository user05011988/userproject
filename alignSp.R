alignSp <-
function(refSp, refSegments, intSp,intSegments,recursion,MAX_DIST_FACTOR, MIN_RC)
{
# Input: refSp - reference spectrum
#        intSp - spectrum of interest
#        refSegments$used - reference
#        intSegments$refInd - matched reference segments
#        recursion - parameters for recursive alignment
# Output: alignedSpectrum
#         extendedSegments
#Author: L. Hedjazi, ICAN Paris 2013

if(length(refSp) != length(intSp))
{
    stop('length(refSp) != length(intSp)')
}

specLen <- length(refSp)

alignedSpectrum <- rep(NaN,specLen)
prevGeneralEnd <- 0

iSegmentInd <- 1
intSegLength<-length(intSegments$Peaks)
refSegLength<-length(refSegments$Peaks)
extensionCount<-0
extendedSegments<-NA
extensionInfo<-list()

###########################################
while (iSegmentInd <= intSegLength)
{
    ## Equi. to iSegment = intSegments[iSegmentInd]
    iSegment<-list()
     
    iSegment$start <- intSegments$start[iSegmentInd]
    iSegment$PeakLeftBoundary <- intSegments$PeakLeftBoundary[[iSegmentInd]]
    iSegment$PeakRightBoundary <- intSegments$PeakRightBoundary[[iSegmentInd]]
    iSegment$Peaks <- intSegments$Peaks[[iSegmentInd]]
    iSegment$end <- intSegments$end[iSegmentInd]
    iSegment$centre <- intSegments$centre[iSegmentInd]
    iSegment$refIndex <- intSegments$refIndex[iSegmentInd]

    if(is.na(iSegment$refIndex))
    {
      
        iSegmentInd <- iSegmentInd + 1
        next
    }
    ###### Segment of interest #######
    iLeftB<-iSegment$PeakLeftBoundary
    iRightB<-iSegment$PeakRightBoundary
    iPeaks<-iSegment$Peaks
    
    ###### Corresponding Reference segment ######
    referenceInd<-iSegment$refIndex



    #refSegment <- refSegments[referenceInd]


    refStart <- refSegments$start[referenceInd]
    refLeftB<-refSegments$PeakLeftBoundary[[referenceInd]]
    refRightB<-refSegments$PeakRightBoundary[[referenceInd]]
    rPeaks<-refSegments$Peaks[[referenceInd]]
    refCentre <- refSegments$centre[referenceInd]
    refUsed <- refSegments$used[referenceInd]
    iStart <- iSegment$start

    ######## Find joint starting position #########
    iSegmentIndex<-iSegmentInd
    refIndex<-referenceInd

    generalStart <- min(iStart, refStart)
    
    #search for the general start preventing overlapping with the previous segments
    while (TRUE)
     {
        #no segments before
        if ((iSegmentIndex<=1) && (refIndex<=1))
        { 
          break
        }

        # the segment of interest is first
        if (iSegmentIndex<=1)
         {
            if (generalStart<refSegments$end[refIndex-1])
             {
                generalStart<-min(generalStart,refSegments$start[refIndex-1])
                refLeftB<-c(refSegments$PeakLeftBoundary[[refIndex-1]], refLeftB)
                refRightB<-c(refSegments$PeakRightBoundary[[refIndex-1]],refRightB)
                rPeaks<-rbind(refSegments$Peaks[[refIndex-1]],rPeaks)
                extensionCount<-extensionCount+1
            }

            break
        }

        #the reference segment is first

        if (refIndex<=1)
        {
            if (generalStart<intSegments$end[iSegmentIndex-1])
             {
                generalStart<-min(generalStart,intSegments$start[iSegmentIndex-1])
                iLeftB<-c(intSegments$PeakLeftBoundary[[iSegmentIndex-1]],iLeftB)
                iRightB<-c(intSegments$PeakRightBoundary[[iSegmentIndex-1]],iRightB)
                iPeaks<-rbind(intSegments$Peaks[[iSegmentIndex-1]],iPeaks)
                extensionCount<-extensionCount+1
            }

            break
        }

        #both segments end before the general start
        if ((intSegments$end[iSegmentIndex-1]<=generalStart)&&(refSegments$end[refIndex-1]<=generalStart))
         {
           break
        }

        #both segments end after the general start (in fact impossible)
        if ((generalStart<intSegments$end[iSegmentIndex-1])&&(generalStart<=refSegments$end[refIndex-1]))
         {
            generalStart<-min(c(generalStart,refSegments$start[refIndex-1],intSegments$start[iSegmentIndex-1]))

            iLeftB<-c(intSegments$PeakLeftBoundary[[iSegmentIndex-1]], iLeftB)
            iRightB<-c(intSegments$PeakRightBoundary[[(iSegmentIndex-1)]],iRightB)
            refLeftB<-c(refSegments$PeakLeftBoundary[[refIndex-1]],refLeftB)
            refRightB<-c(refSegments$PeakRightBoundary[[refIndex-1]],refRightB)
            iPeaks<-rbind(intSegments$Peaks[[iSegmentIndex-1]],iPeaks)
            rPeaks<-rbind(refSegments$Peaks[[refIndex-1]],rPeaks)
            iSegmentIndex<-iSegmentIndex-1
            refIndex<-refIndex-1
            extensionCount<-extensionCount+1
            next
         }

        #the segment of interest ends after the general start
        if (generalStart<intSegments$end[iSegmentIndex-1])
         {
            generalStart<-min(generalStart,intSegments$start[iSegmentIndex-1])
            
            iLeftB<-c(intSegments$PeakLeftBoundary[[iSegmentIndex-1]],iLeftB)
            iRightB<-c(intSegments$PeakRightBoundary[[iSegmentIndex-1]],iRightB)
            iPeaks<-rbind(intSegments$Peaks[[iSegmentIndex-1]],iPeaks)
            iSegmentIndex<-iSegmentIndex-1
            extensionCount<-extensionCount+1
            next
        }

        #the reference segment ends after the general start
        if (generalStart<refSegments$end[refIndex-1])
         {
            generalStart<-min(generalStart,refSegments$start[refIndex-1])
            
            extensionCount<-extensionCount+1
            refLeftB<-c(refSegments$PeakLeftBoundary[[refIndex-1]],refLeftB)
            refRightB<-c(refSegments$PeakRightBoundary[[refIndex-1]],refRightB)
            rPeaks<-rbind(refSegments$Peaks[[(refIndex-1)]],rPeaks)
            refIndex<-refIndex-1
            next
        }
    }

    #search for 'generalEnd' preventing overlapping with the following segments
    iEnd <- iSegment$end
    refEnd <- refSegments$end[referenceInd]
    generalEnd <- max(iEnd,refEnd)


    while(TRUE)
     {
        # No segments ahead
        if ((iSegmentInd>=intSegLength)&&(referenceInd>=refSegLength))
         {
            break
         }

        # No segment ahead in spectrum of interest
        if (iSegmentInd>=intSegLength)
         {
            if (generalEnd>refSegments$start[referenceInd+1])
             {
                generalEnd<- max(generalEnd,refSegments$end[referenceInd+1])
                refLeftB<-c(refSegments$PeakLeftBoundary[[referenceInd+1]],refLeftB)
                refRightB<-c(refSegments$PeakRightBoundary[[referenceInd+1]],refRightB)
                rPeaks<-rbind(rPeaks,refSegments$Peaks[[(referenceInd+1)]])
                extensionCount<-extensionCount+1
                break
            }
            break
        }

        # No segment ahead in reference spectrum
        if (referenceInd>=refSegLength)
        {
            if (generalEnd>intSegments$start[iSegmentInd+1])
             {
                generalEnd<-max(generalEnd,intSegments$end[iSegmentInd+1])
                iLeftB<-c(iLeftB,intSegments$PeakLeftBoundary[[iSegmentInd+1]])
                iRightB<-c(iRightB,intSegments$PeakRightBoundary[[iSegmentInd+1]])
                iPeaks<-rbind(iPeaks,intSegments$Peaks[[iSegmentInd+1]])
                extensionCount<-extensionCount+1
                break
            }
            break
        }

        #Both subsequent segments start after the current general end

        if ((generalEnd <= intSegments$start[iSegmentInd+1]) && (generalEnd <= refSegments$start[referenceInd+1]))
         {
            break
        }

        #Both segments starts before the General End
        if ((generalEnd>intSegments$start[iSegmentInd+1])&&(generalEnd>refSegments$start[referenceInd+1]))
         {
            generalEnd<-max(c(generalEnd,intSegments$end[iSegmentInd+1],refSegments$end[iSegmentInd+1]))
            iLeftB<-c(iLeftB, intSegments$PeakLeftBoundary[[iSegmentInd+1]])
            iRightB<-c(iRightB, intSegments$PeakRightBoundary[[iSegmentInd+1]])
            refLeftB<-c(refLeftB,refSegments$PeakLeftBoundary[[referenceInd+1]])
            refRightB<-c(refRightB, refSegments$PeakRightBoundary[[referenceInd+1]])
            iPeaks<-rbind(iPeaks,intSegments$Peaks[[iSegmentInd+1]])
            rPeaks<-rbind(rPeaks,refSegments$Peaks[[referenceInd+1]])
            referenceInd<-referenceInd+1
            iSegmentInd<-iSegmentInd+1
            extensionCount<-extensionCount+1
            next
        }

        #If the next segment in intSp starts before the general end
        if ((generalEnd>intSegments$start[iSegmentInd+1])&&(is.na(intSegments$refIndex[iSegmentInd+1])))
        {
            generalEnd<-max(generalEnd,intSegments$end[iSegmentInd+1])
            iLeftB<-c(iLeftB,intSegments$PeakLeftBoundary[[iSegmentInd+1]])
            iRightB<-c(iRightB,intSegments$PeakRightBoundary[[iSegmentInd+1]])
            iPeaks<-rbind(iPeaks,intSegments$Peaks[[iSegmentInd+1]])
            iSegmentInd<-iSegmentInd+1
            extensionCount<-extensionCount+1
            next
        }else{  if (generalEnd>intSegments$start[iSegmentInd+1])
           {
            refInd<-referenceInd+1
            referenceInd<-intSegments$refIndex[iSegmentInd+1]
            generalEnd<-max(c(generalEnd,intSegments$end[iSegmentInd+1],refSegments$end[referenceInd]))
            iLeftB<-c(iLeftB,intSegments$PeakLeftBoundary[[iSegmentInd+1]])
            iRightB<-c(iRightB,intSegments$PeakRightBoundary[[iSegmentInd+1]])
            iPeaks<-rbind(iPeaks,intSegments$Peaks[[iSegmentInd+1]])

            for (i in (refInd:referenceInd))
             {
                refLeftB<-c(refLeftB,refSegments$PeakLeftBoundary[[i]])
                refRightB<-c(refRightB,refSegments$PeakRightBoundary[[i]])
                rPeaks<-rbind(rPeaks,refSegments$Peaks[[i]])

            }
            iSegmentInd<-iSegmentInd+1
            extensionCount<-extensionCount+1
            next
           }
          }

        #If the next segment in refSp starts before the general end

        if ((generalEnd>refSegments$start[referenceInd+1])&&(is.na(refSegments$used[referenceInd+1])))
         {  
            generalEnd<-max(generalEnd,refSegments$end[referenceInd+1])
            refLeftB<-c(refLeftB,refSegments$PeakLeftBoundary[[referenceInd+1]])
            refRightB<-c(refRightB,refSegments$PeakRightBoundary[[referenceInd+1]])
            rPeaks<-rbind(rPeaks,refSegments$Peaks[[referenceInd+1]])
            referenceInd<-referenceInd+1
            extensionCount<-extensionCount+1
            next
        }else{ if (generalEnd>refSegments$start[referenceInd+1])
           {
            iSegIndex<-iSegmentInd+1
            iSegmentInd<-refSegments$used[referenceInd+1]
            generalEnd<-max(c(generalEnd,intSegments$end[iSegmentInd],refSegments$end[referenceInd+1]))
            for (i in (iSegIndex:iSegmentInd))
             {
                iLeftB<-c(iLeftB,intSegments$PeakLeftBoundary[[i]])
                iRightB<-c(iRightB,intSegments$PeakRightBoundary[[i]])
                iPeaks<-rbind(iPeaks,intSegments$Peaks[[i]])

             }
            refLeftB<-c(refLeftB,refSegments$PeakLeftBoundary[[referenceInd+1]])
            refRightB<-c(refRightB,refSegments$PeakRightBoundary[[referenceInd+1]])
            rPeaks<-rbind(rPeaks,refSegments$Peaks[[referenceInd+1]])
            referenceInd<-referenceInd+1
            extensionCount<-extensionCount+1
            next
            }
          }
      }

    refSegment    <- refSp[generalStart : generalEnd]
    testSegment   <- intSp[generalStart : generalEnd]

    Bnd<-list()
    Bnd$refLeftB<-refLeftB-generalStart+1
    Bnd$refRightB<-refRightB-generalStart+1
    Bnd$iLeftB<-iLeftB-generalStart+1
    Bnd$iRightB<-iRightB-generalStart+1

    alignedSegment <- localRecurAlign(testSegment, refSegment, recursion,TRUE,TRUE)  


    if (any(is.nan(alignedSegment)))
     {
        readline("any(is.nan(alignedSegment))")
    }
    alignedSpectrum[generalStart : generalEnd] <- alignedSegment

    #############################################################

    ##-- align 'grass':

    grassStart  <- prevGeneralEnd + 1
    grassEnd    <- generalStart - 1

    if( grassEnd > grassStart )
    {
        refSegment <- refSp[ grassStart : grassEnd ]
        testSegment  <- intSp[ grassStart : grassEnd ]

        # do not want to visualize grass

        alignedSegment <- localRecurAlign(testSegment, refSegment, recursion,FALSE,TRUE)
        alignedSpectrum[grassStart : grassEnd] <- alignedSegment
    }
    prevGeneralEnd <- generalEnd

    # don't forget to increase the counter!!!
    iSegmentInd <- iSegmentInd + 1
}

if(extensionCount > 0)
 {
    extensionInfo$extensionCount <- extensionCount
 } 


##########################################################
if(!(is.na(extendedSegments)))
 {
    maxExtensionCount <- -1
    if (extendedSegments == extendedSegments)
     {
        if(extendedSegments$extensionCount > maxExtensionCount)
         {
            maxExtensionCount <- extendedSegments$extensionCount
            maxExtInd <- extendedSegments$extensionSegmentInd
         }
     }
    maxExtensionInfo$extensionSegmentInd <- maxExtInd
    maxExtensionInfo$extensionCount <- maxExtensionCount
    extendedSegments <- c(extendedSegments, maxExtensionInfo)
  }

grassStart<- prevGeneralEnd + 1
grassEnd <- specLen

if( grassEnd > grassStart )
{
    refSegment<- refSp[grassStart : grassEnd]
    testSegment<- intSp[grassStart : grassEnd]

    alignedSegment <- localRecurAlign(testSegment, refSegment, recursion, FALSE,TRUE)
    alignedSpectrum[grassStart : grassEnd]<- alignedSegment
}
#return(list(alignedSpectrum=alignedSpectrum, extendedSegments=extendedSegments))
 return(alignedSpectrum)
}
