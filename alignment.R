alignment=function(dataset,ppm) {
    print('Be patient. Gonna take a while. You should be writing, meanwhile.')
    
    # dataset<-dataset*0.2/median(dataset)
    peakList <- detectSpecPeaks(dataset,
      nDivRange = c(128),
      scales = seq(1, 16, 2),
      baselineThresh = quantile(dataset,0.60,na.rm=T),
      SNR.Th = -1,
      verbose=FALSE
    );
    resFindRef<- findRef(peakList);
    refInd <- resFindRef$refInd;
    # mm=apply(dataset,2,median)
    # ll=apply(dataset,1,function(x)cor(x,mm,method='spearman'))
    # refInd=which.max(ll)
    
    # Set maxShift
    maxShift = 30;
    aligneddataset <- dohCluster(dataset,
      peakList = peakList,
      refInd = refInd,
      maxShift = maxShift,
      acceptLostPeak = TRUE, verbose=FALSE);
    ## Normalization
    ##Segmentation and matching parameters
    #   if (ppm[1]<ppm[2]) ppm=rev(ppm)
    #   setupRSPA(-ppm)
    #   peakParam$ampThr=quantile(dataset,0.75,na.rm=T)
    #   
    #   
    #   
    # refSp<-dataset[suppressWarnings(selectRefSp(dataset,recursion$step)),]
    # 
    # aligneddataset=dataset
    # 
    # refSegments<- segmentateSp(refSp, peakParam)
    # 
    # ##segmentate a test datasetectrum
    # for (i in 1:dim(dataset)[1]) {
    #   print(paste('Spectrum',i))
    #   testSegments<- segmentateSp(dataset[i,], peakParam)
    #   attachedSegs<-attachSegments(refSegments,testSegments)
    #   suppressMessages(attach(attachedSegs))
    #   Segs<-matchSegments(refSp,dataset[i,],testSegmentsNew,refSegmentsNew,MAX_DIST_FACTOR, MIN_RC)
    #   suppressMessages(attach(Segs))
    #   aligneddataset[i,]<- suppressWarnings(alignSp(refSp,refSegs,dataset[i,],testSegs,recursion,MAX_DIST_FACTOR,MIN_RC))
    # }
    print('Done!')
    aligneddataset[is.na(aligneddataset)]=0
    return(aligneddataset)
 
}
