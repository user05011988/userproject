alignment=function(dataset,ppm) {
  print('Be patient. Gonna take a while. You should be writing, meanwhile.')
  
  dataset<-dataset*0.2/median(dataset)
  ## Normalization
  ##Segmentation and matching parameters
  if (ppm[1]<ppm[2]) ppm=rev(ppm)
  setupRSPA(-ppm)
  peakParam$ampThr=quantile(dataset,0.75)
  
  
  
refSp<-dataset[suppressWarnings(selectRefSp(dataset,recursion$step)),]

aligneddataset=dataset

refSegments<- segmentateSp(refSp, peakParam)

##segmentate a test datasetectrum
for (i in 1:dim(dataset)[1]) {
  print(paste('Spectrum',i))
  testSegments<- segmentateSp(dataset[i,], peakParam)
  attachedSegs<-attachSegments(refSegments,testSegments)
  suppressMessages(attach(attachedSegs))
  Segs<-matchSegments(refSp,dataset[i,],testSegmentsNew,refSegmentsNew,MAX_DIST_FACTOR, MIN_RC)
  suppressMessages(attach(Segs))
  aligneddataset[i,]<- suppressWarnings(alignSp(refSp,refSegs,dataset[i,],testSegs,recursion,MAX_DIST_FACTOR,MIN_RC))
}
print('Done!')

return(aligneddataset)
}
