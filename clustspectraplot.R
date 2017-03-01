clustspectraplot = function(autorun_data) {
  
  if (nrow(autorun_data$dataset)>10) {
  scaled_roi=scale(autorun_data$dataset[ , sort(colMeans(autorun_data$dataset),decreasing=T,index.return=T)$ix[1:(ncol(autorun_data$dataset)/3)],drop=F])
  updated_scaled_roi=scaled_roi
  rm_ind=c()
  stop=0
ind=seq(nrow(autorun_data$dataset))
  while ((!is.null(rm_ind)|stop==0)&(nrow(scaled_roi)>15)) {
    stop=1
    if (length(rm_ind)>0) scaled_roi=scaled_roi[-rm_ind,]
    rm_ind=c()
    apres <- apclusterK(negDistMat(r=2), scaled_roi, K=min(c(dim(scaled_roi)[1]-1,10)),verbose=F)
    for (i in 1:length(apres@clusters)) {
      if (length(apres@clusters[[i]])==1) rm_ind=c(rm_ind,apres@clusters[[i]][1])
    }
    ind=apres@exemplars
  }
    updated_scaled_roi=scaled_roi[ind,,drop=F]
print(dim(updated_scaled_roi))
  spectra_lag=rep(NA,nrow(updated_scaled_roi))
  dummy=apply(updated_scaled_roi, 2, function(x)  median(x,na.rm=T))
  print(dim(dummy))
  
  for (i in 1:nrow(updated_scaled_roi)) {
    d <-ccf(updated_scaled_roi[i,],dummy,type = 'covariance',plot = FALSE)
    spectra_lag[i]=d$lag[which.max(d$acf)]
  }
  visual_roi=original_roi=autorun_data$dataset[ind[sort(spectra_lag,index.return=T)$ix],,drop=F]
  
  } else {
    visual_roi=original_roi=autorun_data$dataset
    ind=seq(nrow(autorun_data$dataset))
}
  for (i in 1:nrow(original_roi))   visual_roi[i,]=original_roi[i,]+(i-1)*mean(original_roi)
  
  
  plotdata = data.frame(Xdata=autorun_data$ppm, signals = t(visual_roi) )
  colnames(plotdata)=c('Xdata',rownames(autorun_data$dataset)[ ind])
  
  
  plotdata2=melt(plotdata,id = "Xdata")
  
  
  
  
  
  
  return(plotdata2)
}