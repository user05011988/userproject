medianplot = function(autorun_data) {
  types=unique(autorun_data$Metadata[,2])
  mediandataset=matrix(NA,length(types),ncol(autorun_data$dataset))
  for (i in 1:length(types)) mediandataset[i,]=apply(autorun_data$dataset[which(autorun_data$Metadata[,2]==types[i]),,drop=F],2,median) 
  
  plotdata = data.frame(Xdata=autorun_data$ppm, signals = t(mediandataset ))
  colnames(plotdata)=c('Xdata',types)
  
  
  plotdata2=melt(plotdata,id = "Xdata")
  
  
  # no=apply(autorun_data$dataset,2,median)
  # asd=peakdet(no,max(1e-10,quantile(no,0.6)))
  # hoh=c()
  # for (i in 1:length(asd$maxtab$pos)) {
  #   ff=c(which(asd$mintab$pos<asd$maxtab$pos[i])[length(which(asd$mintab$pos<asd$maxtab$pos[i]))],which(asd$mintab$pos>asd$maxtab$pos[i])[1])
  #   
  #   if (max(asd$mintab$val[ff],na.rm=T)<0.4*asd$maxtab$val[i]&asd$maxtab$val[i]>0) hoh=c(hoh,asd$maxtab$pos[i])
  # }
  # np=apply(nm,2,max)
  # # initiate a line shape object
  # line <- list(
  #   type = "line",
  #   line = list(color = "red"),
  #   xref = "x",
  #   yref = "y"
  # )
  # 
  # lines <- list()
  # for (i in 1:length(hoh)) {
  #   line[c("x0", "x1")] <-autorun_data$ppm[hoh[i]]
  #   line[["y0"]] <- 1.1*np[hoh[i]]
  #   line[["y1"]] <-  1.2*np[hoh[i]]
  #   lines <- c(lines, list(line))
  # }
  
 
  
  return(plotdata2)
}