clustspectraplot = function(imported_data) {
  yu=scale(imported_data$dataset[ , sort(colMeans(imported_data$dataset),decreasing=T,index.return=T)$ix[1:(dim(imported_data$dataset)[2]/3)]])
  co=c()
  stop=0
  while ((!is.null(co)|stop==0)&(dim(yu)[1]>20)) {
    stop=1
    if (length(co)>0) yu=yu[-co,]
    co=c()
apres <- apclusterK(negDistMat(r=2), yu, K=min(c(dim(yu)[1]-1,10)),verbose=F)
for (i in 1:length(apres@clusters)) {
  if (length(apres@clusters[[i]])==1) co=c(co,apres@clusters[[i]][1])
}
print(co)
}
  nn=yu[ apres@exemplars,]
  
spectra_lag=rep(NA,dim(nn)[1])
dummy=apply(nn, 2, function(x)  median(x,na.rm=T))
for (i in 1:dim(nn)[1]) {
  d <-
    ccf(nn[i, ],dummy
      ,
      type = 'covariance',
      plot = FALSE)
  spectra_lag[i]=d$lag[which.max(d$acf)]
  
}

nm=nn=imported_data$dataset[apres@exemplars[sort(spectra_lag,index.return=T)$ix],]
for (i in 1:dim(nn)[1])   nm[i,]=nn[i,]+(i-1)*mean(nn)


plotdata = data.frame(Xdata=imported_data$ppm, signals = t(nm) )
colnames(plotdata)=c('Xdata',rownames(imported_data$dataset)[ apres@exemplars])


plotdata2=melt(plotdata,id = "Xdata")


no=apply(imported_data$dataset,2,median)
asd=peakdet(no,max(0,quantile(no,0.6)))
hoh=c()
for (i in 1:length(asd$maxtab$pos)) {
  ff=c(which(asd$mintab$pos<asd$maxtab$pos[i])[length(which(asd$mintab$pos<asd$maxtab$pos[i]))],which(asd$mintab$pos>asd$maxtab$pos[i])[1])
 
 if (max(asd$mintab$val[ff],na.rm=T)<0.4*asd$maxtab$val[i]&asd$maxtab$val[i]>0) hoh=c(hoh,asd$maxtab$pos[i])
}
np=apply(nm,2,max)
# initiate a line shape object
line <- list(
  type = "line",
  line = list(color = "red"),
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in 1:length(hoh)) {
  line[c("x0", "x1")] <-imported_data$ppm[hoh[i]]
  line[["y0"]] <- 1.1*np[hoh[i]]
  line[["y1"]] <-  1.2*np[hoh[i]]
  lines <- c(lines, list(line))
}

p=plot_ly(data=plotdata2,x=~       Xdata
  ,y=~ value,color=~variable,type='scatter',mode='lines',colors = "Blues")%>% layout(xaxis = list(autorange = "reversed"),shapes = lines,yaxis = list(range = c(0, max(np))))

return(p)
}
