fhs = function(realfile) {


minppm=-2
maxppm=13

inDir=dirname(realfile)
# acquPar$NUC1, 
# acquPar$SFO1, procPar$BYTORDP[1],	procPar$NC_proc[1], 
# procPar$OFFSET, procPar$SF, procPar$SI,	procPar$SW_p, 
# procPar$XDIM
storedpars=parseProcs(inDir)
# storedpars2=parseAcqus(inDir)

storedpars$OFFSET=as.numeric(storedpars$OFFSET)
storedpars$SI=as.numeric(storedpars$SI)
# storedpars$SW=as.numeric(storedpars$SW)
storedpars$XDIM=as.numeric(storedpars$XDIM)
storedpars$NC_proc=as.numeric(storedpars$NC_proc)
storedpars$SW=as.numeric(storedpars$SW)

# # 
# minppmindex = floor((storedpars$OFFSET-minppm)/storedpars$SW*(storedpars$SI-1))
# maxppmindex = ceiling((storedpars$OFFSET-maxppm)/storedpars$SW*(storedpars$SI-1))
# Read first line of title-file
readCon <- file(realfile, 'rb')


data <- matrix(nrow=storedpars$SI[2], ncol=storedpars$SI[1])
tpc <- storedpars$SI[2] / storedpars$XDIM[2]
tpr <- storedpars$SI[1] / storedpars$XDIM[1]
for (i in 1:tpc){
  for (j in 1:tpr){
    rowNum <- (i - 1) * storedpars$XDIM[2] + 1
    colNum <- (j - 1) * storedpars$XDIM[1] + 1
    tileData <- matrix(as.numeric(readBin(readCon,	size=4,	what='integer',
      n=storedpars$XDIM[1] * storedpars$XDIM[2], endian=storedpars$BYTORDP)), nrow=storedpars$XDIM[2], 
      ncol=storedpars$XDIM[1],	byrow=TRUE)
    data[rowNum:(rowNum + storedpars$XDIM[2] - 1), colNum:(colNum + storedpars$XDIM[1] - 1)] <- 
      tileData
  }
}
close(readCon)
data <- data / (2^-storedpars$NC_proc)

interp2d <- function(old, newx, newy) {
  interp.surface.grid(list(x=seq(nrow(old)),y=seq(ncol(old)),z=old),
    list(x=seq(1,nrow(old),length=newx),
      y=seq(1,ncol(old),length=newy)))$z
}

newmat <- interp2d(data, newx=storedpars$SI[2]/8, newy=storedpars$SI[1]/8)
rownames(newmat)=seq(storedpars$OFFSET[2],storedpars$OFFSET[2]-storedpars$SW[2],length.out=storedpars$SI[2]/8)
colnames(newmat)=seq(storedpars$OFFSET[1],storedpars$OFFSET[1]-storedpars$SW[1],length.out=storedpars$SI[1]/8)

rownames(newmat)=paste('j =', rownames(newmat))
colnames(newmat)=paste('ppm =', colnames(newmat))
mtrx.melt <- melt(newmat, id.vars = c('j', 'ppm'), measure.vars = 'qsec')
names(mtrx.melt) <- c('j', 'ppm', 'value')
# Return data to numeric form
mtrx.melt$j <- as.numeric(str_sub(mtrx.melt$j, str_locate(mtrx.melt$j, '=')[1,1] + 1))
mtrx.melt$ppm <- as.numeric(str_sub(mtrx.melt$ppm, str_locate(mtrx.melt$ppm, '=')[1,1] + 1))

p=plot_ly(mtrx.melt,x = ~ ppm,y = ~ j,z = ~value,type = "contour",autocontour=F,contours=list(coloring='lines',end=quantile(newmat,0.99,na.rm=T),start=quantile(newmat,0.9,na.rm=T),size=quantile(newmat,0.99,na.rm=T)/5),showscale=F)%>% layout(xaxis = list(autorange = "reversed"))
return(p)
}


