fittingloop=function(FeaturesMatrix,Xdata,Ydata) {

  # fitting_data=rbind(Xdata, 
  #                    c(dim(FeaturesMatrix)[1],replicate(length(Xdata)-1,0)), 
  #                    c(FeaturesMatrix[,11],replicate(length(Xdata)-dim(FeaturesMatrix)[1],0)), 
  #                    c(FeaturesMatrix[,12],replicate(length(Xdata)-dim(FeaturesMatrix)[1],0)), 
  #                    Ydata)
tries=0
try_error=10000
error=10000
if (dim(FeaturesMatrix)[1]>8) {
  maxtries=10
} else if ((dim(FeaturesMatrix)[1]>5 && dim(FeaturesMatrix)[1]<9) | any(FeaturesMatrix[,4]-FeaturesMatrix[,3]>0.01)) {
  maxtries=6
} else {
  maxtries=3
}

residFun <- function(p, observed, xx) observed - fitting_optimization(p,xx)


while (try_error<=error & tries<maxtries) {#aquí comencem les iteracions, com a màxim 10, i menys en cas d"assolir un error menor al ErrorMaxim
  control = getOptions
  bounds = getLUB(FeaturesMatrix[,1:10]) 
  lb=rbind(bounds$lb,t(FeaturesMatrix[,11:12]))
  ub=rbind(bounds$ub,t(FeaturesMatrix[,11:12]))
  #funció explicada detalladament just damunt d"aquesta
  s0 = lb + (ub-lb)*matrix(runif(dim(lb)[1]*dim(lb)[2]),dim(lb)[1],dim(lb)[2])
# tryCatch({
  nls.out <- nls.lm(par=as.vector(s0), fn = residFun, observed = Ydata,
                    xx = Xdata, lower=as.vector(lb), upper=as.vector(ub),control=control)
  tries=tries+1
  
  # if (EXITFLAG!=0 & EXITFLAG!=3 & EXITFLAG!=2 & EXITFLAG!=1) { #sortida programada en el cas que hagi hagut algun error en lsqcurvefit
  #   output$errors1=NaN
  #   output$errors2=NaN
  #   output$Area=NaN
  #   output$features=replicate(13,NaN)
  #   return(output)
  # }
  
  try_error=(sqrt(nls.out$deviance/length(Ydata)))*100/(max(Ydata)-min(Ydata));
  if (try_error<error) {
    error=try_error
    signals_parameters=coef(nls.out)
  }
    # cat('\n', try_error)
    if (dim(FeaturesMatrix)[1]>8) try_error=0
#   }, error = function(err) {
#     # if (triessignals==0) {
#     #   output$errors1=NaN
#     #   output$errors2=NaN
#     #   output$Area=NaN
#     #   output$features=replicate(13,NaN)
#     #   return(output)
#     # } else {
#     #   return(x)
#     #   break
# # }
# })
}
dim(signals_parameters)=c(7,dim(FeaturesMatrix)[1])
rownames(signals_parameters)=c('intensity','shift','width','gaussian','J_coupling','multiplicities','roof_effect')
  # Xdata=fitting_data[1,]
  # plot(Xdata,Ydata, type="o", main="fitting_data")
  # F=fitting_optimization(x, Xdata)
  # lines(Xdata,F, col=3, lwd=2)
return(signals_parameters)
}


