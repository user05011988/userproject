fittingloop_bg = function(FeaturesMatrix, Xdata, Ydata, other_fit_parameters) {
  #Created by Daniel Cañueto 30/08/2016
  #Calculation through least squares algorithm of parameters that achieve the best fitting.
  
  # There are several optimization iterations and the one with less error is chosen.
  # The number of iter depends on the complexity of the ROI.
  
  # After the first fitting there can be further ones, depending on the need to add additional signals
  # to adapt the signals and ROI information provided by the user to the concrete
  # characteristics of the spectrum
  
  #TO DO: ideally, multiplicity and roof effect should not be incorporated into the optimization. Maybe this is ithe cause of the improper input parameters message. However, it would be necessary to load it from a txt file in every iteration, or with a global variable. I guess this procedures would make it much more slower. But it can be checked.
  #TO DO: another revision of algorithm alternatives
  
  
  
  #Function where to find a minimum
  residFun <-
    function(p, observed, xx)
      observed - fitting_optimization(p, xx)
  
  # Loop to control if additional signals are incorporated, until a maximum of iterations specified bt fitting_maxiterrep.
  # If at the last fitting the improvement was lesser than 25% respective to the previous fitting,
  # iterrep becomes equal to fitting_maxiterrep and the loop is stooped
    bounds=list(ub=matrix(0,dim(FeaturesMatrix)[1],(dim(FeaturesMatrix)[2]/2)-2),lb=matrix(0,dim(FeaturesMatrix)[1],(dim(FeaturesMatrix)[2]/2)-2))
    bounds$lb=t(FeaturesMatrix[,seq(1,9,2),drop=F])
    bounds$ub=t(FeaturesMatrix[,seq(2,10,2),drop=F])
    lb = rbind(bounds$lb, t(FeaturesMatrix[, 11:12, drop = F]))
    ub = rbind(bounds$ub, t(FeaturesMatrix[, 11:12, drop = F]))
    
    #Several iterations of the algorith mare performed, to avoid the finding of local optimums that do not represent the best optimization of parameters
    s0 = lb + (ub - lb) * matrix(runif(dim(lb)[1] * dim(lb)[2]), dim(lb)[1], dim(lb)[2])
      # tryCatch({
      
      nls.out <-
        nls.lm(
          par = as.vector(s0),
          fn = residFun,
          observed = Ydata,
          xx = Xdata,
          lower = as.vector(lb),
          upper = as.vector(ub),
          control = nls.lm.control(factor = other_fit_parameters$factor, maxiter=50, ftol=1e-5, ptol=1e-5)
        )
      
      
     
      signals_parameters = coef(nls.out)
      dim(signals_parameters) = c(7, dim(FeaturesMatrix)[1])
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling',
        'multiplicities',
        'roof_effect'
      )
      
  return(signals_parameters)
}
