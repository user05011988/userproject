fittingloop_bg = function(FeaturesMatrix, Xdata, Ydata, other_fit_parameters) {
  
  residFun <-
    function(par, observed, xx,multiplicities,roof_effect,freq)
      observed - colSums(fitting_optimization(par, xx,multiplicities,roof_effect,observed,freq))

  # Loop to control if additional signals are incorporated, until a maximum of iterations specified bt fitting_maxiterrep.
  # If at the last fitting the improvement was lesser than 25% respective to the previous fitting,
  # iterrep becomes equal to fitting_maxiterrep and the loop is stooped
  lb = as.vector(t(FeaturesMatrix[, seq(1, 9, 2), drop = F]))
  ub = as.vector(t(FeaturesMatrix[, seq(2, 10, 2), drop = F]))
  multiplicities=FeaturesMatrix[,11]
  roof_effect=FeaturesMatrix[,12]

    #Several iterations of the algorith mare performed, to avoid the finding of local optimums that do not represent the best optimization of parameters
  s0 = lb + (ub - lb) * runif(length(ub))
  # tryCatch({

# print(ple)
    nls.out <-
      nls.lm(
        par = s0,
        fn = residFun,
        observed = Ydata,
        xx = Xdata,
        multiplicities=multiplicities,
        roof_effect=roof_effect,
        lower = lb,
        upper = ub,
        freq=other_fit_parameters$freq,
        control = nls.lm.control(
          factor = other_fit_parameters$factor,
          maxiter = other_fit_parameters$nls_lm_maxiter,
          ftol = other_fit_parameters$ftol,
          ptol = other_fit_parameters$ptol
        )

      )



      signals_parameters = coef(nls.out)
      dim(signals_parameters) = c(5, dim(FeaturesMatrix)[1])
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
      )

  return(signals_parameters)
}
