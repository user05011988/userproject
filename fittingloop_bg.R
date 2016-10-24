fittingloop_bg = function(FeaturesMatrix, Xdata, Ydata, other_fit_parameters) {
  #Created by Daniel Ca?ueto 30/08/2016
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
