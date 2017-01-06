fitting_variables = function() {

  program_parameters=list() 
  
  #Parameters of preparation of signals' parameters to optimize
  program_parameters$BGdensity=50 #Density of signals to prepare abaseline below the signals to fit
program_parameters$widthtolerance=0.2 #Allowed Variability of halfwidth
program_parameters$gaussian=0 #Allowed Variability of gaussian percentage
program_parameters$j_coupling_variation=0.2 #Allowed Variability of j-coupling
program_parameters$BG_gaussian_percentage=0 #Allowed gaussian percentage of baseline signals
program_parameters$BG_width=12 #Halfwidth of baseline signals
program_parameters$BG_width_tolerance=0.25 #Allowed Variability of halfwidth of baseline signals

#Parameters related to the fitting loop
program_parameters$errorprov=3 #Percentage limit of fitting error on the ROI. If a lower percentage is reached, the solution is considered optimized and the optimization ends
program_parameters$fitting_maxiter=NA #The number of maximum iterations of optimization can be specified

#Parameters related to the nls algorithm. Read documentation of nls.lm package for details.
program_parameters$nls_lm_maxiter=200
program_parameters$ftol=1e-6
program_parameters$ptol=1e-6
program_parameters$factor=0.01

#Parameters related to the addition of other signals post fitting
program_parameters$additional_signal_ppm_distance=0.002 #Allowed distance for added peaks
program_parameters$signals_to_add = 2 #Allowed distance for added peaks to the ROI
program_parameters$fitting_maxiterrep = 2 #Allowed tries to add peaks
program_parameters$additional_signal_improvement=0.75 #Improvement of ROI by adding peaks. If previous addition did not achieve less than 75% of fitting error, process of addition of peaks is stopped
program_parameters$additional_signal_percentage_limit=3 #If fititng erorr is less tha nthis percentage, addition of peaks is not performed
program_parameters$peakdet_minimum=0.01 #Limit to find a peak as relevant enough to add it.


return(program_parameters)
}