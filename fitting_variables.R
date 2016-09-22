fitting_variables = function() {

  other_fit_parameters=list() 
  
  #Parameters of automatic ROI edition
  other_fit_parameters$automatic_roi_edition='N'
  other_fit_parameters$automatic_shift_distance=0.01
  
  #Parameters of preparation of signals' parameters to optimize
  other_fit_parameters$BGdensity=50
other_fit_parameters$widthtolerance=0.2
other_fit_parameters$gaussian=0
other_fit_parameters$j_coupling_variation=0.0003
other_fit_parameters$BG_gaussian_percentage=1
other_fit_parameters$BG_width=0.02
other_fit_parameters$BG_width_tolerance=0.25

#Parameters related to the fitting loop
other_fit_parameters$errorprov=5
other_fit_parameters$fitting_maxiter=NA

#Parameters related to the lsq algorithm
other_fit_parameters$nls_lm_maxiter=500
other_fit_parameters$ftol=1e-5
other_fit_parameters$ptol=1e-5
other_fit_parameters$factor=0.01

#Parameters related to the addition of other signals post fitting
other_fit_parameters$additional_signal_ppm_distance=0.002
other_fit_parameters$signals_to_add = 2
other_fit_parameters$fitting_maxiterrep = 1
other_fit_parameters$additional_signal_improvement=0.75
other_fit_parameters$additional_signal_percentage_limit=5
other_fit_parameters$peakdet_minimum=0.01

#Parameters related to the output of plots
other_fit_parameters$fitting_error_plot=0.01
other_fit_parameters$signal_area_ratio_plot=0.01

#Parameters related to criteria to detect dangerous quantifications
other_fit_parameters$rlm_limit=3
other_fit_parameters$fitting_error_limit=5
other_fit_parameters$signal_area_ratio_limit=10


return(other_fit_parameters)
}