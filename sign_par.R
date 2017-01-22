sign_par = function(autorun_data, finaloutput,spectrum_index,ROI_profile) {
  
  
    #Preparation of necessary variables and folders to store figures and information of the fitting

  ROI_buckets=which(round(autorun_data$ppm,6)==round(input$num1,6)):which(round(autorun_data$ppm,6)==round(input$num2,6))
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    program_parameters=autorun_data$program_parameters
    program_parameters$freq = autorun_data$freq
    program_parameters$ROI_buckets = ROI_buckets
    program_parameters$buck_step = autorun_data$buck_step
    
      is_roi_testing = "N"
      clean_fit='N'
      signals_names=autorun_data$signals_names[1:2]
      signals_codes=autorun_data$signals_codes[1:2]
      
      # clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      #                    "N")
    # print(ROI_profile)
      #Parameters of every signal necessary for the fitting
      initial_fit_parameters = ROI_profile[, 2:8,drop=F]
     input
      # initial_fit_parameters=as.data.frame(apply(initial_fit_parameters,2,as.numeric))

      # initial_fit_parameters = initial_fit_parameters[complete.cases(initial_fit_parameters),]
      colnames(initial_fit_parameters) = c(
        "positions",
        "widths",
        "quantification_or_not",
        "multiplicities",
        "Jcoupling",
        "roof_effect",
        "shift_tolerance"
      )

      #Ydata is scaled to improve the quality of the fitting
      scaledYdata = as.vector(Ydata / (max(Ydata)))

      #Other parameters necessary for the fitting independent of the type of signal

      program_parameters$clean_fit = clean_fit

      #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
      FeaturesMatrix = fitting_prep(Xdata,
                                    scaledYdata,
                                    initial_fit_parameters,
                                    program_parameters)

      signals_parameters=(FeaturesMatrix[1:dim(initial_fit_parameters)[1],seq(1,9,2)]+FeaturesMatrix[1:dim(initial_fit_parameters)[1],seq(2,10,2)])/2
      #Calculation of the parameters that will achieve the best fitting
      signals_parameters=cbind(signals_parameters,FeaturesMatrix[1:dim(initial_fit_parameters),11:12])
   # meh=list()
   #  meh$signals_parameters=signals_parameters
    
  return(signals_parameters)
}
