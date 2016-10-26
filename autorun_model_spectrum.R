autorun_model_spectrum = function(autorun_data) {

    #Preparation of necessary variables and folders to store figures and information of the fitting
  # if (is_autorun=='N') {indexes=input$x1_select
  
  # print(ROI_profile)
  ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
  dummy = which(!is.na(ROI_data[, 1]))
  ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(ROI_data)[1]))
  quartile_spectrum = as.numeric(apply(autorun_data$dataset, 2, function(x)
    quantile(x, 0.75,na.rm=T)))
  spectrum_index = which.min(apply(autorun_data$dataset, 1, function(x)
    sqrt(mean((x - quartile_spectrum) ^ 2
    ,na.rm=T))))
  plotdata = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata = as.numeric(autorun_data$dataset[spectrum_index,]))
  plotdata2 = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata=rep(0,length(autorun_data$ppm)))
  plotdata3 = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata=rep(0,length(autorun_data$ppm)))
    
  for (ROI_index in seq_along(ROI_separator[, 1])) {
    
    
    #Loading of every ROI parameters
    ROI_profile = ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
    ROI_limits = round(as.numeric(ROI_profile[1, 1:2]),3)
    if (ROI_limits[1] < ROI_limits[2])
      rev(ROI_limits)
    ROI_buckets = which(autorun_data$ppm <= ROI_limits[1] &
        autorun_data$ppm >=
        ROI_limits[2])
    preXdata = autorun_data$ppm[ROI_buckets]
    
    #Preparation of necessary parameters
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
    
    fitting_type = as.character(ROI_profile[1, 3])
    signals_to_quantify = which(ROI_profile[, 7] == 1)

      ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  # print(ROI_buckets)
  
    
    
    Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    
    
    signals_codes = replicate(length(signals_to_quantify), NA)
    signals_names = replicate(length(signals_to_quantify), NA)
    j = 1
    for (i in signals_to_quantify) {
      k = which(autorun_data$signals_names == ROI_profile[i,
        4])
      signals_codes[j] = autorun_data$signals_codes[k]
      signals_names[j] = as.character(autorun_data$signals_names[k])
      j = j + 1
    }
    

    experiment_name = autorun_data$Experiments[[spectrum_index]]
    plot_path = file.path(autorun_data$export_path,
                          experiment_name,
                          signals_names)
    for (i in seq_along(plot_path))
      if (!dir.exists(plot_path[i]))
        dir.create(plot_path[i])
    # If the quantification is through integration with or without baseline
    if (fitting_type == "Clean Sum" ||
        fitting_type == "Baseline Sum") {
      is_roi_testing = "N"
      clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
      baseline = replicate(length(Xdata), 0)
      if (integration_parameters$clean_fit == 'N')
        baseline = seq(mean(Ydata[1:3]), mean(Ydata[(length(Xdata) - 2):length(Xdata)]), length =
            length(Xdata))
      
      #integration ad chechk that there are no negative values
      integrated_signal = Ydata - baseline
      integrated_signal[integrated_signal<0]=0
      #preparation of output
      plotdata2$Ydata[ROI_buckets]= plotdata2$Ydata[ROI_buckets]+integrated_signal
      plotdata3$Ydata[ROI_buckets]= plotdata3$Ydata[ROI_buckets]+integrated_signal+baseline
      
      
    } else if (fitting_type == "Clean Fitting" || fitting_type ==
               "Baseline Fitting") {
      is_roi_testing = "N"
      clean_fit='N'

      # clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      #                    "N")
    # print(ROI_profile)
      #Parameters of every signal necessary for the fitting
      initial_fit_parameters = ROI_profile[, 5:11,drop=F]
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

      #Other parameters necessary for the fitting independent of the type of signal

      other_fit_parameters$clean_fit = clean_fit

      #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
      FeaturesMatrix = fitting_prep(Xdata,
                                    Ydata,
                                    initial_fit_parameters,
                                    other_fit_parameters)


      #Calculation of the parameters that will achieve the best fitting
      signals_parameters = fittingloop(FeaturesMatrix,
                                       Xdata,
                                       Ydata,
                                       other_fit_parameters)

      #Fitting of the signals
      multiplicities=FeaturesMatrix[,11]
      roof_effect=FeaturesMatrix[,12]
      fitted_signals = fitting_optimization(signals_parameters,
                                         Xdata,multiplicities,roof_effect,Ydata,other_fit_parameters$freq)
      # signals_parameters=as.matrix(signals_parameters)
      dim(signals_parameters) = c(5, dim(FeaturesMatrix)[1])
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
         )     
      other_fit_parameters$signals_to_quantify=signals_to_quantify


      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      output_data = output_generator(
        signals_to_quantify,
        fitted_signals,
        Ydata,
        Xdata,
        signals_parameters,multiplicities
      )

      plotdata2$Ydata[ROI_buckets]= plotdata2$Ydata[ROI_buckets]+output_data$signals_sum
      plotdata3$Ydata[ROI_buckets]= plotdata3$Ydata[ROI_buckets]+output_data$fitted_sum
}
   
    
    }

  p=plot_ly(plotdata2,x = ~Xdata, y = ~Ydata, type = 'scatter', name= 'Signals',mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~Ydata,name='Fitted spectrum',fill=NULL)  %>% add_trace(data=plotdata,x=~Xdata,y=~Ydata,name='Original spectrum',fill=NULL)  %>%
    layout(xaxis = list(range=c(max(autorun_data$ppm,na.rm=T),min(autorun_data$ppm,na.rm=T)),title = 'ppm'),
      yaxis = list(title = 'Intensity'))
    
    # blah$finaloutput=finaloutput
    
    
    # blah$autorun_data=autorun_data
  return(p)
}
