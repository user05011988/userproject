autorun = function(autorun_data, finaloutput) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Autorun of quantification for all experiments using the information located at the ROI files.
  
  #TO DO: solving problem of intensity of signals of same metabolite through loading of all ROIs, calculation of maximum intensity for every signal in every spectrum and adaptation of maximum intensity to relative intensity (ROIs sholud have a new parameter)
  print('Be patient. Gonna take a while. You should be writing, meanwhile.')
  
  
  # Loading of ROIs parameters
  ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
  dummy = which(is.na(ROI_data[, 1]))
  if (length(dummy)==0) dummy=dim(ROI_data)[1]+1
  lal=which(duplicated(ROI_data[-dummy,1:2])==F)
  ROI_separator = cbind(lal, c(lal[-1] - 1, dim(ROI_data[-dummy,])[1]))
  
  for (ROI_index in seq_along(ROI_separator[, 1])) {
    
    
    #Loading of every ROI parameters
    import_excel_profile = ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
    print(paste(import_excel_profile[1,1], import_excel_profile[1,2], sep = '-'))
    
    ROI_buckets = which.min(abs(as.numeric(import_excel_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(import_excel_profile[1, 2])-autorun_data$ppm))
    if (ROI_buckets[1]>ROI_buckets[2]) ROI_buckets=rev(ROI_buckets)
    Xdata = autorun_data$ppm[ROI_buckets]
    
    #Preparation of necessary parameters
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
    
    
    
    # bf=apcluster(negDistMat(r=2),dd[be,])
    
    fitting_type = as.character(import_excel_profile[1, 3])
    signals_to_quantify = which(import_excel_profile[, 7] >= 1)
  
    signals_codes = replicate(length(signals_to_quantify), NA)
    signals_names = replicate(length(signals_to_quantify), NA)
    j = 1
    for (i in signals_to_quantify) {
      k = which(autorun_data$signals_names == paste(import_excel_profile[i,
        4],import_excel_profile[i,7],sep='_'))
      
      signals_codes[j] = autorun_data$signals_codes[k]
      signals_names[j] = as.character(autorun_data$signals_names[k])
      j = j + 1
    }
    
    #Quantification for every experiment
    
    for (spectrum_index in 1:dim(autorun_data$dataset)[1]) {
      
      print(paste("Spectrum ",spectrum_index))
      
      
      #Preparation of necessary variables and folders to store figures and information of the fitting
      Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
      
      experiment_name = autorun_data$Experiments[[spectrum_index]]
      plot_path = file.path(autorun_data$export_path,
        experiment_name,
        signals_names)
      for (i in seq_along(plot_path))
        if (!dir.exists(plot_path[i]))
          dir.create(plot_path[i])
      
      #If the quantification is through integration with or without baseline
      if (fitting_type == "Clean Sum" ||
          fitting_type == "Baseline Sum") {
        is_roi_testing = "N"
        clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
        integration_parameters = data.frame(plot_path, is_roi_testing,
          clean_fit)
        results_to_save = integration(integration_parameters, Xdata,
          
          Ydata)
        #Generation of output variables specific of every quantification
        
        write.csv(
          integration_parameters,
          file.path(plot_path,
            "integration_parameters.csv"),
          row.names = F
        )
        
        #If the quantification is through fitting with or without baseline
      } else if (fitting_type == "Clean Fitting" || fitting_type ==
          "Baseline Fitting") {
        is_roi_testing = "N"
        
        clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
          "N")
        
        #Parameters of every signal necessary for the fitting
        initial_fit_parameters = import_excel_profile[, 5:11,drop=F]
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
        # Ydata = as.vector(Ydata / (max(Ydata)))
        
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
        multiplicities=c(FeaturesMatrix[,11],rep(1,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        roof_effect=c(FeaturesMatrix[,12],rep(0,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        # signals_parameters[which(seq_along(signals_parameters)%%5==3)]=signals_parameters[which(seq_along(signals_parameters)%%5==3)]/1.5
        # signals_parameters[which(seq_along(signals_parameters)%%5==5)]=signals_parameters[which(seq_along(signals_parameters)%%5==5)]/2
        
        fitted_signals = fitting_optimization(signals_parameters,
          Xdata,multiplicities,roof_effect,Ydata,other_fit_parameters$freq)

        # signals_parameters[which(seq_along(signals_parameters)%%5==3)]=signals_parameters[which(seq_along(signals_parameters)%%5==3)]*1.5
        # signals_parameters[which(seq_along(signals_parameters)%%5==5)]=signals_parameters[which(seq_along(signals_parameters)%%5==5)]*2

        # signals_parameters=as.matrix(signals_parameters)
        dim(signals_parameters) = c(5, length(signals_parameters)/5)
        rownames(signals_parameters) = c(
          'intensity',
          'shift',
          'width',
          'gaussian',
          'J_coupling'
        )
        signals_parameters=rbind(signals_parameters,multiplicities,roof_effect)
        
        #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
        output_data = output_generator(
          signals_to_quantify,
          fitted_signals,
          Ydata,
          Xdata,
          signals_parameters,multiplicities
        )
       
        output_data$intensity=signals_parameters[1, signals_to_quantify]
        output_data$width=signals_parameters[3, signals_to_quantify]
        # output_data$intensity[! signals_to_quantify]=NA
        # output_data$width[! signals_to_quantify]=NA
        # output_data$Area[! signals_to_quantify]=NA
        # output_data$shift[! signals_to_quantify]=NA
        # output_data$fitting_error[! signals_to_quantify]=NA
        # output_data$signal_area_ratio[! signals_to_quantify]=NA
        # 
        #Generation of the dataframe with the final output variables
        results_to_save = data.frame(
          shift = output_data$shift,
          Area = output_data$Area,
          signal_area_ratio = output_data$signal_area_ratio,
          fitting_error = output_data$fitting_error,
          intensity = output_data$intensity,
          width = output_data$width
        )
        
        #Adaptation of the quantification to de-scaled Ydata
        # results_to_save$Area = results_to_save$Area * max(Ydata)
        
        #Generation of the figure when the conditions specified in the Parameters file are accomplished
        plot_data = rbind(
          output_data$signals_sum,
          output_data$baseline_sum,
          output_data$fitted_sum,
          output_data$signals
        )
        rownames(plot_data) = c("signals_sum",
          "baseline_sum",
          "fitted_sum",
          as.character(paste(import_excel_profile[,4],import_excel_profile[,7],sep='_')),rep('additional signal',dim(plot_data)[1]-length(import_excel_profile[,4])-3))
        
        other_fit_parameters$signals_to_quantify=signals_to_quantify
       
        plotgenerator(
          results_to_save,
          plot_data,
          Xdata,
          Ydata,
          fitted_signals,
          other_fit_parameters,
          signals_names,
          experiment_name,
          is_roi_testing,
          plot_path
        )
        
        #Generation of output variables specific of every quantification
        for (i in seq_along(plot_path)) {
          write.csv(
            import_excel_profile,
            file.path(plot_path[i],
              "import_excel_profile.csv")
            # row.names = F
          )
          write.csv(Ydata,file.path(plot_path[i], "Ydata.csv"),row.names = F,col.names = F)
          
          other_fit_parameters$signals_to_quantify=NULL
          
          write.csv(
            other_fit_parameters,
            file.path(plot_path[i],
              "other_fit_parameters.csv"),
            # row.names = F
          )
          write.table(fitted_signals,
            file.path(plot_path[i], "fitted_signals.csv"))
          # row.names = F,
          # col.names = F))
          write.table(plot_data,
            file.path(plot_path[i], "plot_data.csv"))
          # col.names = F)
          write.csv(FeaturesMatrix,
            file.path(plot_path[i], "FeaturesMatrix.csv"))
          # row.names = F)
          write.table(signals_parameters,
            file.path(plot_path[i],
              "signals_parameters.csv"))
          # col.names = F
          write.csv(Xdata,file.path(plot_path[i], "Xdata.csv"),row.names = F,col.names = F)

          # row.names = F,
          # col.names = F))
          write.csv(Ydata,file.path(plot_path[i], "Ydata.csv"),row.names = F,col.names = F)

          write.csv(results_to_save,
            file.path(plot_path[i], "results_to_save.csv"),
            row.names = F)
          
        }
        
      }
      
      #Generation of output variables specific of every ROI
      
      finaloutput = save_output(
        spectrum_index,
        signals_codes,
        results_to_save,
        autorun_data$buck_step,
        finaloutput
      )
      write.csv(finaloutput$Area,
        file.path(autorun_data$export_path,
          "Area.csv"))
      write.csv(finaloutput$shift,
        file.path(autorun_data$export_path,
          "shift.csv"))
      write.csv(finaloutput$width,
        file.path(autorun_data$export_path,
          "width.csv"))
      write.csv(
        finaloutput$signal_area_ratio,
        file.path(autorun_data$export_path,
          "signal_area_ratio.csv")
      )
      write.csv(
        finaloutput$fitting_error,
        file.path(autorun_data$export_path,
          "fitting_error.csv")
      )
      write.csv(
        finaloutput$intensity,
        file.path(autorun_data$export_path,
          "intensity.csv")
      )
      
      # row.names = F,
      # col.names = F))
      
      
    }
    
  }
  print("Done!")
  #Validation post-quantification system
  # alarmmatrix=validation(finaloutput, other_fit_parameters)
  # write.csv(alarmmatrix,
  #           file.path(autorun_data$export_path, "alarmmatrix.csv"),
  #           )
  return(finaloutput)
}