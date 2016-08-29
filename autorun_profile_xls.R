autorun_profile_xls = function(autorun_data, finaloutput) {
  
  # if(!dir.exists(file.path(autorun_data$plots_path, 'MANRUN')))
  # dir.create(file.path(autorun_data$plots_path, 'MANRUN'))
  # if(!dir.exists(file.path(autorun_data$plots_path, 'AUTORUN')))
  # dir.create(file.path(autorun_data$plots_path, 'AUTORUN'))
  # profiles_paths=list.files(autorun_data$profile_folder_path,full.names=T,pattern='.xls')
  profiles_paths = list.files(autorun_data$profile_folder_path, full.names = T, 
                              pattern = ".csv")
  
  # wp = waitbar(0,'Checking Profiles...')
  
  for (ROI_index in 1:length(profiles_paths)) {
    # tic disp(ROI_index) Inicialitzem les variables axe_1D=0
    import_excel_profile = read.delim(profiles_paths[ROI_index], sep = ";", 
                                      header = T, stringsAsFactors = F)
    import_excel_profile = import_excel_profile[complete.cases(import_excel_profile), 
                                                ]
    ROI_limits = as.numeric(strsplit(as.character(import_excel_profile[1, 
                                                                       1]), ",")[[1]])
    fitting_type = as.character(import_excel_profile[1, 10])
    signals_to_quantify = which(import_excel_profile[, 5] == 1)
    signals_codes = replicate(length(signals_to_quantify), NA)
    signals_names = replicate(length(signals_to_quantify), NA)
    j = 1
    for (i in signals_to_quantify) {
      k = which(autorun_data$signals_names == as.character(import_excel_profile[i, 
                                                                                2]))
      signals_codes[j] = autorun_data$signals_codes[k]
      signals_names[j] = as.character(autorun_data$signals_names[k])
      j = j + 1
    }
    
    # waitbar(ROI_index/size(profiles_paths,1),wp) we = waitbar(0,'Checking
    # Experiments...')
    for (spectrum_index in 1:dim(autorun_data$dataset)[1]) {
      # disp(spectrum_index)
      print(spectrum_index)
      # E=1 RegionY=0 try
      # waitbar(spectrum_index/size(autorun_data$dataset,1),we) catch #si en
      # algun moment es tanca la finestra perquè és vol interrompre el procés
      # disp('Error: Quantification interrupted.') return end
      ROI_buckets = which(autorun_data$ppm < ROI_limits[1] & autorun_data$ppm > 
                            ROI_limits[2])
      Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
      Xdata = autorun_data$ppm[ROI_buckets]
      experiment_name = autorun_data$Experiments[[spectrum_index]]
      plot_path = file.path(autorun_data$export_path, experiment_name, 
                            signals_names)
      for (i in seq_along(plot_path)) if (!dir.exists(plot_path[i])) 
        dir.create(plot_path[i])
      if (fitting_type == "Clean Sum" || fitting_type == "Baseline Sum") {
        is_roi_testing = "N"
        clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
        integration_parameters = data.frame(plot_path, is_roi_testing, 
                                            clean_fit)
        
        
        results_to_save = integration(integration_parameters, Xdata, 
                                      Ydata)
        write.csv(integration_parameters, file.path(plot_path[i], 
                                                    "integration_parameters.csv"), row.names = F)
        
        
      } else if (fitting_type == "Clean Fitting" || fitting_type == 
                 "Baseline Fitting") {
        is_roi_testing = "N"  # no estem al ROI_index testing
        
        clean_fit = ifelse(fitting_type == "Clean Fitting", "Y", 
                           "N")  #no hi ha baseline
        
        initial_fit_parameters = import_excel_profile[, 3:9]
        initial_fit_parameters = initial_fit_parameters[complete.cases(initial_fit_parameters), 
                                                        ]
        colnames(initial_fit_parameters) = c("positions", "widths", 
                                             "quantification_or_not", "multiplicities", "Jcoupling", 
                                             "roof_effect", "shift_tolerance")
        scaledYdata = as.vector(Ydata/(max(Ydata)))
        other_fit_parameters = data.frame(freq = autorun_data$freq, 
                                          clean_fit)
        FeaturesMatrix = fitting_prep(Xdata, scaledYdata, initial_fit_parameters, 
                                      other_fit_parameters)
        
        
        signals_parameters = fittingloop(FeaturesMatrix, Xdata, 
                                         scaledYdata)
        
        # if (exists('output$features')==F) {
        fitted_signals = definitivefitting(signals_parameters, 
                                           Xdata)  # ep! canvi initial_roof_effect!
        
        
        # results_to_save=outputgenerator(fitting_parameters$iPosQ,fitting_parameters$multiplicities,fitting_parameters$roof_effect,fitted_signals,scaledYdata,Xdata,signals_parameters)
        output_data = output_generator(signals_to_quantify, fitted_signals, 
                                       scaledYdata, Xdata, signals_parameters)
        results_to_save = data.frame(shift = output_data$shift, 
                                     Area = output_data$Area, signal_area_ratio = output_data$signal_area_ratio, 
                                     fitting_error = output_data$fitting_error)
        results_to_save$Area = results_to_save$Area * max(Ydata)
        plot_data = rbind(output_data$signals_sum, output_data$baseline_sum, 
                          output_data$fitted_sum, output_data$quantified_signals)
        rownames(plot_data) = c("signals_sum", "baseline_sum", 
                                "fitted_sum", signals_names)
        
        plotgenerator(results_to_save, plot_data, Xdata, Ydata, 
                      fitted_signals, autorun_data$E_max, autorun_data$P_max, 
                      signals_names, experiment_name, is_roi_testing, plot_path)
        # } else { results_to_save=ROI_index }
        for (i in seq_along(plot_path)) {
          
          write.csv(initial_fit_parameters, file.path(plot_path[i], 
                                                      "initial_fit_parameters.csv"), row.names = F)
          write.table(scaledYdata, file.path(plot_path[i], "scaledYdata.csv"), 
                      row.names = F, col.names = F)
          write.csv(other_fit_parameters, file.path(plot_path[i], 
                                                    "other_fit_parameters.csv"), row.names = F)
          write.table(fitted_signals, file.path(plot_path[i], "fitted_signals.csv"), 
                      row.names = F, col.names = F)
          write.table(plot_data, file.path(plot_path[i], "plot_data.csv"), 
                      col.names = F)
          write.csv(FeaturesMatrix, file.path(plot_path[i], "FeaturesMatrix.csv"), 
                    row.names = F)
          write.table(signals_parameters, file.path(plot_path[i], 
                                                    "signals_parameters.csv"), col.names = F)
        }
        
      }
      
      finaloutput = save_output(spectrum_index, signals_codes, results_to_save, 
                                autorun_data$buck_step, finaloutput)
      write.csv(finaloutput$Area, file.path(autorun_data$export_path, 
                                            "Area.csv"))
      write.csv(finaloutput$shift, file.path(autorun_data$export_path, 
                                             "shift.csv"))
      write.csv(finaloutput$signal_area_ratio, file.path(autorun_data$export_path, 
                                                         "signal_area_ratio.csv"))
      write.csv(finaloutput$fitting_error, file.path(autorun_data$export_path, 
                                                     "fitting_error.csv"))
      write.table(Xdata, file.path(plot_path[i], "Xdata.csv"), row.names = F, 
                  col.names = F)
      write.table(Ydata, file.path(plot_path[i], "Ydata.csv"), row.names = F, 
                  col.names = F)
      write.csv(results_to_save, file.path(plot_path[i], "results_to_save.csv"), 
                row.names = F)
    }
    
    
    # close(wp) set(comp.auto_run_profile,'Value',0)
  }
  return(finaloutput)
}
