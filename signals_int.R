signals_int = function(autorun_data, finaloutput,spectrum_index,signals_introduce,ROI_profile) {
  
  
    #Preparation of necessary variables and folders to store figures and information of the fitting
  
  
  ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
      # is_roi_testing = "N"
      # clean_fit='N'
      # signals_names=autorun_data$signals_names[1:2]
      # signals_codes=autorun_data$signals_codes[1:2]
      # 
      signals_to_quantify = which(ROI_profile[, 7] == 1)
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
    
     print(signals_names)
      # other_fit_parameters$clean_fit = clean_fit
experiment_name = autorun_data$Experiments[[spectrum_index]]

plot_path = file.path(autorun_data$export_path,
  experiment_name,
  signals_names)
scaledYdata = as.vector(Ydata / (max(Ydata)))
fitting_type=ROI_profile[1,3]
      #Fitting of the signals
      multiplicities=signals_introduce[,6]
      roof_effect=signals_introduce[,7]
      signals_parameters=as.vector(t(signals_introduce[,1:5]))
      
      # print(signals_parameters)
      # print(Xdata)
      other_fit_parameters$freq=autorun_data$freq
      fitted_signals = fitting_optimization(signals_parameters,
                                         Xdata,multiplicities,roof_effect,Ydata,other_fit_parameters$freq)
      # print(fitted_signals)
      # signals_parameters=as.matrix(signals_parameters)
      dim(signals_parameters) = c(5, dim(signals_introduce)[1])
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
         )     
      # signals_to_quantify=c(1,2)
      other_fit_parameters$signals_to_quantify=signals_to_quantify

    # print(signals_parameters)
      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      output_data = output_generator(
        signals_to_quantify,
        fitted_signals,
        scaledYdata,
        Xdata,
        signals_parameters,multiplicities
      )
    # print(output_data)
      output_data$intensity=signals_parameters[1, signals_to_quantify] * max(Ydata)
      output_data$width=signals_parameters[3, signals_to_quantify]

      #Generation of the dataframe with the final output variables
      results_to_save = data.frame(
        shift = output_data$shift,
        Area = output_data$Area * max(Ydata),
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
      # print(plot_data)
      rownames(plot_data) = c("signals_sum",
                              "baseline_sum",
                              "fitted_sum",
                              as.character(ROI_profile[,4]))

      # r=1
      # plotdata = data.frame(Xdata=autorun_data$ppm[ROI_buckets], t(dataset[input$x1_rows_selected,ROI_buckets,drop=F]))
     
      plotdata2 = data.frame(Xdata,
        Ydata,
        plot_data[3, ] * max(Ydata),
        plot_data[2, ] * max(Ydata))
      plotdata3 <- melt(plotdata2, id = "Xdata")
      plotdata3$variable = c(
        rep('Original Spectrum', length(Ydata)),
        rep('Generated Spectrum', length(Ydata)),
        rep('Generated Background', length(Ydata))
      )
      plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) *
          max(Ydata)))
      plotdata5 = melt(plotdata4, id = "Xdata")
      
      p=ggplot() +
        geom_line(data = plotdata3,
          aes(
            x = Xdata,
            y = value,
            colour = variable,
            group = variable
          )) +
        geom_line(data = plotdata5,
          aes(
            x = Xdata,
            y = value,
            colour = 'Surrounding signals',
            group = variable
          )) +
        scale_x_reverse() + labs(x='ppm',y='Intensity')
      for (r in 1:length(other_fit_parameters$signals_to_quantify)) {
        plotdata = data.frame(Xdata, signals = plot_data[3 + other_fit_parameters$signals_to_quantify[r], ] * max(Ydata))
        p=p +
          geom_area(
            data = plotdata,
            aes(
              x = Xdata,
              y = signals,
              position = 'fill',
              fill = 'Quantified Signal'
            )
          )
      }
     
    finaloutput = save_output(
      spectrum_index,
      signals_codes,
      results_to_save,
      autorun_data$buck_step,
      finaloutput)
  print(plot_path)
    signals_parameters=t(rbind(signals_parameters[, signals_to_quantify],multiplicities[signals_to_quantify],roof_effect[signals_to_quantify]))
    blah=list()
    blah$signals_parameters=signals_parameters
    blah$other_fit_parameters=other_fit_parameters
    blah$plot_path=plot_path
    blah$p=p
    blah$Xdata=Xdata
    blah$Ydata=Ydata
    blah$finaloutput=finaloutput
    blah$results_to_save=results_to_save
    blah$fitting_type=fitting_type
    blah$ROI_profile=ROI_profile
    blah$finaloutput=finaloutput
    blah$signals_codes
    
  return(blah)
}
