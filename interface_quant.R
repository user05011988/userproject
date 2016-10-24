interface_quant = function(autorun_data, finaloutput,ind,ROI_profile,is_autorun) {
  blah=list()
  
    #Preparation of necessary variables and folders to store figures and information of the fitting
  # if (is_autorun=='N') {indexes=input$x1_select
  if (is_autorun=='N') {
    # if (is.null(input$fit_selection_cell_clicked$row)) {
    #   indexes=input$x1_rows_selected
    # } else {
      indexes=ind
      # indexes=input$troco_cell_clicked$row
    # }
  } else {
    indexes=1:dim(autorun_data$dataset)[1]
  }
  # print(ROI_profile)
  for (spectrum_index in indexes) {
    print(spectrum_index)
  ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  # print(ROI_buckets)
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
    fitting_type = as.character(ROI_profile[1, 3])
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
    
    
    experiment_name = autorun_data$Experiments[[spectrum_index]]
    plot_path = file.path(autorun_data$export_path,
                          experiment_name,
                          signals_names)
    for (i in seq_along(plot_path))
      if (!dir.exists(plot_path[i]))
        dir.create(plot_path[i])
    # print(plot_path)
    # If the quantification is through integration with or without baseline
    if (fitting_type == "Clean Sum" ||
        fitting_type == "Baseline Sum") {
      is_roi_testing = "N"
      clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
      integration_parameters = data.frame(plot_path, is_roi_testing,
                                          clean_fit)
      fa = interface_integration(integration_parameters, Xdata,

                                    Ydata)
      
      results_to_save=fa$results_to_save
      p=fa$p
      
      blah$integration_parameters=integration_parameters
      #Generation of output variables specific of every quantification

     

      #If the quantification is through fitting with or without baseline
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
      other_fit_parameters$freq=autorun_data$freq
      
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

      output_data$intensity=signals_parameters[1, signals_to_quantify]
      output_data$width=signals_parameters[3, signals_to_quantify]


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

      #Generation of the figure when the conditions specified in the Parameters file are accomplished
      # r=1
      plot_data = rbind(
        output_data$signals_sum,
        output_data$baseline_sum,
        output_data$fitted_sum,
        output_data$signals
      )
      rownames(plot_data) = c("signals_sum",
                              "baseline_sum",
                              "fitted_sum",
                              as.character(ROI_profile[,4]))

      # plotdata = data.frame(Xdata=autorun_data$ppm[ROI_buckets], t(dataset[input$x1_select,ROI_buckets,drop=F]))
     
      plotdata2 = data.frame(Xdata,
        Ydata,
        plot_data[3, ],
        plot_data[2, ] )
      plotdata3 <- melt(plotdata2, id = "Xdata")
      plotdata3$variable = c(
        rep('Original Spectrum', length(Ydata)),
        rep('Generated Spectrum', length(Ydata)),
        rep('Generated Background', length(Ydata))
      )
      plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) ))
      plotdata5 = melt(plotdata4, id = "Xdata")
      # p=plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
      # p <- add_trace(p,data=plotdata5,x = ~Xdata,
      #   y = ~value,
      #   colour = 'Surrounding signals',
      #   group = ~variable)
      # p <- add_trace(p,data=plotdata5,x = ~Xdata,
      #   y = ~value,
      #   colour = 'Surrounding signals',
      #   group = ~variable)
      # plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"))
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
        scale_x_reverse() + labs(x='ppm',y='Intensity') + expand_limits(y=0)
      
     for (r in 1:length(other_fit_parameters$signals_to_quantify)) {
       plotdata = data.frame(Xdata, signals = plot_data[3 + other_fit_parameters$signals_to_quantify[r], ] )
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
  
   
    signals_parameters=t(rbind(signals_parameters,multiplicities,roof_effect))
    blah$signals_parameters=signals_parameters
    blah$other_fit_parameters=other_fit_parameters
    blah$results_to_save=results_to_save
    blah$import_excel_profile=ROI_profile
    blah$Ydata=Ydata
    blah$fitted_signals=fitted_signals
    blah$plot_data=plot_data
    blah$FeaturesMatrix=FeaturesMatrix
    blah$signals_parameters=signals_parameters
    blah$Xdata=Xdata
    }
    blah$p=p
    blah$plot_path=plot_path
    blah$results_to_save=results_to_save
    blah$spectrum_index=spectrum_index
    blah$signals_codes=signals_codes
    blah$fitting_type=fitting_type
    # blah$finaloutput=finaloutput
    
    if (is_autorun=='Y') {
      finaloutput=save_roi_testing(blah,autorun_data, finaloutput)
      blah$finaloutput=finaloutput
    }
  }
    
    # blah$finaloutput=finaloutput
    
    
    # blah$autorun_data=autorun_data
  return(blah)
}
