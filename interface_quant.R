interface_quant = function(autorun_data, finaloutput,ind,ROI_profile,is_autorun) {
  blah=list()
  ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
  dummy = ROI_data[which(is.na(ROI_data[, 1])),]
  dummy2= list()
  for (i in 1:dim(ROI_profile)[1]) {
    dummy2[[length(dummy2)+1]]=which(dummy[,4] == ROI_profile[i,4])
  }
  
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
  for (spectrum_index in indexes) {
    print(spectrum_index)
  ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
    fitting_type = as.character(ROI_profile[1, 3])
    signals_to_quantify = which(ROI_profile[, 7] >0)
    signals_codes = replicate(length(signals_to_quantify), NA)
    signals_names = replicate(length(signals_to_quantify), NA)
    j = 1
    for (i in signals_to_quantify) {
      k = which(autorun_data$signals_names == paste(ROI_profile[i,
        4],ROI_profile[i,7],sep='_'))
      
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
      integration_parameters = data.frame(plot_path, is_roi_testing,
                                          clean_fit)
      fa = interface_integration(integration_parameters, Xdata,

                                    Ydata)
      
      results_to_save=fa$results_to_save
      p=fa$p
      blah$p2=fa$p2
      
      
      blah$integration_parameters=integration_parameters
      #Generation of output variables specific of every quantification

     

      #If the quantification is through fitting with or without baseline
    } else if (fitting_type == "Clean Fitting" || fitting_type ==
               "Baseline Fitting") {
      is_roi_testing = "N"
      clean_fit='N'

      # clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      #                    "N")
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
      multiplicities=FeaturesMatrix[,11]
      roof_effect=FeaturesMatrix[,12]

      signals_parameters_2=signals_parameters
      multiplicities_2=multiplicities
      roof_effect_2=roof_effect
      #Fitting of the signals
      dim(signals_parameters) = c(5, length(signals_parameters)/5)
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
      ) 
      dummy2= list()
      for (i in 1:dim(ROI_profile)[1]) {
        dummy = ROI_data[which(ROI_data[, 7] != ROI_profile[i, 7]),]
        dummy=dummy[which(duplicated(rbind(ROI_profile,dummy))[(dim(ROI_profile)[1]+1):(dim(ROI_profile)[1]+dim(dummy))]==F),]
        if (length(which(dummy[,4] == ROI_profile[i,4]))>0) {
          dummy2[[length(dummy2)+1]]=which(dummy[,4] == ROI_profile[i,4])
          for (j in 1:length(dummy2[[i]])) {
           
            
            cc= signals_parameters_2[(5*i-4):(5*i)]

            cc[5]=dummy[dummy2[[i]][j],][9]
            cc[1]=dummy[dummy2[[i]][j],][12]*cc[1]
            cc[2]=as.numeric(dummy[dummy2[[i]][j],][5])+(as.numeric(cc[2])-as.numeric(ROI_profile[i,5]))
            signals_parameters_2=c(signals_parameters_2,cc)
            multiplicities_2=c(multiplicities_2,dummy[dummy2[[i]][j],][8])
            roof_effect_2=c(roof_effect_2,dummy[dummy2[[i]][j],][10])
          }   
        }
      }
      
      Xdata_2=autorun_data$ppm
      signals_parameters_2=unlist(signals_parameters_2)
      multiplicities_2=unlist(multiplicities_2)
      roof_effect_2=unlist(roof_effect_2)

      fitted_signals = fitting_optimization(signals_parameters_2,
                                         Xdata_2,multiplicities_2,roof_effect_2,Ydata,other_fit_parameters$freq)
      # signals_parameters=as.matrix(signals_parameters)
      
      dim(signals_parameters_2) = c(5, length(signals_parameters_2)/5)
      rownames(signals_parameters_2) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
      ) 
      other_fit_parameters$signals_to_quantify=signals_to_quantify
      Ydata_2 = as.numeric(autorun_data$dataset[spectrum_index, ])

      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      output_data = output_generator(
        signals_to_quantify,
        fitted_signals,
        Ydata_2,
        Xdata_2,
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
      
      plot_data = rbind(
        output_data$signals_sum,
        output_data$baseline_sum,
        output_data$fitted_sum,
        output_data$signals
      )
      
      # rownames(plot_data) = c("signals_sum",
      #   "baseline_sum",
      #   "fitted_sum",
      #   as.character(ROI_profile[,4]))
      
      # plotdata = data.frame(Xdata=autorun_data$ppm[ROI_buckets], t(dataset[input$x1_select,ROI_buckets,drop=F]))
      
      plotdata2 = data.frame(Xdata=Xdata_2,
        Ydata=Ydata_2,
        plot_data[3, ],
        plot_data[2, ] )
      plotdata3 <- melt(plotdata2, id = "Xdata")
      plotdata3$variable = c(
        rep('Original Spectrum', length(Ydata_2)),
        rep('Generated Spectrum', length(Ydata_2)),
        rep('Generated Background', length(Ydata_2))
      )
      plotdata4 = data.frame(Xdata=Xdata_2, (t(plot_data[-c(1, 2, 3), , drop = F]) ))
      plotdata5 = melt(plotdata4, id = "Xdata")


      
      plotdata = data.frame(Xdata=Xdata_2, signals = plot_data[1, ] )
      p=plot_ly(plotdata,x = ~Xdata, y = ~signals, type = 'scatter', color= 'Signals',mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL)  %>%
        layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
          yaxis = list(range=c(0,max(Ydata)),title = 'Intensity'))
      
      plot_data=plot_data[,ROI_buckets]
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

      
      blah$p2=ggplot() +
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
        blah$p2=blah$p2 +
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
    signals_parameters_2=t(rbind(signals_parameters_2,multiplicities_2,roof_effect_2))
    
    blah$signals_parameters=signals_parameters
    blah$other_fit_parameters=other_fit_parameters
    blah$results_to_save=results_to_save
    blah$import_excel_profile=ROI_profile
    blah$Ydata=Ydata
    blah$fitted_signals=fitted_signals
    blah$plot_data=plot_data
    blah$FeaturesMatrix=FeaturesMatrix
    blah$signals_parameters=signals_parameters
    blah$signals_parameters_2=signals_parameters_2
    
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
      png(filename=paste(plot_path,"Fit2.png",sep='/'), 
        type="cairo",
        units="in", 
        width=8, 
        height=4, 
        pointsize=12, 
        res=96)
      print(blah$p2)
      dev.off()
    }
  }
    
    # blah$finaloutput=finaloutput

    # blah$autorun_data=autorun_data
  return(blah)
}
