signals_int = function(autorun_data, finaloutput,spectrum_index,signals_introduce,ROI_profile) {
  
  c=NULL
    #Preparation of necessary variables and folders to store figures and information of the fitting
  
  ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-autorun_data$ppm))
  
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    program_parameters=autorun_data$program_parameters
    program_parameters$freq = autorun_data$freq
    program_parameters$ROI_buckets = ROI_buckets
    program_parameters$buck_step = autorun_data$buck_step
    
      
      signals_to_quantify = which(ROI_profile[, 5] >0)
      signals_codes = replicate(length(signals_to_quantify), NA)
      signals_names = replicate(length(signals_to_quantify), NA)
      j = 1
      for (i in signals_to_quantify) {
        k = which(autorun_data$signals_names == paste(ROI_profile[i,
          4],ROI_profile[i,5],sep='_'))
        
        signals_codes[j] = autorun_data$signals_codes[k]
        signals_names[j] = as.character(autorun_data$signals_names[k])
        j = j + 1
      }
    
      # program_parameters$clean_fit = clean_fit
experiment_name = autorun_data$Experiments[[spectrum_index]]


fitting_type=ROI_profile[1,3]
      #Fitting of the signals
      multiplicities=signals_introduce[,6]
      roof_effect=signals_introduce[,7]
      signals_parameters=as.vector(t(signals_introduce[,1:5]))
      Xdata_2=autorun_data$ppm
      Ydata_2 = as.numeric(autorun_data$dataset[spectrum_index, ])
      
      program_parameters$freq=autorun_data$freq
      fitted_signals = fitting_optimization(signals_parameters,
                                         Xdata_2,multiplicities,roof_effect,Ydata,program_parameters$freq)
     
      dim(signals_parameters) = c(5, length(signals_parameters)/5)
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'half_band_width',
        'gaussian',
        'J_coupling'
         )     
      # signals_to_quantify=c(1,2)
      program_parameters$signals_to_quantify=signals_to_quantify

    # print(signals_parameters)
      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      dummy = output_generator(
        signals_to_quantify,
        fitted_signals,
        Ydata_2,
        Xdata_2,
        signals_parameters,multiplicities
      )
      output_data=dummy$output_data
      error1=dummy$error1
    # print(output_data)
      output_data$intensity=signals_parameters[1, signals_to_quantify]
      output_data$half_band_width=signals_parameters[3, signals_to_quantify]

      #Generation of the dataframe with the final output variables
      results_to_save = data.frame(
        shift = output_data$shift,
        Area = output_data$Area,
        signal_area_ratio = output_data$signal_area_ratio,
        fitting_error = output_data$fitting_error,
        intensity = output_data$intensity,
        half_band_width = output_data$half_band_width
      )
      
      #Adaptation of the quantification to de-scaled Ydata

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
        as.character(paste(ROI_profile[,4],ROI_profile[,5],sep='_')),rep('additional signal',dim(plot_data)[1]-length(ROI_profile[,4])-3))
      
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
      # plotdata4 = data.frame(Xdata=Xdata_2, (t(plot_data[-c(1, 2, 3), , drop = F]) ))
      # plotdata5 = melt(plotdata4, id = "Xdata")
      
      colors=c('red','blue','black','brown','cyan','green','yellow')
      # plotdata = data.frame(Xdata=Xdata_2, signals = plot_data[1, ] )
      p=plot_ly(plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL) %>% layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'), yaxis = list(range=c(0,max(Ydata)),title = 'Intensity'))
      for (i in 4:nrow(plot_data)) {
        plotdata5 =  data.frame(Xdata=Xdata_2, variable=rownames(plot_data)[i] ,value=plot_data[i,])
        
        p=p %>%add_trace(data=plotdata5,x=~Xdata,y=~value,name=~variable,type='scatter',mode='lines',fill='tozeroy',fillcolor=colors[i-3])   
      }
      
      
    # finaloutput = save_output(
    #   spectrum_index,
    #   signals_codes,
    #   results_to_save,
    #   autorun_data$buck_step,
    #   finaloutput)
    
    provisional_data=list()
    provisional_data$signals_parameters=signals_parameters
    provisional_data$program_parameters=program_parameters
    provisional_data$p=p
    # provisional_data$p2=p2
    provisional_data$Xdata=Xdata
    provisional_data$Ydata=Ydata
    # provisional_data$finaloutput=finaloutput
    provisional_data$results_to_save=results_to_save
    provisional_data$error1=error1
    # provisional_data$FeaturesMatrix=FeaturesMatrix
    # provisional_data$fitted_signals=fitted_signals[,ROI_buckets]
    
    provisional_data$spectrum_index=spectrum_index
    provisional_data$signals_codes=signals_codes
    provisional_data$signals_names=signals_names
    
    provisional_data$fitting_type=fitting_type
    provisional_data$ROI_profile=ROI_profile
    provisional_data$finaloutput=finaloutput
    provisional_data$plot_data=plot_data[,ROI_buckets]
    

  return(provisional_data)
}
