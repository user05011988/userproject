interface_quant = function(autorun_data, finaloutput,ind,ROI_profile,is_autorun,useful_data) {
  blah=list()
  # ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
  ROI_data=autorun_data$ROI_data
  
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
  } else {
    indexes=1:dim(autorun_data$dataset)[1]
  }
  for (spectrum_index in indexes) {
    print(paste("Spectrum ",spectrum_index))
    ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-autorun_data$ppm))
  
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    program_parameters=autorun_data$program_parameters
    program_parameters$freq = autorun_data$freq
    program_parameters$ROI_buckets = ROI_buckets
    program_parameters$buck_step = autorun_data$buck_step
    
    fitting_type = as.character(ROI_profile[1, 3])
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
    
    experiment_name = autorun_data$Experiments[[spectrum_index]]
   
    # If the quantification is through integration with or without baseline
    if (fitting_type == "Clean Sum" ||
        fitting_type == "Baseline Sum") {
      is_roi_testing = "N"
      clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
      integration_parameters = data.frame(is_roi_testing,
                                          clean_fit)
      dummy = interface_integration(integration_parameters, Xdata,

                                    Ydata)
      
      results_to_save=dummy$results_to_save
      p=dummy$p
      plot_data=dummy$plot_data
      
      blah$integration_parameters=integration_parameters
      #Generation of output variables specific of every quantification
      if (is_autorun=='Y') {
        useful_data[[spectrum_index]][[signals_codes]]$ROI_profile=ROI_profile
        useful_data[[spectrum_index]][[signals_codes]]$integration_parameters=integration_parameters
        useful_data[[spectrum_index]][[signals_codes]]$plot_data=dummy$plot_data
        useful_data[[spectrum_index]][[signals_codes]]$Xdata=Xdata
        useful_data[[spectrum_index]][[signals_codes]]$Ydata=Ydata
        useful_data[[spectrum_index]][[signals_codes]]$results_to_save=dummy$results_to_save
        
        finaloutput = save_output(
          spectrum_index,
          signals_codes,
          results_to_save,
          autorun_data$buck_step,
          finaloutput
        )
        tryCatch({write_info(autorun_data$export_path, finaloutput)}, error = function(err) {
          print('Not possible to overwrite a csv file open with Microsoft Excel')
        })
      }

      #If the quantification is through fitting with or without baseline
    } else if (fitting_type == "Clean Fitting" || fitting_type ==
               "Baseline Fitting") {
      program_parameters$clean_fit = 'N'
      program_parameters$freq=autorun_data$freq
      
      FeaturesMatrix = fitting_prep(Xdata,
                                    Ydata,
                                    ROI_profile[, 5:11,drop=F],
                                    program_parameters)
      #Calculation of the parameters that will achieve the best fitting
      signals_parameters = fittingloop(FeaturesMatrix,
                                       Xdata,
                                       Ydata,
                                       program_parameters)
      # multiplicities=FeaturesMatrix[,11]
      # roof_effect=FeaturesMatrix[,12]
      multiplicities=c(FeaturesMatrix[,11],rep(1,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
      roof_effect=c(FeaturesMatrix[,12],rep(0,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))

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
      # dummy2= list()
      # for (i in 1:dim(ROI_profile)[1]) {
      #   dummy = ROI_data[which(ROI_data[, 5] != ROI_profile[i, 5]),]
      #   dummy=dummy[which(duplicated(rbind(ROI_profile,dummy))[(dim(ROI_profile)[1]+1):(dim(ROI_profile)[1]+dim(dummy)[1])]==F),]
      #   if (length(which(dummy[,4] == ROI_profile[i,4]))>0) {
      #     print("a")
      #     dummy2[[length(dummy2)+1]]=which(dummy[,4] == ROI_profile[i,4])
      #     for (j in 1:length(dummy2[[i]])) {
      #      
      #       print("b")
      #       
      #       cc= signals_parameters_2[(5*i-4):(5*i)]
      # 
      #       cc[5]=dummy[dummy2[[i]][j],][9]
      #       cc[1]=dummy[dummy2[[i]][j],][12]*cc[1]
      #       cc[2]=as.numeric(dummy[dummy2[[i]][j],][5])+(as.numeric(cc[2])-as.numeric(ROI_profile[i,6]))
      #       signals_parameters_2=c(signals_parameters_2,cc)
      #       print("c")
      #       
      #       multiplicities_2=c(multiplicities_2,dummy[dummy2[[i]][j],][8])
      #       roof_effect_2=c(roof_effect_2,dummy[dummy2[[i]][j],][10])
      #     }   
      #   }
      # }
      # 
      Xdata_2=autorun_data$ppm
      # signals_parameters_2=unlist(signals_parameters_2)
      # multiplicities_2=unlist(multiplicities_2)
      # roof_effect_2=unlist(roof_effect_2)
      signals_parameters_2=unlist(signals_parameters)
      multiplicities_2=unlist(multiplicities)
      roof_effect_2=unlist(roof_effect)

      fitted_signals = fitting_optimization(signals_parameters_2,
                                         Xdata_2,multiplicities_2,roof_effect_2,Ydata,program_parameters$freq)
      # signals_parameters=as.matrix(signals_parameters)
      
      dim(signals_parameters_2) = c(5, length(signals_parameters_2)/5)
      rownames(signals_parameters_2) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
      ) 
      program_parameters$signals_to_quantify=signals_to_quantify
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
        correlation = output_data$correlation,
        intensity = output_data$intensity,
        width = output_data$width
      )
      
      plot_data = rbind(
        output_data$signals_sum,
        output_data$baseline_sum,
        output_data$fitted_sum,
        output_data$signals
      )
      
      # p2=plotgenerator(
      #   results_to_save,
      #   plot_data[,ROI_buckets],
      #   Xdata,
      #   Ydata,
      #   fitted_signals[,ROI_buckets],
      #   program_parameters,
      #   signals_names,
      #   experiment_name,
      #   is_roi_testing
      # )
      
      
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
      
   

    signals_parameters=t(rbind(signals_parameters,multiplicities,roof_effect))
    signals_parameters_2=t(rbind(signals_parameters_2,multiplicities_2,roof_effect_2))
    
    blah$signals_parameters=signals_parameters
    blah$program_parameters=program_parameters
    blah$results_to_save=results_to_save
    blah$ROI_profile=ROI_profile
    blah$Ydata=Ydata
    blah$fitted_signals=fitted_signals[,ROI_buckets]
    blah$plot_data=plot_data[,ROI_buckets]
    blah$FeaturesMatrix=FeaturesMatrix
    blah$signals_parameters=signals_parameters
    blah$signals_parameters_2=signals_parameters_2
    blah$Xdata=Xdata
    if (is_autorun=='Y') {
      for (i in seq_along(blah$signals_codes)) {
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$ROI_profile=ROI_profile
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$program_parameters=program_parameters
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$fitted_signals=fitted_signals[,ROI_buckets]
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$plot_data=plot_data[,ROI_buckets]
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$FeaturesMatrix=blah$FeaturesMatrix
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$signals_parameters=blah$signals_parameters
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$Xdata=Xdata
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$Ydata=Ydata
        useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$results_to_save=results_to_save
      }
      finaloutput = save_output(
        spectrum_index,
        signals_codes,
        results_to_save,
        autorun_data$buck_step,
        finaloutput
      )
      tryCatch({write_info(autorun_data$export_path, finaloutput)}, error = function(err) {
        print('Not possible to overwrite a csv file open with Microsoft Excel')
      })
      blah$finaloutput=finaloutput
      
    }
    
    }
    print("d")
    
    
    blah$p=p
    # blah$p2=p2
    blah$results_to_save=results_to_save
    blah$spectrum_index=spectrum_index
    blah$signals_codes=signals_codes
    blah$fitting_type=fitting_type
    
    
   
    # blah$finaloutput=finaloutput
    
  }
   blah$useful_data=useful_data

    # blah$autorun_data=autorun_data
  return(blah)
}
