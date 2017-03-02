#########################################################################
#     Dolphin - R package for reliable automatic quantification of 1H 1D NMR spectra
#     Copyright (C) 2017 Daniel Cañueto
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
############################################################################

#Quantification of ROIs in the interface. Structure is quite similar to 'autorun'


interface_quant = function(autorun_data, finaloutput,ind,ROI_profile,is_autorun,useful_data) {
  
  provisional_data=list(finaloutput=finaloutput,useful_data=useful_data)

  if (is_autorun=='N') {
 
      indexes=ind-2
  } else {
    indexes=1:dim(autorun_data$dataset)[1]
  }
  
  ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-autorun_data$ppm))
  Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
  program_parameters=autorun_data$program_parameters
  program_parameters$freq = autorun_data$freq
  program_parameters$ROI_buckets = ROI_buckets
  program_parameters$buck_step = autorun_data$buck_step
  
  fitting_type = as.character(ROI_profile[1, 3])
  signals_to_quantify = which(ROI_profile[, 5] >0)
  signals_codes = signals_names = rep(NA,length(signals_to_quantify))
  j = 1
  for (i in signals_to_quantify) {
    k = which(autorun_data$signals_names == paste(ROI_profile[i,
      4],ROI_profile[i,5],sep='_'))
    
    signals_codes[j] = autorun_data$signals_codes[k]
    signals_names[j] = as.character(autorun_data$signals_names[k])
    j = j + 1
  }
  for (spectrum_index in indexes) {
    print(paste("Spectrum ",spectrum_index))
    
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
    experiment_name = autorun_data$Experiments[[spectrum_index]]
   
    # If the quantification is through integration with or without baseline
    if (fitting_type == "Clean Sum" ||
        fitting_type == "Baseline Sum") {
      clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
      Ydatamedian=as.numeric(apply(autorun_data$dataset[, ROI_buckets,drop=F],2,median))
      
     dummy = integration(clean_fit, Xdata,Ydata,Ydatamedian,interface='T')
      
      results_to_save=dummy$results_to_save
      p=dummy$p
      plot_data=dummy$plot_data
      
      # provisional_data$integration_parameters=integration_parameters
      #Generation of output variables specific of every quantification
      if (is_autorun=='Y') {
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$ROI_profile=ROI_profile
        # provisional_data$useful_data[[spectrum_index]][[signals_codes]]$integration_parameters=integration_parameters
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$plot_data=dummy$plot_data
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$Xdata=Xdata
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$Ydata=Ydata
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$results_to_save=results_to_save
        provisional_data$useful_data[[spectrum_index]][[signals_codes]]$error1=results_to_save$fitting_error
        
        finaloutput = save_output(
          spectrum_index,
          signals_codes,
          results_to_save,
          autorun_data$buck_step,
          finaloutput
        )
        
      }

      #If the quantification is through fitting with or without baseline
    } else if (fitting_type == "Clean Fitting" || fitting_type ==
               "Baseline Fitting") {
      program_parameters$clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
        "N")
      program_parameters$freq=autorun_data$freq
      
      FeaturesMatrix = fitting_prep(Xdata,
                                    Ydata,
                                    ROI_profile[, 5:11,drop=F],
                                    program_parameters)
      #Calculation of the parameters that will achieve the best fitting
      dummy = fittingloop(FeaturesMatrix,
                                       Xdata,
                                       Ydata,
                                       program_parameters)
      signals_parameters=dummy$signals_parameters
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
        'half_band_width',
        'gaussian',
        'J_coupling'
      ) 
      
      Xdata_2=autorun_data$ppm
      # signals_parameters_2=unlist(signals_parameters_2)
      # multiplicities_2=unlist(multiplicities_2)
      # roof_effect_2=unlist(roof_effect_2)
      # signals_parameters_2=unlist(signals_parameters)
      multiplicities_2=unlist(multiplicities)
      roof_effect_2=unlist(roof_effect)

      fitted_signals = fitting_optimization(signals_parameters_2,
                                         Xdata_2,multiplicities_2,roof_effect_2,Ydata,program_parameters$freq)
      # signals_parameters=as.matrix(signals_parameters)
      
      # dim(signals_parameters_2) = c(5, length(signals_parameters_2)/5)
      # rownames(signals_parameters_2) = c(
      #   'intensity',
      #   'shift',
      #   'half_band_width',
      #   'gaussian',
      #   'J_coupling'
      # ) 
      program_parameters$signals_to_quantify=signals_to_quantify
      Ydata_2 = as.numeric(autorun_data$dataset[spectrum_index, ])

      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      dummy = output_generator(
        signals_to_quantify,
        fitted_signals,
        Ydata_2,
        Xdata_2,
        signals_parameters,multiplicities,ROI_buckets
      )
      output_data=dummy$output_data
      error1=dummy$error1
      
      if (any(output_data$fitting_error>0.05)==T) {
        dummy = fittingloop(FeaturesMatrix,
          Xdata,
          Ydata,
          program_parameters)
        signals_parameters=dummy$signals_parameters
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
          'half_band_width',
          'gaussian',
          'J_coupling'
        ) 
        
        Xdata_2=autorun_data$ppm
        # signals_parameters_2=unlist(signals_parameters_2)
        # multiplicities_2=unlist(multiplicities_2)
        # roof_effect_2=unlist(roof_effect_2)
        # signals_parameters_2=unlist(signals_parameters)
        multiplicities_2=unlist(multiplicities)
        roof_effect_2=unlist(roof_effect)
        
        fitted_signals = fitting_optimization(signals_parameters_2,
          Xdata_2,multiplicities_2,roof_effect_2,Ydata,program_parameters$freq)
        # signals_parameters=as.matrix(signals_parameters)
        
        # dim(signals_parameters_2) = c(5, length(signals_parameters_2)/5)
        # rownames(signals_parameters_2) = c(
        #   'intensity',
        #   'shift',
        #   'half_band_width',
        #   'gaussian',
        #   'J_coupling'
        # ) 
        program_parameters$signals_to_quantify=signals_to_quantify
        Ydata_2 = as.numeric(autorun_data$dataset[spectrum_index, ])
        
        #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
        dummy = output_generator(
          signals_to_quantify,
          fitted_signals,
          Ydata_2,
          Xdata_2,
          signals_parameters,multiplicities,ROI_buckets
        )
        
        #If new deconvolution has improved previous one
        if (dummy$error1<error1) {
          output_data=dummy$output_data
          error1=dummy$error1
        }}

      #Generation of the dataframe with the final output variables
      results_to_save = data.frame(
        shift = output_data$shift,
        Area = output_data$Area,
        signal_area_ratio = output_data$signal_area_ratio,
        fitting_error = output_data$fitting_error,
        intensity = output_data$intensity,
        half_band_width = output_data$half_band_width
      )
      
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
      
colors=c('red','blue','black','brown','cyan','green','yellow')
      p=plot_ly(plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL) %>% layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'), yaxis = list(range=c(0,max(Ydata)),title = 'Intensity'))
        for (i in 4:nrow(plot_data)) {
          plotdata5 =  data.frame(Xdata=Xdata_2, variable=rownames(plot_data)[i] ,value=plot_data[i,])
      
        p=p %>%add_trace(data=plotdata5,x=~Xdata,y=~value,name=~variable,type='scatter',mode='lines',fill='tozeroy',fillcolor=colors[i-3])   
}
      

    signals_parameters=rbind(signals_parameters,multiplicities,roof_effect)
    if (provisional_data$useful_data[[spectrum_index]][[signals_codes[1]]]$error1>0.8*error1) {
    provisional_data$program_parameters=program_parameters
    provisional_data$results_to_save=results_to_save
    provisional_data$ROI_profile=ROI_profile
    provisional_data$Ydata=Ydata
    provisional_data$plot_data=plot_data[,ROI_buckets]
    provisional_data$FeaturesMatrix=FeaturesMatrix
    provisional_data$error1=error1
    provisional_data$signals_parameters=signals_parameters
    provisional_data$Xdata=Xdata
    }
    if (is_autorun=='Y') {
      if (provisional_data$useful_data[[spectrum_index]][[signals_codes[1]]]$error1>error1) {
      for (i in seq_along(signals_codes)) {
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$ROI_profile=ROI_profile
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$program_parameters=program_parameters
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$plot_data=plot_data[,ROI_buckets]
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$FeaturesMatrix=FeaturesMatrix
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$signals_parameters=signals_parameters
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$error1=error1
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$Xdata=Xdata
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$Ydata=Ydata
        provisional_data$useful_data[[spectrum_index]][[signals_codes[i]]]$results_to_save=results_to_save
        # print('Change')
      }
        provisional_data$finaloutput = save_output(
          spectrum_index,
          signals_codes,
          results_to_save,
          autorun_data$buck_step,
          provisional_data$finaloutput)
      }
      }
     
    }
    
    provisional_data$p=p
    provisional_data$results_to_save=results_to_save
    provisional_data$spectrum_index=spectrum_index
    provisional_data$signals_codes=signals_codes
    provisional_data$fitting_type=fitting_type
    provisional_data$signals_names=signals_names
 
  }
  return(provisional_data)
}
