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

#Autorun of quantification for all experiments using the information located at the ROI patterns file

autorun = function(autorun_data, finaloutput,useful_data) {
  
  print('Be patient. Gonna take a while. You should be writing, meanwhile.')

  #Splitting of ROI data into individual ROIs to be quantified
  dummy = which(is.na(autorun_data$ROI_data[, 1]))
  if (length(dummy)==0) dummy=dim(autorun_data$ROI_data)[1]+1
  ind=which(duplicated(autorun_data$ROI_data[-dummy,1:2])==F)
  ROI_separator = cbind(ind, c(ind[-1] - 1, dim(autorun_data$ROI_data[-dummy,])[1]))
  
  #For every ROI
  for (ROI_index in seq_along(ROI_separator[, 1])) {
    
    #Preparation of ROI parameters
    ROI_profile = autorun_data$ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]
    ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-autorun_data$ppm))
    if (length(ROI_buckets)<5) next
    if (ROI_buckets[1]>ROI_buckets[2]) ROI_buckets=rev(ROI_buckets)
    Xdata = autorun_data$ppm[ROI_buckets]
    fitting_type = as.character(ROI_profile[1, 3])
    signals_to_quantify = which(ROI_profile[, 5] >= 1)
    
    print(paste(ROI_profile[1,1], ROI_profile[1,2], sep = '-'))
    print(paste('ROI',ROI_index,'of',nrow(ROI_separator)))
    
      #Preparation of program parameters to be sued during fitting, with some variables added to ease interpretability of code
      program_parameters=autorun_data$program_parameters
      program_parameters$freq = autorun_data$freq
      program_parameters$ROI_buckets = ROI_buckets
      program_parameters$buck_step = autorun_data$buck_step
    
      #Association of columns of output with signals names in ROI data 
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
    #Quantification for every spectrum
    for (spectrum_index in 1:nrow(autorun_data$dataset)) {
      
      print(paste("Spectrum ",spectrum_index))
     
      #Preparation of necessary variables to store figures and information of the fitting
      Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
      #If the quantification is through integration with or without baseline
      if (fitting_type == "Clean Sum" ||
          fitting_type == "Baseline Sum") {
        #Fitting error is calculated through the comparison with the median spectrum, so singals interfering with the integration can be controlled
        Ydatamedian=as.numeric(apply(autorun_data$dataset[, ROI_buckets,drop=F],2,median))
        
        clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
        dummy = integration(clean_fit, Xdata,Ydata,Ydatamedian)
        results_to_save=dummy$results_to_save
        
        #Generation of useful variables specific of every quantification
        useful_data[[spectrum_index]][[signals_codes]]$ROI_profile=ROI_profile
        useful_data[[spectrum_index]][[signals_codes]]$plot_data=dummy$plot_data
        useful_data[[spectrum_index]][[signals_codes]]$Xdata=Xdata
        useful_data[[spectrum_index]][[signals_codes]]$Ydata=Ydata
        useful_data[[spectrum_index]][[signals_codes]]$results_to_save=results_to_save
        useful_data[[spectrum_index]][[signals_codes]]$error1=results_to_save$fitting_error
        
        #If the quantification is through fitting with or without baseline
      } else if (fitting_type == "Clean Fitting" || fitting_type ==
          "Baseline Fitting") {

        program_parameters$clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
          "N")
        
        
        #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
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

                #Fitting of the signals
        multiplicities=c(FeaturesMatrix[,11],rep(1,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        roof_effect=c(FeaturesMatrix[,12],rep(0,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        fitted_signals = fitting_optimization(signals_parameters,
          Xdata,multiplicities,roof_effect,Ydata,program_parameters$freq)
               dim(signals_parameters) = c(5, length(signals_parameters)/5)
        rownames(signals_parameters) = c(
          'intensity',
          'shift',
          'half_band_width',
          'gaussian',
          'J_coupling'
        )
        signals_parameters=rbind(signals_parameters,multiplicities,roof_effect)
        
        #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
        dummy = output_generator(
          signals_to_quantify,
          fitted_signals,
          Ydata,
          Xdata,
          signals_parameters,multiplicities
        )
        output_data=dummy$output_data
        error1=dummy$error1
        
        #If any of the qunatificaitons has more than 5% fitting error, try again the deconvolution
        if (any(output_data$fitting_error>0.05)==T) {
        dummy = fittingloop(FeaturesMatrix,
          Xdata,
          Ydata,
          program_parameters)
        
        signals_parameters=dummy$signals_parameters
        #Fitting of the signals
        multiplicities=c(FeaturesMatrix[,11],rep(1,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        roof_effect=c(FeaturesMatrix[,12],rep(0,(length(signals_parameters)/5)-dim(FeaturesMatrix)[1]))
        fitted_signals = fitting_optimization(signals_parameters,
          Xdata,multiplicities,roof_effect,Ydata,program_parameters$freq)
        dim(signals_parameters) = c(5, length(signals_parameters)/5)
        rownames(signals_parameters) = c(
          'intensity',
          'shift',
          'half_band_width',
          'gaussian',
          'J_coupling'
        )
        signals_parameters=rbind(signals_parameters,multiplicities,roof_effect)
        
        #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
        dummy = output_generator(
          signals_to_quantify,
          fitted_signals,
          Ydata,
          Xdata,
          signals_parameters,multiplicities
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
        
        #Generation of the figure data
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
        
        #Generation of useful variables specific of every quantification
        # program_parameters$signals_to_quantify=signals_to_quantify
        #   program_parameters$signals_to_quantify=NULL
          for (i in seq_along(signals_codes)) {
          useful_data[[spectrum_index]][[signals_codes[i]]]$ROI_profile=ROI_profile
          useful_data[[spectrum_index]][[signals_codes[i]]]$program_parameters=program_parameters
          useful_data[[spectrum_index]][[signals_codes[i]]]$plot_data=plot_data
          useful_data[[spectrum_index]][[signals_codes[i]]]$error1=error1
          useful_data[[spectrum_index]][[signals_codes[i]]]$FeaturesMatrix=FeaturesMatrix
          useful_data[[spectrum_index]][[signals_codes[i]]]$signals_parameters=signals_parameters
          useful_data[[spectrum_index]][[signals_codes[i]]]$Xdata=Xdata
          useful_data[[spectrum_index]][[signals_codes[i]]]$Ydata=Ydata
          useful_data[[spectrum_index]][[signals_codes[i]]]$results_to_save=results_to_save
          }
       
        
      }
      
      #Generation of output variables specific of every quantification
      
      finaloutput = save_output(
        spectrum_index,
        signals_codes,
        results_to_save,
        autorun_data$buck_step,
        finaloutput
      )

      
    }
    
  }
  print("Done!")
 
  dummy=list(finaloutput=finaloutput,useful_data=useful_data)
  return(dummy)
}