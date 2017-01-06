save_roi_testing=function(blah,autorun_data,finaloutput,useful_data) {
  
if (blah$fitting_type == "Clean Sum" ||
    blah$fitting_type == "Baseline Sum") {
  
  
  useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$integration_parameters=blah$integration_parameters
  useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$plot=blah$p2



} else if (blah$fitting_type == "Clean Fitting" || blah$fitting_type ==
    "Baseline Fitting") {
 
  
  print(blah$spectrum_index)
  print(blah$signals_codes)
  print(length(blah$p2))
  blah$program_parameters$signals_to_quantify=NULL
  for (i in seq_along(blah$p2))  useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$plot=blah$p2[[i]]
  
  for (i in seq_along(blah$signals_codes)) {
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$import_excel_profile=blah$ROI_profile
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$program_parameters=blah$program_parameters
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$fitted_signals=blah$fitted_signals
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$plot_data=blah$plot_data
    # useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$FeaturesMatrix=blah$FeaturesMatrix
    # useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$signals_parameters=blah$signals_parameters
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$Xdata=blah$Xdata
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$Ydata=blah$Ydata
    useful_data[[blah$spectrum_index]][[blah$signals_codes[i]]]$results_to_save=blah$results_to_save
  }
  
  

}



  finaloutput = save_output(
      blah$spectrum_index,
      blah$signals_codes,
      blah$results_to_save,
      autorun_data$buck_step,
      finaloutput
    )

  tryCatch({write_info(autorun_data$export_path, finaloutput)}, error = function(err) {
    print('Not possible to overwrite a csv file open with Microsoft Excel')
  })

    dummy=list(finaloutput=finaloutput,useful_data=useful_data)
    return(dummy)
    
  }