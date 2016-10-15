save_roi_testing=function(blah,autorun_data,final_output) {
  p=blah$p
  # blah$finaloutput=finaloutput
  results_to_save=blah$results_to_save
  spectrum_index=blah$spectrum_index
  signals_codes=blah$signals_codes
  fitting_type=blah$fitting_type
  plot_path=blah$plot_path
  
  print(fitting_type)
  
  # print(plot_path)
# list2env(blah,.GlobalEnv)
if (fitting_type == "Clean Sum" ||
    fitting_type == "Baseline Sum") {
  
  
  integration_parameters=blah$integration_parameters
  
ggsave(
  file.path(integration_parameters$plot_path, 'Fit.jpeg'),plot = p,
  width = 10,
  height = 5
)
write.csv(
  integration_parameters,
  file.path(plot_path,
    "integration_parameters.csv"),
  row.names = F
)

} else if (fitting_type == "Clean Fitting" || fitting_type ==
    "Baseline Fitting") {
  signals_parameters=blah$signals_parameters
  other_fit_parameters=blah$other_fit_parameters
  
  results_to_save=blah$results_to_save
  ROI_profile=blah$import_excel_profile
  # other_fit_parameters$signals_to_quantify=ROI_profile[,7]
  
  
  Ydata=blah$Ydata
  fitted_signals=blah$fitted_signals
  plot_data=blah$plot_data
  FeaturesMatrix=blah$FeaturesMatrix
  signals_parameters=blah$signals_parameters
  Xdata=blah$Xdata
  # print(other_fit_parameters$signals_to_quantify)
  # print(results_to_save$signal_area_ratio)
  # print(results_to_save$fitting_error)
  # print(other_fit_parameters$signals_to_quantify)
  
  # print(other_fit_parameters)
for (r in 1:length(results_to_save$signal_area_ratio)) {
  #There is only creation of plot if the conditions specified in the Parameters file are accomplished
  if (results_to_save$signal_area_ratio[other_fit_parameters$signals_to_quantify[r]] < other_fit_parameters$signal_area_ratio_plot ||
      results_to_save$fitting_error[other_fit_parameters$signals_to_quantify[r]] > other_fit_parameters$fitting_error_plot) {
    print(paste(plot_path))
ggsave(paste(plot_path[other_fit_parameters$signals_to_quantify[r]],"Fit.jpeg",sep='/'),plot = p,width = 10, height = 5)
  }
}
for (i in seq_along(plot_path)) {
  write.csv(
    ROI_profile,
    file.path(plot_path[i],
      "import_excel_profile.csv")
    # row.names = F
  )
  write.table(Ydata,
    file.path(plot_path[i], "Ydata.csv"),
    # row.names = F,
    col.names = F)
  
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
  write.table(Xdata,
    file.path(plot_path[i], "Xdata.csv"))
  # row.names = F,
  # col.names = F))
  write.table(Ydata,
    file.path(plot_path[i], "Ydata.csv"))
  write.csv(results_to_save,
    file.path(plot_path[i], "results_to_save.csv"),
    row.names = F)
  
}
}


    # spectrum_index
    # signals_codes
    # autorun_data
    # finaloutput
    
    
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