write_info = function(export_path, finaloutput,ROI_data) {
  dir.create(export_path)

write.csv(finaloutput$Area,
  file.path(export_path,
    "Area.csv"))
write.csv(finaloutput$shift,
  file.path(export_path,
    "shift.csv"))
write.csv(finaloutput$half_band_width,
  file.path(export_path,
    "half_band_width.csv"))
write.csv(
  finaloutput$signal_area_ratio,
  file.path(export_path,
    "signal_area_ratio.csv")
)
write.csv(
  finaloutput$fitting_error,
  file.path(export_path,
    "fitting_error.csv")
)
write.csv(
  finaloutput$intensity,
  file.path(export_path,
    "intensity.csv")
)
tryCatch(write.csv(ROI_data,file.path(export_path,"ROI_profiles_used.csv"),row.names=F), error = function(err) 
  print('Not possible to overwrite the original csv file')
)
}