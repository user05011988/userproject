write_info = function(export_path, finaloutput,ROI_data) {
  dir.create(export_path)

write.csv(finaloutput$Area,
  file.path(export_path,
    "Area.csv"))
write.csv(finaloutput$shift,
  file.path(export_path,
    "shift.csv"))
write.csv(finaloutput$width,
  file.path(export_path,
    "width.csv"))
write.csv(
  finaloutput$signal_area_ratio,
  file.path(export_path,
    "signal_area_ratio.csv")
)
write.csv(
  finaloutput$correlation,
  file.path(export_path,
    "correlation.csv")
)
write.csv(
  finaloutput$intensity,
  file.path(export_path,
    "intensity.csv")
)
tryCatch(write.csv(ROI_data,file.path(export_path,"ROI_profiles_used.csv")), error = function(err) 
  print('Not possible to overwrite the original csv file')
)
}