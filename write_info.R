write_info = function(export_path, finaloutput) {
  

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
}