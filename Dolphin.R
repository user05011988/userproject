#TO DO: save parameters of imported_data not exported to autorun_data in separate list

setwd("C:/Users/user/Documents/Dolphin/R")

source('packages_sources.R')
packages_sources()
compiler::enableJIT(3)

#Reading of parameters file
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_portuguesos.csv"
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_portuguesos_new.csv"

parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_19_TSP_improved.csv"
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_binning_dataset.csv"
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_csv.csv"
parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_20.csv"
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_binning_dataset_new.txt"
parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_20_2.csv"
parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters.csv"

#import of data (dataset in csv format or Bruker nmr folder)
imported_data = import_data(parameters_path)


if (!dir.exists(imported_data$export_path))
  dir.create(imported_data$export_path)
for (i in seq_along(imported_data$Experiments)) {
  if (!dir.exists(file.path(imported_data$export_path, imported_data$Experiments[i]))) {
    dir.create(file.path(imported_data$export_path, imported_data$Experiments[i]))
  }
}
#creation of list with the different final outputs
finaloutput = list()
dummy = matrix(NaN,
               dim(imported_data$dataset)[1],
               length(imported_data$signals_names))
rownames(dummy) = imported_data$Experiments
colnames(dummy) = imported_data$signals_names
finaloutput$Area = finaloutput$signal_area_ratio = finaloutput$fitting_error =
  finaloutput$shift = finaloutput$intensity = finaloutput$width = dummy

#creation of several outputs with data of interest before beginnig the quantification
write.csv(
  as.data.frame(imported_data$params),
  file.path(imported_data$export_path, 'initialparams.csv'),
  row.names = F
)
colnames(imported_data$dataset) = imported_data$ppm
rownames(imported_data$dataset) = imported_data$Experiments
write.csv(imported_data$dataset,
          file.path(imported_data$export_path, 'initialdataset.csv'))
if ("not_loaded_experiments" %in% names(imported_data))
  write.table(
    imported_data$not_loaded_experiments,
    file.path(imported_data$export_path, 'not_loaded_experiments.csv'),
    row.names = F,
    col.names = F
  )
# write.table(
#   t(as.data.frame(imported_data$signals_names)),
#   file.path(imported_data$export_path, 'used_library.csv'),
#   row.names = F,
#   col.names = F
# )

#creation of a folder for every experiment
# for (i in seq_along(imported_data$Experiments))
#   if (!dir.exists(file.path(export_path, imported_data$Experiments[i])))
#     dir.create(file.path(export_path, imported_data$Experiments[i]))

#creation of list of necessary parameters for automatic quantification
autorun_data = list(
  dataset = imported_data$dataset,
  ppm = imported_data$ppm,
  buck_step = imported_data$buck_step,
  profile_folder_path = imported_data$profile_folder_path,
  signals_names = imported_data$signals_names,
  signals_codes = imported_data$signals_codes,
  Experiments = imported_data$Experiments,
  export_path = imported_data$export_path,
  freq = imported_data$freq
)
 rm(imported_data)

#automatic quantification
finaloutput = autorun(autorun_data, finaloutput)
