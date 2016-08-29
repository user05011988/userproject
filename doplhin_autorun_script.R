#aquí es diu a quina carpeta són els scripts
setwd("C:/Users/user/Documents/r_dolphin - csv")
#aquí es diu on hi és l'excel de paràmetres
parameters_path="Parameters_csv.csv"

library(R.matlab)
library(minpack.lm)
library(reshape)
library(ggplot2)
# library(xlsx)
source("peakpvoigt.R")
source("fitting_optimization.R")
source("fittingloop.R")
source("getOptions.R")
source("getLUB.R")
source("definitivefitting.R")
source("outputgenerator.R")
source("plotgenerator.R")
source("peakdet.R")
source("fitting_prep.R")
source("integration.R")
source("autorun_profile_xls.R")
source("Metadata2Buckets.R")
source("readspectrum.R")
source("jtp.R")
source("import_data_xls.R")
source("save_output.R")
require(compiler)
enableJIT(3)
imported_data=import_data_xls(parameters_path)
finaloutput=list()
dummy=matrix(NaN,dim(imported_data$dataset)[1],length(imported_data$signals_names))
rownames(dummy)=imported_data$Experiments
colnames(dummy)=imported_data$signals_names

finaloutput$Area=finaloutput$signal_area_ratio=finaloutput$fitting_error=finaloutput$shift=dummy          
export_path=imported_data$params$export_path
write.csv(as.data.frame(imported_data$params),file.path(export_path,'initialparams.csv'),row.names=F)
colnames(imported_data$dataset)=imported_data$ppm
rownames(imported_data$dataset)=imported_data$Experiments
write.csv(imported_data$dataset,file.path(export_path,'initialdataset.csv'))
write.table(imported_data$not_loaded_experiments,file.path(export_path,'not_loaded_experiments.csv'),row.names=F,col.names=F)
write.table(t(as.data.frame(imported_data$signals_names)),file.path(export_path,'library.csv'),row.names=F,col.names=F)

for (i in seq_along(imported_data$Experiments)) if(!dir.exists(file.path(export_path,imported_data$Experiments[i]))) dir.create(file.path(export_path,imported_data$Experiments[i]))


autorun_data=list(dataset=imported_data$dataset, ppm=imported_data$ppm, buck_step=imported_data$buck_step,
                  profile_folder_path=imported_data$params$profile_folder_path, signals_names=imported_data$signals_names,
                  signals_codes=imported_data$signals_codes,Experiments=imported_data$Experiments, E_max=imported_data$E_max,
                  P_max=imported_data$P_max,export_path=imported_data$params$export_path,freq=imported_data$freq)

finaloutput=autorun_profile_xls(autorun_data,finaloutput)
