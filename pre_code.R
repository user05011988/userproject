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
parameters_path = "C:/Users/user/Dropbox/interface_example/Parameters.csv"
parameters_path = "C:/Users/user/Documents/r_dolphin - csv/Parameters_binning_dataset_new.txt"
parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_20_2.csv"

parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_sencer.csv"
parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_sencer_repository.csv"

parameters_path = "C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Parameters_reduced_20.csv"

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
  file.path(imported_data$export_path, 'initialdataset.csv'),row.names=F)
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
  freq = imported_data$freq,
  Metadata=imported_data$Metadata
)

finaloutput = autorun(autorun_data, finaloutput)


other_fit_parameters = fitting_variables()


ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
dummy = which(!is.na(ROI_data[, 1]))
ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(ROI_data)[1]))
# mtcars2=ROI_data[1:2,4:11]
# mtcars=ROI_data[1:2,4:11]

ROI_names=paste(ROI_data[ROI_separator[, 1],1],ROI_data[ROI_separator[, 1],2])
select_options=1:length(ROI_names)
names(select_options)=ROI_names
t_test_data=autorun_data$dataset

ss=unique(autorun_data$Metadata[,1])
tt=matrix(NA,length(ss),dim(t_test_data)[2])
for (ind in seq_along(ss)) {
  for (k in 1.:dim(t_test_data)[2]) {
    tt[ind,k]=tryCatch(shapiro.test(t_test_data[autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
  }
  
}
p_value_bucketing=rep(NA,dim(t_test_data)[2])
for (k in 1:dim(t_test_data)[2]) {
  # if (!any(is.na(t_test_data[,k]))) {
    if (!any(tt[,k]<0.05,na.rm=T)) {
      p_value_bucketing[k]=tryCatch(wilcox.test(t_test_data[autorun_data$Metadata[,1]==ss[1],k],t_test_data[autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
    } else {
      p_value_bucketing[k]=tryCatch(t.test(t_test_data[autorun_data$Metadata[,1]==ss[1],k],t_test_data[autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
    }
    
  # }
}
p_value_bucketing[is.na(p_value_bucketing)]=0
plotdata = data.frame(Xdata=autorun_data$ppm, p_value_bucketing)
mediani=apply(autorun_data$dataset,2,function(x) median(x,na.rm=T))
# plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
bucketing <- cbind(melt(plotdata, id = "Xdata"),mediani)
plot_ly(data=bucketing,x=~Xdata,y=~mediani,color=~value,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"),yaxis = list(range = c(0, max(mediani))))

t_test_data_2=finaloutput$Area
t_test_data_2[finaloutput$fitting_error>other_fit_parameters$fitting_error_limit]=NA
t_test_data_2[finaloutput$signal_area_ratio<other_fit_parameters$signal_area_ratio_limit]=NA

ll=as.data.frame(t_test_data_2)
Xwit=cbind(ll,factor(autorun_data$Metadata[,1]))
# rownames(Xwit)=NULL
ab=melt(Xwit)
colnames(ab)=c('Metadata','Signal','Value')
outlier_table=matrix(0,dim(ll)[1],dim(ll)[2])
colnames(outlier_table)=colnames(t_test_data_2)
rownames(outlier_table)=rownames(finaloutput$fitting_error)

for (j in 1:length(ss)) {
  outlier_table[autorun_data$Metadata==ss[j],][sapply(ll[autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
  # ind=which(autorun_data$Metadata==ss[j])
  # sell$outlier_table[ind[sell$finaloutput$Area[autorun_data$Metadata==ss[j],i] %in%  outliers],i]=1
}
ss=unique(autorun_data$Metadata[,1])
tt=matrix(NA,length(ss),dim(t_test_data_2)[2])
for (ind in seq_along(ss)) {
  for (k in 1:dim(t_test_data_2)[2]) {
    tt[ind,k]=tryCatch(shapiro.test(t_test_data_2[autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
  }
  
}
p_value=rep(NA,dim(t_test_data_2)[2])
for (k in 1:dim(t_test_data_2)[2]) {
  # if (!any(is.na(t_test_data_2[,k]))) {
  if (!any(tt[,k]<0.05,na.rm=T)) {
    p_value[k]=tryCatch(wilcox.test(t_test_data_2[autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
  } else {
    p_value[k]=tryCatch(t.test(t_test_data_2[autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
  }
  
  # }
}
p_value_final=t(as.matrix(p_value))
colnames(p_value_final)=colnames(t_test_data_2)


save(setdiff(ls(), lsf.str()),file='ff.RData')
