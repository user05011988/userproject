correction = function(finaloutput,autorun_data) {
  other_fit_parameters = fitting_variables()
  other_fit_parameters$freq = autorun_data$freq
  other_fit_parameters$buck_step = autorun_data$buck_step

shift_corrmatrix=cor(finaloutput$shift,use='pairwise.complete.obs',method='spearman')
shift_corrmatrix=round(shift_corrmatrix,5)
shift_corrmatrix[shift_corrmatrix==1]=0
shift_reference=as.numeric(apply(shift_corrmatrix,1,which.max))
#preparation of matrix that will contain suspicious quantifications according to shift information
shift_alarmmatrix=shift_prediction=matrix(0,dim(finaloutput$shift)[1],dim(finaloutput$shift)[2])
#for every signal it is performed a robust linear model according to the signal with most similar behavior.
#However, even robust linear models can suffer if there are too many spectra where there have been problems.
#It is necessary to remove before outliers. I do it seeing how correlation evolves when I remove the sample
#where there is more difference with the mean shift difference between the two signals.
#To avoid perfect linear models because of too few samples that do not represent actual condtions, I penalze every removal of a sample.

for (ii in 1:dim(shift_corrmatrix)[1]) {
  rlm_preinfo=matrix(NA,dim(finaloutput$shift)[1],2)
  rlm_samples=cbind(finaloutput$shift[,ii],finaloutput$shift[,shift_reference[[ii]]])
  rlm_signalsdifference=rlm_samples[,1]-rlm_samples[,2]
  rlm_signalsdifference_ind=sort(rlm_signalsdifference,decreasing=T,index.return=T)$ix
  rlm_preinfo[dim(rlm_samples)[1],]=c(dim(rlm_samples)[1],dim(rlm_samples)[1]*cor(rlm_samples,use='pairwise.complete.obs',method='spearman')[1,2])
  for (i in 1:(length(rlm_signalsdifference_ind)/2)) {
    rlm_samples_sub=rlm_samples[-rlm_signalsdifference_ind[1:i],]
    rlm_preinfo[dim(rlm_samples_sub)[1],]=c(dim(rlm_samples_sub)[1],dim(rlm_samples_sub)[1]*cor(rlm_samples,use='pairwise.complete.obs',method='spearman')[1,2])
  }
  # I analyze with how many samples there has been the best correlation (penalization-adjusted) and save the samples that will be used for the robust linear model
  if(length(rlm_signalsdifference_ind[dim(rlm_samples)[1]-which.max(rlm_preinfo[,2])])>0) {
    rlm_samples_sub = rlm_samples[-rlm_signalsdifference_ind[dim(rlm_samples)[1]-which.max(rlm_preinfo[,2])],]
  } else {
    rlm_samples_sub=rlm_samples
  }
  
  #Robust linear model and prediction of expected shifts
  rlm_model=lmrob(rlm_samples_sub[,1]~rlm_samples_sub[,2])
  shift_prediction[,ii]=as.numeric(rlm_model$coefficients[1])+as.numeric(rlm_model$coefficients[2])*finaloutput$shift[,shift_reference[[ii]]]
  #Calculation of suspicious samples because of too much difference with expected shift
  shift_suspicioussamples=which(abs(finaloutput$shift[,ii]-shift_prediction[,ii])>other_fit_parameters$rlm_limit*rlm_model$scale)
  shift_alarmmatrix[shift_suspicioussamples,c(ii,shift_reference[ii])]=1
  
}


rownames(shift_alarmmatrix)=rownames(shift_prediction)=rownames(finaloutput$shift)
colnames(shift_alarmmatrix)=colnames(shift_prediction)=colnames(finaloutput$shift)


aa=list.files(autorun_data$export_path)

for (exp in 1:dim(shift_alarmmatrix)[1]) {
  
  c=which(aa==rownames(shift_alarmmatrix)[exp])
  checkmet=which(shift_alarmmatrix[exp,]==1)
  
  if (length(checkmet)>0) {
  for (met in seq_along(checkmet)) {

ab=list.files(paste(autorun_data$export_path,aa[c],sep='/'))
d=which(ab==colnames(shift_alarmmatrix)[checkmet[met]])
fit_check=list.files(paste(autorun_data$export_path,aa[c],ab[d],sep='/'))
if (file.exists(paste(autorun_data$export_path,aa[c],ab[d],'import_excel_profile.csv',sep='/'))) {
import_excel_profile=read.csv(paste(autorun_data$export_path,aa[c],ab[d],'import_excel_profile.csv',sep='/'),stringsAsFactors = F)[,-1]
# FeaturesMatrix=read.csv(paste(autorun_data$export_path,aa[c],ab[d],'FeaturesMatrix.csv',sep='/'))[,-1]
Xdata=as.numeric(unlist(read.csv(paste(autorun_data$export_path,aa[c],ab[d],'Xdata.csv',sep='/'),sep=' ')))
Ydata=as.numeric(unlist(read.csv(paste(autorun_data$export_path,aa[c],ab[d],'Ydata.csv',sep='/'),sep=' ')))
other_fit_parameters=as.list(read.csv(paste(autorun_data$export_path,aa[c],ab[d],'other_fit_parameters.csv',sep='/'),stringsAsFactors = F)[1,])

fitting_type = as.character(import_excel_profile[1, 3])
# if (fitting_type == "Clean Fitting" || fitting_type ==
#      "Baseline Fitting") {


signals_to_quantify = which(import_excel_profile[, 7] == 1)
tre=which(colnames(shift_prediction)==ab[d])
tre2=which(import_excel_profile[, 4] == ab[d])
signals_codes=c()
for (i in seq_along(signals_to_quantify)) {
  signals_codes=c(signals_codes,which(autorun_data$signals_names == import_excel_profile[signals_to_quantify[tre2],
                                                             4]))
}


import_excel_profile[signals_to_quantify[tre2],5]=shift_prediction[exp,tre]
import_excel_profile[signals_to_quantify[tre2],11]=0.002
initial_fit_parameters = import_excel_profile[, 5:11,drop=F]
experiment_name=aa[c]
signals_names=as.character(import_excel_profile[signals_to_quantify, 4])
is_roi_testing = "N"
plot_path=character()
for (i in seq_along(signals_to_quantify)) {
  plot_path[i] = paste(autorun_data$export_path,experiment_name,ab[which(ab==signals_names[i])],sep='/')
}

colnames(initial_fit_parameters) = c(
  "positions",
  "widths",
  "quantification_or_not",
  "multiplicities",
  "Jcoupling",
  "roof_effect",
  "shift_tolerance"
)

scaledYdata = as.vector(Ydata / (max(Ydata)))


FeaturesMatrix = fitting_prep(Xdata,
                              scaledYdata,
                              initial_fit_parameters,
                              other_fit_parameters)


#Calculation of the parameters that will achieve the best fitting
signals_parameters = fittingloop(FeaturesMatrix,
                                 Xdata,
                                 scaledYdata,
                                 other_fit_parameters)

#Fitting of the signals
fitted_signals = definitivefitting(signals_parameters,
                                   Xdata)

output_data = output_generator(
  signals_to_quantify,
  fitted_signals,
  scaledYdata,
  Xdata,
  signals_parameters
)

output_data$intensity=signals_parameters[1, signals_to_quantify] * max(Ydata)

#Generation of the dataframe with the final output variables
results_to_save = data.frame(
  shift = output_data$shift,
  Area = output_data$Area * max(Ydata),
  signal_area_ratio = output_data$signal_area_ratio,
  fitting_error = output_data$fitting_error,
  intensity = output_data$intensity
)

#Adaptation of the quantification to de-scaled Ydata
# results_to_save$Area = results_to_save$Area * max(Ydata)

#Generation of the figure when the conditions specified in the Parameters file are accomplished
plot_data = rbind(
  output_data$signals_sum,
  output_data$baseline_sum,
  output_data$fitted_sum,
  output_data$signals
)
rownames(plot_data) = c("signals_sum",
                        "baseline_sum",
                        "fitted_sum",
                        as.character(import_excel_profile[,4]))

other_fit_parameters$signals_to_quantify=signals_to_quantify

plotgenerator(
  results_to_save,
  plot_data,
  Xdata,
  Ydata,
  fitted_signals,
  other_fit_parameters,
  signals_names,
  experiment_name,
  is_roi_testing,
  plot_path
)

#Generation of output variables specific of every quantification
for (i in seq_along(plot_path)) {
  write.csv(
    import_excel_profile,
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
    row.names = F
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
  # row.names = F,
  # col.names = F))
  write.csv(results_to_save,
            file.path(plot_path[i], "results_to_save.csv"),
            row.names = F)
}

  }
  }
#Generation of output variables specific of every ROI

finaloutput = save_output(
  exp,
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
}
}

return(finaloutput)
}