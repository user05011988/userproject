validation = function(finaloutput,
                                 other_fit_parameters) {

  #Created by Daniel Cañueto 13/09/2016
  #Finding of suspicious quantifications through difference with predicted shift, signal to total area ratio, fitting error, and difference with expected relative intensity
  
  #TO DO: maybe would be better to analyze official relative intensity. 
  #TO DO: allow more than two signals when analyzing official relative intensity. 
  
  
  #shift analysis
  #correlation matrix of shift of signals and finding for every signal the signal that has best ability to predict shift
shift_corrmatrix=cor(finaloutput$shift,use='pairwise.complete.obs',method='spearman')
shift_corrmatrix=round(shift_corrmatrix,5)
shift_corrmatrix[shift_corrmatrix==1]=0
shift_reference=as.numeric(apply(shift_corrmatrix,1,which.max))
#preparation of matrix that will contain suspicious quantifications according to shift information
shift_alarmmatrix=matrix(0,dim(finaloutput$shift)[1],dim(finaloutput$shift)[2])
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
  shift_prediction=as.numeric(rlm_model$coefficients[1])+as.numeric(rlm_model$coefficients[2])*finaloutput$shift[,shift_reference[[ii]]]
  #Calculation of suspicious samples because of too much difference with expected shift
  shift_suspicioussamples=which(abs(finaloutput$shift[,ii]-shift_prediction)>other_fit_parameters$rlm_limit*rlm_model$scale)
  shift_alarmmatrix[shift_suspicioussamples,c(ii,shift_reference[ii])]=1
  
  }
  
#Analysis of which samples have too much fitting error
fitting_error_alarmmatrix=matrix(0,dim(finaloutput$fitting_error)[1],dim(finaloutput$fitting_error)[2])
fitting_error_alarmmatrix[finaloutput$fitting_error<other_fit_parameters$fitting_error_limit]=1


#Analysis of which samples have too scant signal respective to the total area where the signal is located
signal_area_ratio_alarmmatrix=matrix(0,dim(finaloutput$signal_area_ratio)[1],dim(finaloutput$signal_area_ratio)[2])
signal_area_ratio_alarmmatrix[finaloutput$signal_area_ratio>other_fit_parameters$signal_area_ratio_limit]=1



#Analysis of which metabolites have have relative intensity too far from expected according to robust linear model constructed

#Find which metabolites have more than one signal and analyzing case by case
# ind=grep('_s1',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s1',colnames(finaloutput$intensity))])]
# ind2=grep('_s2',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s2',colnames(finaloutput$intensity))])]
# 
# #I find how many samples will be used for the roubst linear model in the same way than with the shift
# intensity_alarmmatrix=matrix(0,dim(finaloutput$intensity)[1],dim(finaloutput$intensity)[2])
# for (ii in 1:length(ind)) {
#   rlm_preinfo=matrix(NA,dim(finaloutput$shift)[1],2)
#   rlm_samples=cbind(finaloutput$intensity[,ind[ii]],finaloutput$intensity[,ind2[ii]])
#   rlm_signalsdifference=rlm_samples[,1]-rlm_samples[,2]
#   rlm_signalsdifference_ind=sort(rlm_signalsdifference,decreasing=T,index.return=T)$ix
#   rlm_preinfo[dim(rlm_samples)[1],]=c(dim(rlm_samples)[1],dim(rlm_samples)[1]*cor(rlm_samples,use='pairwise.complete.obs',method='spearman')[1,2])
#   for (i in 1:(length(rlm_signalsdifference_ind)/2)) {
#     rlm_samples_sub=rlm_samples[-rlm_signalsdifference_ind[1:i],]
#     rlm_preinfo[dim(rlm_samples_sub)[1],]=c(dim(rlm_samples_sub)[1],dim(rlm_samples_sub)[1]*cor(rlm_samples,use='pairwise.complete.obs',method='spearman')[1,2])
#   }
#   # I analyze with how many samples there has been the best correlation (penalization-adjusted) and save the samples that will be used for the robust linear model
#   if(length(rlm_signalsdifference_ind[dim(rlm_samples)[1]-which.max(rlm_preinfo[,2])])>0) {
#     rlm_samples_sub = rlm_samples[-rlm_signalsdifference_ind[dim(rlm_samples)[1]-which.max(rlm_preinfo[,2])],]
#   } else {
#     rlm_samples_sub=rlm_samples
#   }
#   #Robust linear model and prediction of expected shifts
#   rlm_model=lmrob(rlm_samples_sub[,1]~rlm_samples_sub[,2])
#   
#   intensity_prediction=as.numeric(rlm_model$coefficients[1])+as.numeric(rlm_model$coefficients[2])*finaloutput$intensity[,ind2[ii]]
# 
#   intensity_suspicioussamples=which(abs(finaloutput$intensity[,ind[ii]]-intensity_prediction)>other_fit_parameters$rlm_limit*rlm_model$scale)
#   intensity_alarmmatrix[intensity_suspicioussamples,c(ind[ii],ind2[ii])]=1
# 
#   
# }


#I sum all "points" gained by every quantification
# alarmmatrix=shift_alarmmatrix+signal_area_ratio_alarmmatrix+fitting_error_alarmmatrix+intensity_alarmmatrix
alarmmatrix=shift_alarmmatrix+signal_area_ratio_alarmmatrix+fitting_error_alarmmatrix
colnames(alarmmatrix)=colnames(finaloutput$shift)
rownames(alarmmatrix)=rownames(finaloutput$shift)

return(alarmmatrix)
}