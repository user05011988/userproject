validation = function(finaloutput,
                                 program_parameters) {

  #Created by Daniel Cañueto 13/09/2016
  #Finding of suspicious quantifications through difference with predicted shift, signal to total area ratio, fitting error, and difference with expected relative intensity
  
  #TO DO: maybe would be better to analyze official relative intensity. 
  #TO DO: allow more than two signals when analyzing official relative intensity. 
  
  
  #shift analysis
  #correlation matrix of shift of signals and finding for every signal the signal that has best ability to predict shift
  ind=which(apply(finaloutput$width,2, function(x) all(is.na(x)))==F)
  
  corr_area_matrix=cor(finaloutput$Area[,ind],use='pairwise.complete.obs',method='spearman')
  shift_corrmatrix=cor(finaloutput$shift[,ind],use='pairwise.complete.obs',method='spearman')
  fo=matrix(0,dim(finaloutput$shift)[1],dim(finaloutput$shift[,ind])[2])
  flo=array(0,dim=c(dim(finaloutput$shift)[1],dim(finaloutput$shift[,ind])[2],3))
 
  for (ii in seq_along(ind)) {
    ll=shift[,sort(abs(shift_corrmatrix[,ii]),decreasing=T,index.return=T)$ix[1:3]]
    nanana=tryCatch({lmrob(ll[,1] ~ ll[,2],control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(ll[,1] ~ ll[,2])},warning= function(e) {lm(ll[,1] ~ ll[,2])})
    tro1=suppressWarnings(predict(nanana, interval='prediction'))
    sf=which(shift[,ii]<tro1[,2]|shift[,ii]>tro1[,3])
    nanana=tryCatch({lmrob(ll[,1] ~ ll[,3],control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(ll[,1] ~ ll[,3])},warning= function(e) {lm(ll[,1] ~ ll[,3])})    
    tro2=suppressWarnings(predict(nanana, interval='prediction'))
    sg=which(shift[,ii]<tro2[,2]|shift[,ii]>tro2[,3])
    flo[,ii,]=(tro1+tro2)/2
    fo[Reduce(intersect, list(sf,sg)),ii]=1
    
  }
  colnames(fo)=colnames(finaloutput$shift)[ind]
  rownames(fo)=rownames(finaloutput$shift)
  
  fo2=matrix(0,dim(finaloutput$width)[1],length(ind))
  flo2=array(0,dim=c(dim(finaloutput$width)[1],length(ind),3))
  medianwidth=apply(finaloutput$width,2,median)[ind]
  for (ii in seq_along(ind)) {
    print(ii)
    
    nanana=tryCatch({lmrob(as.numeric(finaloutput$width[ii,ind]) ~ medianwidth,control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(as.numeric(finaloutput$width[ii,ind]) ~ medianwidth)},warning= function(e) {lm(as.numeric(finaloutput$width[ii,ind]) ~ medianwidth)}) 
    tro=suppressWarnings(predict(nanana, interval='prediction'))
    flo2[ii,,]=tro
    fo2[ii,which(finaloutput$width[ii,ind]<tro[,2]|finaloutput$width[ii,ind]>tro[,3])]=1
  }
  colnames(fo2)=colnames(finaloutput$shift)[ind]
  rownames(fo2)=rownames(finaloutput$shift)
  
  
#Analysis of which samples have too much fitting error
fitting_error_alarmmatrix=matrix(0,dim(finaloutput$correlation)[1],dim(finaloutput$correlation[,ind])[2])
fitting_error_alarmmatrix[finaloutput$correlation[,ind]<program_parameters$fitting_error_limit]=1


#Analysis of which samples have too scant signal respective to the total area where the signal is located
signal_area_ratio_alarmmatrix=matrix(0,dim(finaloutput$signal_area_ratio)[1],dim(finaloutput$signal_area_ratio[,ind])[2])
signal_area_ratio_alarmmatrix[finaloutput$signal_area_ratio[,ind]>program_parameters$signal_area_ratio_limit]=1

alarmmatrix=fo+fo2+signal_area_ratio_alarmmatrix+fitting_error_alarmmatrix
colnames(alarmmatrix)=colnames(fo)
rownames(alarmmatrix)=rownames(fo)

#Analysis of which metabolites have have relative intensity too far from expected according to robust linear model constructed

#Find which metabolites have more than one signal and analyzing case by case
# ind=grep('_s1',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s1',colnames(finaloutput$intensity))])]
# ind2=grep('_s2',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s2',colnames(finaloutput$intensity))])]
# # 
# # #I find how many samples will be used for the roubst linear model in the same way than with the shift
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
#   intensity_suspicioussamples=which(abs(finaloutput$intensity[,ind[ii]]-intensity_prediction)>program_parameters$rlm_limit*rlm_model$scale)
#   intensity_alarmmatrix[intensity_suspicioussamples,c(ind[ii],ind2[ii])]=1
# 
# 
# }


#I sum all "points" gained by every quantification
# alarmmatrix=shift_alarmmatrix+signal_area_ratio_alarmmatrix+fitting_error_alarmmatrix+intensity_alarmmatrix
validationdata=list(alarmmmatrix=alarmmatrix,flo=flo,flo2=flo2)

return(validationdata)
}