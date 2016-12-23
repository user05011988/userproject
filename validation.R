validation = function(finaloutput,
                                 other_fit_parameters,validation_type,profile_folder_path,metadata) {

  #Created by Daniel Ca?ueto 13/09/2016
  #Finding of suspicious quantifications through difference with predicted shift, signal to total area ratio, fitting error, and difference with expected relative intensity
  
  #TO DO: maybe would be better to analyze official relative intensity. 
  #TO DO: allow more than two signals when analyzing official relative intensity. 
  print(validation_type)
  
  
  
  #shift analysis
  #correlation matrix of shift of signals and finding for every signal the signal that has best ability to predict shift
  #Analysis of which samples have too much fitting error
  if (validation_type==1) {
  alarmmatrix=finaloutput$fitting_error
  brks <- quantile(alarmmatrix, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
} else if (validation_type==2) { 
  alarmmatrix=finaloutput$signal_area_ratio
  brks <- quantile(alarmmatrix, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
} else if (validation_type==3) { 
  ind=which(apply(finaloutput$shift,2, function(x) all(is.na(x)))==F)
  shift_corrmatrix=cor(finaloutput$shift,use='pairwise.complete.obs',method='spearman')
  alarmmatrix=matrix(NA,dim(finaloutput$shift)[1],dim(finaloutput$shift)[2])
  colnames(alarmmatrix)=colnames(finaloutput$shift)
  rownames(alarmmatrix)=rownames(finaloutput$shift)
  for (i in ind) {
    ll=finaloutput$shift[,ind[sort(abs(shift_corrmatrix[,i]),decreasing=T,index.return=T)$ix][1:3]]
   j=is.na(rowMeans(ll))
     nanana=tryCatch({lmrob(ll[!j,1] ~ ll[!j,2],control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(ll[!j,1] ~ ll[!j,2])},warning= function(e) {lm(ll[!j,1] ~ ll[!j,2])})
    tro1=suppressWarnings(predict(nanana, interval='prediction'))
    nanana=tryCatch({lmrob(ll[!j,1] ~ ll[!j,3],control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(ll[!j,1] ~ ll[!j,3])},warning= function(e) {lm(ll[!j,1] ~ ll[!j,3])})    
    tro2=suppressWarnings(predict(nanana, interval='prediction'))
  alarmmatrix[!j,i]=apply(rbind(finaloutput$shift[!j,i]-tro1[,1],finaloutput$shift[!j,i]-tro2[,1]),2,min)}

  brks <- quantile(alarmmatrix, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(c(seq(40, 255, length.out = (length(brks) + 1)/2),seq(255, 40, length.out = (length(brks) + 1)/2)), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  } else if (validation_type==4) { 
    ind=which(apply(finaloutput$width,2, function(x) all(is.na(x)))==F)
 alarmmatrix=matrix(NA,dim(finaloutput$width)[1],dim(finaloutput$width)[2])
    colnames(alarmmatrix)=colnames(finaloutput$width)
    rownames(alarmmatrix)=rownames(finaloutput$width)
  medianwidth=apply(finaloutput$width,2,function(x)median(x,na.rm=T))
  for (i in 1:dim(finaloutput$width)[1]) {
    nanana=tryCatch({lmrob(as.numeric(finaloutput$width[i,]) ~ medianwidth,control = lmrob.control(maxit.scale=5000))},error= function(e) {lm(as.numeric(finaloutput$width[i,]) ~ medianwidth)},warning= function(e) {lm(as.numeric(finaloutput$width[i,]) ~ medianwidth)}) 
    tro=suppressWarnings(predict(nanana, interval='prediction'))
    alarmmatrix[i,ind][!is.na(finaloutput$width[i,ind])]=finaloutput$width[i,ind][!is.na(finaloutput$width[i,ind])]-tro[,1]
  }
  brks <- quantile(abs(alarmmatrix), probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(c(seq(40, 255, length.out = (length(brks) + 1)/2),seq(255, 40, length.out = (length(brks) + 1)/2)), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  } else if (validation_type==5) { 
    
  
  alarmmatrix=matrix(NA,dim(finaloutput$Area)[1],dim(finaloutput$Area)[2])
  ss=unique(metadata[,2])
  print(ss)
  alarmmatrix=matrix(NA,dim(finaloutput$width)[1],dim(finaloutput$width)[2])
  colnames(alarmmatrix)=colnames(finaloutput$Area)
  rownames(alarmmatrix)=rownames(finaloutput$Area)
  for (k in 1:length(ss)) {
    print(metadata[,2]==ss[k])
    a=apply(finaloutput$Area[metadata[,2]==ss[k],],2,function(x) IQR(x,na.rm=T))
    b=rbind(apply(finaloutput$Area[metadata[,2]==ss[k],],2,function(x) quantile(x,0.25,na.rm=T)),apply(finaloutput$Area[metadata[,2]==ss[k],],2,function(x) quantile(x,0.75,na.rm=T)))
    for (i in which(metadata[,2]==ss[k])) {
      for (j in which(!is.na(a))) {
        if (!is.na(finaloutput$Area[i,j]) && finaloutput$Area[i,j]>b[1,j]&&finaloutput$Area[i,j]<b[2,j]) {
          alarmmatrix[i,j]=0
        } else if (!is.na(finaloutput$Area[i,j]) &&finaloutput$Area[i,j]<b[1,j]) {
          alarmmatrix[i,j]=abs(finaloutput$Area[i,j]-b[1,j])/a[j]
        } else if (!is.na(finaloutput$Area[i,j]) &&finaloutput$Area[i,j]>b[2,j]) {
          alarmmatrix[i,j]=abs(finaloutput$Area[i,j]-b[2,j])/a[j]
        }
      }
    }}
  brks <- quantile(alarmmatrix, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  } else if (validation_type==6) { 
    
    relative_intensity = read.csv(profile_folder_path, stringsAsFactors = F)
    
    alarmmatrix=matrix(NA,dim(finaloutput$intensity)[1],dim(finaloutput$intensity)[2])
    
    alarmmatrix=matrix(0,dim(finaloutput$intensity)[1],dim(finaloutput$intensity)[2])
    colnames(alarmmatrix)=colnames(finaloutput$intensity)
    rownames(alarmmatrix)=rownames(finaloutput$intensity)
    ma=relative_intensity[which(relative_intensity[,7]>0),c(4,7)]
    # for (i in 1:dim(finaloutput$intensity)[2]) {
    # ma[[length(ma)+1]]=
    CV <- function(x){
      (sd(x)/mean(x))
    }
    
    ss=which(ma[,2]==2)
    for (i in ss) {
      b=which(ma[,1]==ma[i,1])
      ccv=relative_intensity[which(relative_intensity[,7]>0)[b],12]
      ccvv=finaloutput$intensity[,b]
      ccvvv=finaloutput$fitting_error[,b]
      for (j in 1:nrow(ccvvv)) {
        aa=ccvv[j,]*ccv[which.min(ccvvv[j,])]/ccvv[j,which.min(ccvvv[j,])] - ccv
        alarmmatrix[j,b]=aa
      }
    }
    nn=rep(NA,19)
    nn[10]=0.5
    aa=quantile(alarmmatrix, probs = seq(0, 1, length.out=1001))
    nn[11:19]=seq(which(aa>0)[1]-1,1000,length.out = 9)/1000
    nn[1:9]=seq(0,which(aa<0)[length(which(aa<0))]-1,length.out = 9)/1000
    brks <- quantile(alarmmatrix, probs = nn, na.rm = TRUE)
    clrs <- round(c(seq(40, 255, length.out = (length(brks) + 1)/2),seq(255, 40, length.out = (length(brks) + 1)/2)), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
  }
  



#Analysis of which metabolites have have relative intensity too far from expected according to robust linear model constructed

#Find which metabolites have more than one signal and analyzing case by case
# ind=grep('_s1',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s1',colnames(finaloutput$intensity))])]
# ind2=grep('_s2',colnames(finaloutput$intensity))[order(colnames(finaloutput$intensity)[grep('_s2',colnames(finaloutput$intensity))])]
# # 
# # #I find how many samples will be used for the roubst linear model in the same way than with the shift
# intensity_alarmmatrix=matrix(0,dim(finaloutput$intensity)[1],dim(finaloutput$intensity)[2])
# for (i in 1:length(ind)) {
#   rlm_preinfo=matrix(NA,dim(finaloutput$shift)[1],2)
#   rlm_samples=cbind(finaloutput$intensity[,ind[i]],finaloutput$intensity[,ind2[i]])
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
#   intensity_prediction=as.numeric(rlm_model$coefficients[1])+as.numeric(rlm_model$coefficients[2])*finaloutput$intensity[,ind2[i]]
# 
#   intensity_suspicioussamples=which(abs(finaloutput$intensity[,ind[i]]-intensity_prediction)>other_fit_parameters$rlm_limit*rlm_model$scale)
#   intensity_alarmmatrix[intensity_suspicioussamples,c(ind[i],ind2[i])]=1
# 
# 
# }


#I sum all "points" gained by every quantification
# alarmmatrix=shift_alarmmatrix+signal_area_ratio_alarmmatrix+alarmmatrix+intensity_alarmmatrix
validationdata=list(alarmmatrix=alarmmatrix,brks=brks,clrs=clrs)

return(validationdata)
}