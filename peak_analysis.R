peak_analysis=function(dataset,ppm,export_path,metadata) {
  print('Processing. Talk some gossip, meanwhile.')
  
  if (ppm[1]<ppm[2]) ppm=rev(ppm)
  setupRSPA(-ppm)

refdataset<-dataset[suppressWarnings(selectRefSp(dataset,recursion$step)),]
# refSegments<- segmentateSp(refdataset, peakParam)
refdataset<-apply(dataset,2,median)


peak_ppm=c()
for (i in 1:length(refSegments$Peaks)) peak_ppm=c(peak_ppm,ppm[unlist(refSegments$Peaks[[i]][1])])

peak_info=matrix(NA,0,11)
for (i in 1:length(refSegments$Peaks)) peak_info=rbind(peak_info,refSegments$Peaks[[i]])


peak_shape_corr=matrix(NA,dim(dataset)[1],dim(peak_info)[1])
for (j in 1:dim(peak_info)[1]) {
  med=try(apply(dataset[,peak_info[j,10]:peak_info[j,11],drop=F],2,median))
  med2=try(dataset[,peak_info[j,10]:peak_info[j,11]])
  peak_shape_corr[,j]=cor(t(med2),med)
}
peak_shape_corr[is.na(peak_shape_corr)]=0
colnames(peak_shape_corr)=peak_ppm

valid_peak_ind=apply(peak_shape_corr,2,function(x)quantile(x,0.25))
peak_info=peak_info[valid_peak_ind>0.8,]

peak_quantification=matrix(NA,dim(dataset)[1],dim(peak_info)[1])
colnames(peak_quantification)=peak_ppm[valid_peak_ind>0.8]
for (i in 1:dim(dataset)[1]) {
  for (j in 1:dim(peak_info)[1]) {
    peak_quantification[i,j]=sum(dataset[i,peak_info[j,10]:peak_info[j,11]])-peak_info[j,9]*length(dataset[i,peak_info[j,10]:peak_info[j,11]])
    
  }}

peak_quantification[peak_shape_corr[,valid_peak_ind>0.8]<0.8]=NA

corr_matrix=round(cor(peak_quantification,use='pairwise.complete.obs',method='spearman'),2)

threshold_corr_matrix=corr_matrix
threshold_corr_matrix[threshold_corr_matrix<0.8]=0

correlated_signals_indexes=threshold_corr_matrix[which(colSums(threshold_corr_matrix)>1),which(colSums(threshold_corr_matrix)>1)]
secure_multiplets=list()
k=1
for (i in 1:dim(correlated_signals_indexes)[1]) {
  
  corr_ind=which(correlated_signals_indexes[i,]>0.75)
  cc=c()
  
  for ( j in 1:length(corr_ind)) {
    corr_ind2=which(correlated_signals_indexes[corr_ind[j],]>=0.75)
    if (identical(corr_ind2,corr_ind)) {
      cc=c(cc,corr_ind[j])   
    } else {
      cc=0
      break
    }
  }
  
  if (cc!=0) {
    secure_multiplets[[k]]=corr_ind
    k=k+1
  }
  
}
secure_multiplets=unique(secure_multiplets)
ab2=unlist(unique(secure_multiplets))
for (i in 1:length(secure_multiplets)) {
  secure_multiplets[[i]]=which(colnames(threshold_corr_matrix) %in% names(secure_multiplets[[i]])==T)
  names(secure_multiplets[[i]])=colnames(threshold_corr_matrix)[secure_multiplets[[i]]]
}

clustering_corr_matrix=corr_matrix[which(colSums(threshold_corr_matrix)>1),which(colSums(threshold_corr_matrix)>1)]
clustering_corr_matrix=clustering_corr_matrix[-ab2,-ab2]
clustering_quality_indicator=matrix(NA,3,9)
for (i in seq(0.1,0.9,0.1)) {
  for (j in 1:3) {
    clustering_quality_indicator[j,i*10]=apcluster(negDistMat(r=j), clustering_corr_matrix,q=i)@netsim
  }}
dummy=apply(clustering_quality_indicator,1,function(x) diff(x)/mean(diff(x)))
optimal_clustering_values=(which(dummy==max(dummy), arr.ind = TRUE) +c(1,0))/c(10,1)
q_clustering_value=which.max(diff(clustering_quality_indicator))+1
signals_clusters <- apcluster(negDistMat(r=optimal_clustering_values[2]), clustering_corr_matrix,q=optimal_clustering_values[1])@clusters

for (i in 1:length(signals_clusters)) {
  signals_clusters[[i]]=which(colnames(threshold_corr_matrix) %in% names(signals_clusters[[i]])==T)
  names(signals_clusters[[i]])=colnames(threshold_corr_matrix)[signals_clusters[[i]]]
}

signals_list=unique(append(secure_multiplets,signals_clusters))
for (i in 1:length(which(colSums(threshold_corr_matrix)<=1)))  signals_list[[length(signals_list)+1]]=which(colSums(threshold_corr_matrix)<=1)[i]

peak_quantification_median=apply(peak_quantification,2,median)
ROI_profile_suggestion=matrix(NA,0,9)
for (i in 1:length(signals_list)) {
  signals_patterns=cbind(1:length(signals_list[[i]]),1:length(signals_list[[i]]))
  signals_intensities=c()
  for (k in 1:dim(signals_patterns)[1]) signals_intensities=c(signals_intensities,mean(peak_quantification_median[signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]]))
  for (k in 1:dim(signals_patterns)[1]) {
    # ROI_profile_suggestion=rbind(ROI_profile_suggestion,c(paste(i,k,sep='_'),mean(as.numeric(names(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]))),1,1,length(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]),abs((as.numeric(names(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]))[2]-as.numeric(names(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]))[1])*600.2),0,0.001,signals_intensities[k]/max(signals_intensities)))
    ROI_profile_suggestion=rbind(ROI_profile_suggestion,c(paste(i,k,sep='_'),mean(as.numeric(names(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]))),1,1,length(signals_list[[i]][signals_patterns[k,1]:signals_patterns[k,2]]),0,0,0.001,signals_intensities[k]/max(signals_intensities)))
    
  }
}



ROI_profile_suggestion[is.na(ROI_profile_suggestion)]=0 
ROI_profile_suggestion=as.data.frame(ROI_profile_suggestion[sort(ROI_profile_suggestion[,2],index.return=T)$ix,],stringsAsFactors = F)
ROI_profile_suggestion[,-1]=lapply(ROI_profile_suggestion[,-1],function(x) as.numeric(x))

signals_diff=c(0,which(diff(ROI_profile_suggestion[,2])>0.04),dim(ROI_profile_suggestion)[1])
ROI_limits_suggestion=matrix(NA,dim(ROI_profile_suggestion)[1],2)
for (i in 1:dim(ROI_profile_suggestion)[1]) {
  relevant_signals_diff=which(signals_diff %in% i ==T)
  if (length(relevant_signals_diff)>0) {
    ROI_limits_suggestion[(signals_diff[relevant_signals_diff-1]+1):signals_diff[relevant_signals_diff],]=matrix(rep(c(ROI_profile_suggestion[(signals_diff[relevant_signals_diff-1]+1),2]-0.02,ROI_profile_suggestion[signals_diff[relevant_signals_diff],2]+0.02),2),length((signals_diff[relevant_signals_diff-1]+1):signals_diff[relevant_signals_diff]),2,byrow=T)
  }
}
p_value_final=p_values(peak_quantification,metadata)
p_value_final=p_value_final[sort(as.numeric(names(p_value_final)),index.return=T)$ix]

ROI_profile_suggestion=cbind(round(ROI_limits_suggestion,3),rep('Baseline Fitting',dim(ROI_profile_suggestion)[1]),ROI_profile_suggestion,as.vector(p_value_final))
colnames(ROI_profile_suggestion)=c('ROI_left','ROI_right','Q.Mode','Signal,Position..ppm.','Width','Q.Signal','Multiplicity','J.coupling..Hz.','Roof.effect','Shift.tolerance','Intensity','p_value')

write.csv(ROI_profile_suggestion,paste(export_path,'ROI_profile_suggestion.csv',sep='/'),row.names = F)
write.csv(peak_quantification,paste(export_path,'peak_quantification.csv',sep='/'),row.names = F)
print('Done! Look on your export folder, you should have a new csv with ROI profiles suggestions and a new CSV with quantifications of integrated peaks.')

}