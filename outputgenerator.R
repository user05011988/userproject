output_generator = function(signals_to_quantify,fitted_signals,Ydata,Xdata,signals_parameters) {
  
  output_data=list()

    # output_data$features[,1]=Code[signals_to_quantify]
    # output_data$features[,2]=signals_to_quantify
    # output_data$features[,3]=multiplicities[signals_to_quantify]
    # # output_data$features[,4:8]=x[1:5,signals_to_quantify]
    # output_data$features[,9]=length(Ydata)
    # output_data$features[,12]=roof_effect[signals_to_quantify]
  BGsignals=(signals_parameters[6,]==100)  

  output_data$Area=rowSums(fitted_signals[signals_to_quantify,,drop=F])
output_data$shift=signals_parameters[signals_to_quantify,2,drop=F]
output_data$quantified_signals=fitted_signals[signals_to_quantify,,drop=F]
# if (is.null(dim(output_data$quantified_signals)) output_data$quantified_signals=t(output_data$quantified_signals)
output_data$signals_sum=colSums(fitted_signals[!BGsignals,,drop=F],na.rm=T)
output_data$baseline_sum=colSums(fitted_signals[BGsignals,,drop=F],na.rm=T)
output_data$fitted_sum=output_data$signals_sum+output_data$baseline_sum
# dim(signals_parameters)=c(7,length(signals_parameters)/7)


# Calcul dels nous errors
# A_spectra=c()
# A_signals=c()


  for (i2 in signals_to_quantify) {
    
      cumulative_area=cumsum(fitted_signals[i2,])/sum(fitted_signals[i2,])
      
      subregion_leftlimit=ifelse(is.nan(sum(cumulative_area)),1,which.min(abs(cumulative_area - 0.05)))
      subregion_rightlimit=ifelse(is.nan(sum(cumulative_area)),length(Xdata),which.min(abs(cumulative_area - 0.95)))
      # X_signals=Xdata[subregion_leftlimit:subregion_rightlimit]
      subregion_spectrum=Ydata[subregion_leftlimit:subregion_rightlimit]
      subregion_signals=fitted_signals[i2,subregion_leftlimit:subregion_rightlimit]
      subregion_fitted=output_data$fitted_sum[subregion_leftlimit:subregion_rightlimit]
      # Y_baseline=baseline_sum[subregion_leftlimit:subregion_rightlimit]
      # A_spectra=append(A_spectra,sum(subregion_spectrum))
      # A_signals=append(A_signals,sum(subregion_signals))
      # A_total[i2]=sum(subregion_fitted)
      output_data$signal_area_ratio=append(output_data$signal_area_ratio,100-((abs(sum(subregion_spectrum)-sum(subregion_signals))/sum(subregion_spectrum))*100))
      output_data$fitting_error=append(output_data$fitting_error,mean(((subregion_spectrum-subregion_fitted)^2)/subregion_spectrum^2)*100)
  }

#Fi del calcul dels nous errors
return(output_data)
}
