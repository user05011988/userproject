automatic_roi_edition = function(dataset,
                                 import_excel_profile,
                                 Xdata,
                                 other_fit_parameters,ROI_limits,ppm) {

oldshift=import_excel_profile[, 5]
#Change of shift
#I find the spectrum with ROI more similar to the ROI of 75% quartile spectrum of the dataset. I choose 75 % quartile spectrum because median can give problems for signals wit hhigh variability
quartile_spectrum = as.numeric(apply(dataset[, other_fit_parameters$ROI_buckets,drop=F], 2, function(x)
  quantile(x, 0.75)))
reference_spectrum_ind = which.min(apply(dataset[, other_fit_parameters$ROI_buckets,drop=F], 1, function(x)
  sqrt(mean((x - quartile_spectrum) ^ 2
  ))))

#I localize the peaks of this reference ROI
peak_info = peakdet(dataset[reference_spectrum_ind, other_fit_parameters$ROI_buckets,drop=F], 0.01)
#I sum expected peaks from ROI info
peak_sum = sum(import_excel_profile[, 8])
#I localize location of most relevant peaks in refernce ROI
sorted_peaks = sort(peak_info$maxtab$val,
                    index.return = T,
                    decreasing = T)$ix[1:peak_sum]
reference_peaks_location = Xdata[peak_info$maxtab$pos[sorted_peaks]]
reference_peaks_int = peak_info$maxtab$val[sorted_peaks]





#Depending on the kind of signal in the ROI info I find if the shift of the signal in the reference spectrum is close enough to the shift specified in ROI info. If true, then I change the shift info.
for (i in 1:dim(import_excel_profile)[1]) {
  if (import_excel_profile[i, 8] == 1) {
    #singlet
    ROI_shift_location = Xdata[which.min(abs(import_excel_profile[i, 5] -
                                               Xdata))]
    reference_peak_ind = which.min(abs(reference_peaks_location - ROI_shift_location))
    if (abs(round(reference_peaks_location[reference_peak_ind] - ROI_shift_location, 3)) <=
        round(other_fit_parameters$automatic_shift_distance, 3))
      import_excel_profile[i, 5] = reference_peaks_location[reference_peak_ind]

    
    
  } else if (import_excel_profile[i, 8] == 2) {
    #doublet
    ROI_shift_location = c(Xdata[which.min(abs((
      import_excel_profile[i, 5] - import_excel_profile[i, 9] / (2 * other_fit_parameters$freq)
    ) - Xdata
    ))],
    Xdata[which.min(abs((
      import_excel_profile[i, 5] + import_excel_profile[i, 9] / (2 * other_fit_parameters$freq)
    ) - Xdata
    ))])
    reference_peak_ind = rep(NA, 2)
    for (j in 1:2)
      reference_peak_ind[j] = which.min(abs(reference_peaks_location - ROI_shift_location[j]))
    if (all(abs(round(
      sort(reference_peaks_location[reference_peak_ind]) - sort(ROI_shift_location),
      3
    )) <= round(other_fit_parameters$automatic_shift_distance, 3)))
      import_excel_profile[i, 5] = mean(reference_peaks_location[reference_peak_ind])
  } else if (import_excel_profile[i, 8] == 3) {
    #triplet
    ROI_shift_location = c(Xdata[which.min(abs((
      import_excel_profile[i, 5] - import_excel_profile[i, 9] / other_fit_parameters$freq
    ) - Xdata
    ))],
    Xdata[which.min(abs(import_excel_profile[i, 5] - Xdata))],
    Xdata[which.min(abs((
      import_excel_profile[i, 5] + import_excel_profile[i, 9] / other_fit_parameters$freq
    ) - Xdata
    ))])
    minimizer = rep(NA, (length(reference_peaks_location)-2))
    for (j in 1:(length(reference_peaks_location)-2))
      # reference_peak_ind[j] = which.min(abs(reference_peaks_location - ROI_shift_location[j]))
      minimizer[j]=mean(abs(sort(ROI_shift_location)-sort(reference_peaks_location)[j:(j+2)]))
    
    if (min(minimizer) <= round(other_fit_parameters$automatic_shift_distance, 3))
      import_excel_profile[i, 5] = mean(sort(reference_peaks_location)[which.min(minimizer):(which.min(minimizer)+2)])
    reference_peaks_int=reference_peaks_int[sort(reference_peaks_location,index.return=T)$ix]
    reference_peak_ind=which.min(minimizer):(which.min(minimizer)+2)
    reference_peaks_location=sort(reference_peaks_location)
    }
  
  # gg=seq(min(Xdata),max(Xdata),0.0001)
  # 
  # fa=approx(Xdata,dataset[reference_spectrum_ind, other_fit_parameters$ROI_buckets,drop=F],gg)
  # sdf=which.max(reference_peaks_int[reference_peak_ind])
  # dd_ind=which(round(fa$x,4)==reference_peaks_location[sdf])
  # aa=which(fa$y>0.5*reference_peaks_int[sdf])
  # bb=which(aa==dd_ind)
  # cc=c(1,which(diff(aa)>1),length(aa))
  # ccc=sort(abs(cc-bb),index.return=T)$ix
  # cccc=sort(cc[ccc[1:2]])
  # cccc[1]=cccc[1]+1
  # import_excel_profile[i, 6]=other_fit_parameters$freq*(fa$x[aa[cccc[2]]]-fa$x[aa[cccc[1]]])/2
  # tra=peakdet(fa$y,0.01)
  
}

ROI_limits=round(ROI_limits-round(mean(oldshift-import_excel_profile[, 5]),3),3)

#Change of shift tolerance
#Calculation of lag of every ROI from a median ROI, looking to the lag where there is more crosscorrelation.
spectra_lag = matrix(NA, dim(import_excel_profile)[1],dim(dataset[, other_fit_parameters$ROI_buckets,drop=F])[1])
for (i in 1:dim(dataset[, other_fit_parameters$ROI_buckets,drop=F])[1]) {
  for (j in 1:dim(import_excel_profile)[1]) {
    ll=which(ppm>  (import_excel_profile[j, 5]-0.02) & ppm <  (import_excel_profile[j, 5] +0.02))
    d <-
      ccf(dataset[i, ll],
          apply(dataset[, ll,drop=F], 2, median),
          type = 'covariance',
          plot = FALSE)
    spectra_lag[j,i] = d$lag[which.max(d$acf)]
  }
  # d <-
  #   ccf(dataset[i, ll],
  #       apply(dataset[, ll,drop=F], 2, median),
  #       type = 'covariance',
  #       plot = FALSE)
  # spectra_lag[j,i] = d$lag[which.max(d$acf)]
  # }
  import_excel_profile[j, 11] = (max(abs(spectra_lag[j,])) * other_fit_parameters$buck_step) +
    0.002
  # }
}
#From the optimum lag for every spectrum I remove outliers probably produced by spurious peaks
# spectra_lag = spectra_lag[!spectra_lag %in% boxplot.stats(spectra_lag)$out]
#I change the shift tolerance, adding 0.001 to be a little more sure I have not been too strict


dummy=list(import_excel_profile=import_excel_profile,ROI_limits=ROI_limits)
return(dummy)
}