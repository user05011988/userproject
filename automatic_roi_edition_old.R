automatic_roi_edition = function(dataset,
                                 import_excel_profile,
                                 Xdata,
                                 other_fit_parameters,ROI_limits) {
  #Created by Daniel Cañueto 13/09/2016
  #Automatic ROI edition of shift and shift tolerance.If it fails the user should investigate through the interface the best combination.
  
  #TO DO: find a way to edit the width automatically
  
  
  oldshift=import_excel_profile[, 5]
  #Change of shift
  #I find the spectrum with ROI more similar to the ROI of 75% quartile spectrum of the dataset. I choose 75 % quartile spectrum because median can give problems for signals wit hhigh variability
  quartile_spectrum = as.numeric(apply(dataset[, other_fit_parameters$ROI_buckets], 2, function(x)
    quantile(x, 0.75)))
  reference_spectrum_ind = which.min(apply(dataset[, other_fit_parameters$ROI_buckets], 1, function(x)
    sqrt(mean((x - quartile_spectrum) ^ 2
    ))))
  
  #I localize the peaks of this reference ROI
  peak_info = peakdet(dataset[reference_spectrum_ind, other_fit_parameters$ROI_buckets], 0.01)
  #I sum expected peaks from ROI info
  peak_sum = sum(import_excel_profile[, 8])
  #I localize location of most relevant peaks in refernce ROI
  sorted_peaks = sort(peak_info$maxtab$val,
                      index.return = T,
                      decreasing = T)$ix[1:peak_sum]
  reference_peaks_location = Xdata[peak_info$maxtab$pos[sorted_peaks]]
  

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
      reference_peak_ind = rep(NA, 3)
      for (j in 1:3)
        reference_peak_ind[j] = which.min(abs(reference_peaks_location - ROI_shift_location[j]))
      if (all(abs(round((
        sort(reference_peaks_location) - sort(ROI_shift_location)
      ), 3)) <= round(other_fit_parameters$automatic_shift_distance, 3)))
        import_excel_profile[i, 5] = mean(reference_peaks_location[reference_peak_ind])
    }
  }
  
  ROI_limits=round(ROI_limits-mean(oldshift-import_excel_profile[, 5]),3)
  
  #Change of shift tolerance
  #Calculation of lag of every ROI from a median ROI, looking to the lag where there is more crosscorrelation.
  spectra_lag = rep(0, dim(dataset[, other_fit_parameters$ROI_buckets])[1])
  for (i in 1:dim(dataset[, other_fit_parameters$ROI_buckets])[1]) {
    d <-
      ccf(dataset[i, other_fit_parameters$ROI_buckets],
          apply(dataset[, other_fit_parameters$ROI_buckets], 2, median),
          type = 'covariance',
          plot = FALSE)
    spectra_lag[i] = d$lag[which.max(d$acf)]
  }
  #From the optimum lag for every spectrum I remove outliers probably produced by spurious peaks
  spectra_lag = spectra_lag[!spectra_lag %in% boxplot.stats(spectra_lag)$out]
  #I change the shift tolerance, adding 0.001 to be a little more sure I have not been too strict
  import_excel_profile[, 11] = (max(abs(spectra_lag)) * other_fit_parameters$buck_step) +
    0.001
  
  dummy=list(import_excel_profile=import_excel_profile,ROI_limits=ROI_limits)
  return(dummy)
}