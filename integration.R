integration = function(integration_parameters, Xdata, Ydata,Ydatamedian) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Integration of ROI


  #preallocation of results_to_save
  results_to_save = list(
    signal_area_ratio = NA,
    fitting_error = NA,
    Area = NA,
    shift = NA,
    intensity = NA
  )

  #preparation of baseline, if specified by the user
  baseline = replicate(length(Xdata), 0)
  if (integration_parameters$clean_fit == 'N')
    baseline = seq(mean(Ydata[1:3]), mean(Ydata[(length(Xdata) - 2):length(Xdata)]), length =
                     length(Xdata))

  #integration ad chechk that there are no negative values
  integrated_signal = Ydata - baseline
    integrated_signal[integrated_signal<0]=0
  #preparation of results_to_save
  results_to_save$Area = sum(integrated_signal)
  results_to_save$intensity = max(integrated_signal)

  cumulative_area = cumsum(integrated_signal) / sum(integrated_signal)


  # p1 = which.min(abs(cumulative_area - 0.05))
  # p2 = which.min(abs(cumulative_area - 0.95))
  if (all(is.na(cumulative_area))) {
    p1=1
    p2=length(cumulative_area)
  }
  p1 = which(cumulative_area< 0.05)[length(which(cumulative_area< 0.05))]
  p2 = which(cumulative_area > 0.95)[1]

  results_to_save$signal_area_ratio = tryCatch((sum(integrated_signal[p1:p2]) / sum(Ydata[p1:p2])) *
    100,error = function(e) NaN, silent=T)
  print(results_to_save$signal_area_ratio)
  results_to_save$fitting_error = 1-cor(Ydata,Ydatamedian,method='spearman')
  results_to_save$half_band_width = NaN

  peaks = peakdet(integrated_signal, 0.2*max(0.000001,max(integrated_signal)))
  results_to_save$shift = mean(Xdata[peaks$maxtab$pos])

  plot_data=rbind(integrated_signal,baseline,integrated_signal+baseline,integrated_signal)
  
  dummy=list(results_to_save=results_to_save,plot_data=plot_data)
  return(dummy)
}
