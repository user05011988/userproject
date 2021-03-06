output_generator = function(signals_to_quantify,
                            fitted_signals,
                            Ydata,
                            Xdata,
                            signals_parameters,multiplicities,ROI_buckets) {

  #Created by Daniel Cañueto 30/08/2016
  #Creation of quantification variables (Area, correlation,shift,signal_area_ratio)
  #and of necessary variables to make a plot of the quantification
  fitted_signals[is.na(fitted_signals)]=0

  BGsignals = (multiplicities == 0) #finds background signals

  output_data = list()

  output_data$signals = fitted_signals[!BGsignals, , drop =
                                                    F]
  output_data$signals_sum = colSums(fitted_signals[!BGsignals, , drop =
                                                     F], na.rm = T)
  output_data$baseline_sum = colSums(fitted_signals[BGsignals, , drop =
                                                      F], na.rm = T)
  output_data$fitted_sum = output_data$signals_sum + output_data$baseline_sum

  for (i2 in signals_to_quantify) {
    #correlation and signal_area_ratio are calculated from the subregion of the ROI
    # where 90% of the area of the signal is located, calculated throgh cumulative sum.
    # See documentation for details about meaning and calculation of these two parameters

    cumulative_area = cumsum(fitted_signals[i2, ]) / sum(fitted_signals[i2, ])
    subregion_leftlimit = ifelse(is.nan(sum(cumulative_area)), 1, which.min(abs(cumulative_area - 0.05)))
    subregion_rightlimit = ifelse(is.nan(sum(cumulative_area)), length(Xdata), which.min(abs(cumulative_area - 0.95)))
    # print(subregion_leftlimit)
    # print(subregion_rightlimit)
    subregion_spectrum = Ydata[subregion_leftlimit:subregion_rightlimit]
    subregion_signals = fitted_signals[i2, subregion_leftlimit:subregion_rightlimit]
    subregion_fitted = output_data$fitted_sum[subregion_leftlimit:subregion_rightlimit]

    output_data$signal_area_ratio = append(output_data$signal_area_ratio, 100 -
                                             ((
                                               abs(sum(subregion_spectrum) - sum(subregion_signals)) / sum(subregion_spectrum)
                                             ) * 100))
    # aa=cor(subregion_spectrum, subregion_fitted)
    aa=summary(lm(subregion_spectrum~subregion_fitted))$sigma/max(subregion_spectrum)
    
    # if (is.na(aa)) aa=0
    output_data$correlation = append(output_data$correlation,aa
                                       # mean(((subregion_spectrum - subregion_fitted) ^ 2
                                       # ) / subregion_spectrum ^ 2) * 100)
      )
  }
  cumulative_area = cumsum(output_data$fitted_sum) / sum(output_data$fitted_sum)
  subregion_leftlimit = ifelse(is.nan(sum(cumulative_area)), 1, which.min(abs(cumulative_area - 0.05)))
  subregion_rightlimit = ifelse(is.nan(sum(cumulative_area)), length(Xdata), which.min(abs(cumulative_area - 0.95)))
  subregion_spectrum = Ydata[subregion_leftlimit:subregion_rightlimit]
  subregion_fitted = output_data$fitted_sum[subregion_leftlimit:subregion_rightlimit]
  error1=summary(lm(subregion_spectrum~subregion_fitted))$sigma/max(subregion_spectrum)
  print(error1)
  output_data$Area = rowSums(fitted_signals[signals_to_quantify, , drop =
                                              F])
  output_data$shift = signals_parameters[2, signals_to_quantify]
  dummy=list(output_data=output_data,error1=error1)
  return(dummy)
}
