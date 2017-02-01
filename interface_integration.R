interface_integration = function(integration_parameters, Xdata, Ydata) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Integration of ROI


  #preallocation of results_to_save
  results_to_save = list(
    signal_area_ratio = NA,
    correlation = NA,
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
  p1 = which(cumulative_area< 0.05)[length(which(cumulative_area< 0.05))]
  p2 = which(cumulative_area > 0.95)[1]
  results_to_save$signal_area_ratio = tryCatch((sum(integrated_signal[p1:p2]) / sum(Ydata[p1:p2])) *
      100,error = function(e) NaN, silent=T)
  print(results_to_save$signal_area_ratio)
  results_to_save$correlation = NaN
  results_to_save$width = NaN

  peaks = peakdet(integrated_signal, 0.2*max(1e-10,max(integrated_signal)))
  results_to_save$shift = mean(Xdata[peaks$maxtab$pos])

  #plot
  plotdata = data.frame(Xdata, signal = integrated_signal)
  plotdata2 = data.frame(Xdata, Ydata)
  plotdata3 <- melt(plotdata2, id = "Xdata")
  plotdata3$variable = rep('Original Spectrum', length(Ydata))
  plotdata4 = data.frame(Xdata, integrated_signal)
  plotdata5 = melt(plotdata4, id = "Xdata")
  # p2=ggplot() +
  #   geom_line(data = plotdata3,
  #     aes(
  #       x = Xdata,
  #       y = value,
  #       colour = variable,
  #       group = variable
  #     )) +
  # 
  #   geom_area(data = plotdata,
  #     aes(
  #       x = Xdata,
  #       y = signal,
  #       position = 'fill',
  #       fill = 'Quantified Signal'
  #     )) +
  #   scale_x_reverse()
  p=plot_ly(plotdata,x = ~Xdata, y = ~signal, type = 'scatter', color= 'Signal',mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL) %>%
    layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
      yaxis = list(range=c(0,max(Ydata)),title = 'Intensity'))
  plot_data=rbind(integrated_signal,baseline,integrated_signal+baseline,integrated_signal)
  dummy=list(results_to_save=results_to_save,p=p,plot_data=plot_data)
  
  return(dummy)
}
