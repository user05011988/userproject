integration = function(integration_parameters, Xdata, Ydata) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Integration of ROI


  #preallocation of output
  output = list(
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
  #preparation of output
  output$Area = sum(integrated_signal)
  output$intensity = max(integrated_signal)

  cumulative_area = cumsum(integrated_signal) / sum(integrated_signal)


  # p1 = which.min(abs(cumulative_area - 0.05))
  # p2 = which.min(abs(cumulative_area - 0.95))
  if (all(is.na(cumulative_area))) {
    p1=1
    p2=length(cumulative_area)
  }
  p1 = which(cumulative_area< 0.05)[length(which(cumulative_area< 0.05))]
  p2 = which(cumulative_area > 0.95)[1]

  output$signal_area_ratio = tryCatch((sum(integrated_signal[p1:p2]) / sum(Ydata[p1:p2])) *
    100,error = function(e) NaN, silent=T)
  print(output$signal_area_ratio)
  output$fitting_error = NaN
  output$width = NaN

  peaks = peakdet(integrated_signal, 0.2*max(0.000001,max(integrated_signal)))
  output$shift = mean(Xdata[peaks$maxtab$pos])

  #plot
  png(filename=paste(integration_parameters$plot_path,"Fit.png",sep='/'),
    type="cairo",
    units="in",
    width=8,
    height=4,
    pointsize=12,
    res=96)
  plotdata = data.frame(Xdata, signal = integrated_signal)
  plotdata2 = data.frame(Xdata, Ydata)
  plotdata3 <- melt(plotdata2, id = "Xdata")
  plotdata3$variable = rep('Original Spectrum', length(Ydata))
    plotdata4 = data.frame(Xdata, integrated_signal)
  plotdata5 = melt(plotdata4, id = "Xdata")
  p=ggplot() +
    geom_line(data = plotdata3,
              aes(
                x = Xdata,
                y = value,
                colour = variable,
                group = variable
              )) +
    # geom_line(data = plotdata5,
    #           aes(
    #             x = Xdata,
    #             y = value,
    #             colour = 'subfraccions',
    #             group = variable
    #           )) +
    geom_area(data = plotdata,
              aes(
                x = Xdata,
                y = signal,
                position = 'fill',
                fill = 'Quantified Signal'
              )) +
    scale_x_reverse()
  print(p)
  dev.off()


  return(output)
}
