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
    baseline = seq(mean(Ydata[1:3]), mean(Ydata[length(Xdata) - 2:length(Xdata)]), length =
                     length(Xdata))

  #integration ad chechk that there are no negative values
  integrated_signal = Ydata - baseline
  if (min(integrated_signal) < 0)
    integrated_signal = integrated_signal - min(integrated_signal)

  #preparation of output
  output$Area = sum(integrated_signal)
  output$intensity = max(integrated_signal)

  cumulative_area = cumsum(integrated_signal) / sum(integrated_signal)
  p1 = which.min(abs(cumulative_area - 0.05))
  p2 = which.min(abs(cumulative_area - 0.95))
  output$signal_area_ratio = (sum(integrated_signal[p1:p2]) / sum(Ydata[p1:p2])) *
    100
  output$fitting_error = NA
  output$width = NA

  peaks = peakdet(integrated_signal, 0.2)
  output$shift = mean(Xdata[peaks$maxtab$pos])

  #plot
  plotdata = data.frame(Xdata, senyal = integrated_signal)
  plotdata2 = data.frame(Xdata, Ydata)
  plotdata3 <- melt(plotdata2, id = "Xdata")
  plotdata3$variable = rep('Original Spectrum', length(Ydata))
    plotdata4 = data.frame(Xdata, integrated_signal)
  plotdata5 = melt(plotdata4, id = "Xdata")
  ggplot() +
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
                y = senyal,
                position = 'fill',
                fill = 'Quantified Signal'
              )) +
    scale_x_reverse()
  ggsave(
    file.path(integration_parameters$plot_path, 'Fit.jpeg'),
    width = 10,
    height = 5
  )


  return(output)
}
