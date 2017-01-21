plotgenerator = function(results_to_save,
                         plot_data,
                         Xdata,
                         Ydata,
                         fitted_signals,
                         program_parameters,
                         signals_names,
                         experiment_name,
                         is_roi_testing
                         ) {
  #Created by Daniel Cañueto 30/08/2016
  #Generation of the plot of the quantification of the signal with information of the original ROI, of the fitting of every signal of interest of the ROI and of the background and addtional signals created to adapt the fitting to the concrete characteristics of the ROI
  # print(results_to_save$signal_area_ratio)
  # print(program_parameters$signals_to_quantify)
  # print(results_to_save$correlation)
  plots <- vector("list", length(results_to_save$signal_area_ratio)) 
  for (r in 1:length(results_to_save$signal_area_ratio)) {
    
    #There is only creation of plot if the conditions specified in the Parameters file are accomplished
    # if (results_to_save$signal_area_ratio[r] < program_parameters$signal_area_ratio_plot ||
    #     results_to_save$correlation[r] > program_parameters$fitting_error_plot) {
      plotdata = data.frame(Xdata, signals = plot_data[3 + program_parameters$signals_to_quantify[r], ] )
      plotdata2 = data.frame(Xdata,
                             Ydata,
                             plot_data[3, ],
                             plot_data[2, ])
      plotdata3 <- melt(plotdata2, id = "Xdata")
      plotdata3$variable = c(
        rep('Original Spectrum', length(Ydata)),
        rep('Generated Spectrum', length(Ydata)),
        rep('Generated Background', length(Ydata))
      )
      plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) ))
      plotdata5 = melt(plotdata4, id = "Xdata")
      
      
        plots[[r]]=ggplot() +
          geom_line(data = plotdata3,
            aes(
              x = Xdata,
              y = value,
              colour = variable,
              group = variable
            )) +
          geom_line(data = plotdata5,
            aes(
              x = Xdata,
              y = value,
              colour = 'Surrounding signals',
              group = variable
            )) +
          geom_area(
            data = plotdata,
            aes(
              x = Xdata,
              y = signals,
              # position = 'fill',
              fill = 'Quantified Signal'
            )
          ) +
          scale_x_reverse() + labs(x='ppm',y='Intensity')
       
      
    }
  # }
  return(plots)
}