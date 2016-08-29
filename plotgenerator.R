plotgenerator = function(results_to_save,plot_data,Xdata,Ydata,fitted_signals,E_max,P_max,signals_names,experiment_name,is_roi_testing,plot_path) {
  for (r in 1:length(results_to_save$signal_area_ratio)) {
    
    if (results_to_save$signal_area_ratio[r]<P_max || results_to_save$fitting_error[r]>E_max) {
      
      
      
      # mode=ifelse(is_roi_testing=='Y','MANRUN','AUTORUN')
      # met_folder_path=file.path(plots_path, mode, signals_names[[r]])
      # if(!dir.exists(met_folder_path)) dir.create(met_folder_path)
      # save_path=file.path(met_folder_path, paste(experiment_name,'.png',sep=''))
     # if (is.null(dim(plot_data$quantified_signals))) plot_data$quantified_signals=t(plot_data$quantified_signals)
      plotdata=data.frame(Xdata,quantified_signals=plot_data[3+r,]*max(Ydata))
  plotdata2=data.frame(Xdata,Ydata,plot_data[3,]*max(Ydata),plot_data[2,]*max(Ydata))
  plotdata3 <- melt(plotdata2, id="Xdata")
  plotdata3$variable=c(rep('Original Spectrum',length(Ydata)),rep('Generated Spectrum',length(Ydata)),rep('Generated Background',length(Ydata)))
  plotdata4=data.frame(Xdata,(t(plot_data[-c(1,2,3),,drop=F])*max(Ydata)))
  plotdata5=melt(plotdata4, id="Xdata")
  # plotdata5$variable='Surrounding Signals'
  
  ggplot() + 
    geom_line(data = plotdata3,aes(x = Xdata,y = value,colour=variable,group=variable)) + 
    geom_line(data = plotdata5,aes(x = Xdata,y = value,colour='Surrounding signals',group=variable)) +
    geom_area(data = plotdata,aes(x = Xdata,y = quantified_signals,position = 'fill',fill='Quantified Signal')) +
    scale_x_reverse()
  # ggsave(save_path,width = 10, height = 5)
  ggsave(file.path(plot_path[r],'Fit.jpeg'),width = 10, height = 5)
  
    }
    }
  # 
  # 
  # p=ggplot(plotdata,aes(x = Xdata,y = quantified_signals)) +
  #   geom_area( position = 'stack') +
  #   geom_area( position = 'stack', colour="blue", show.legend=FALSE)
  # 
  # p=p+ggplot(plotdata3,aes(x = Xdata,y = value,colour=variable ))+ geom_line()
  # p=p+plot(gca,Xdata,Ydata,'k',Xdata,ybaseline,'y',Xdata,fraccio_total,'b',Xdata,subfraccions,'r'),set(gca,'NextPlot','replace')
  # M={'Quantification Signal''Original Spectra''Background Signal''Generated Spectra''Surrounding Signals'}
}