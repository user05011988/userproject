integration=function(integration_parameters, Xdata, Ydata) {
#aquí preparo les condicions per fer la integració


 #agafem la regió en ppm
  output=list(signal_area_ratio=NA,fitting_error=NA,Area=NA,shift=NA)
baseline=replicate(length(Xdata),0)

if (integration_parameters$clean_fit=='N')     baseline=seq(mean(Ydata[1:3]),mean(Ydata[length(Xdata)-2:length(Xdata)]),length=length(Xdata))
 #si s'ha d'incorporar baseline

integrated_signal=Ydata-baseline
if (min(integrated_signal)<0) integrated_signal=integrated_signal-min(integrated_signal)
output$Area=sum(integrated_signal)
cumulative_area=cumsum(integrated_signal)/sum(integrated_signal)
p1=which.min(abs(cumulative_area - 0.05))
p2=which.min(abs(cumulative_area - 0.95))
output$signal_area_ratio=(sum(integrated_signal[p1:p2])/sum(Ydata[p1:p2]))*100
output$fitting_error=NA
peaks=peakdet(integrated_signal,0.2)
output$shift=mean(Xdata[peaks$maxtab$pos])
# mode=ifelse(integration_parameters$isroitest=='Y','MANRUN','AUTORUN')
# met_folder_path=file.path(integration_parameters$plot_path, mode, signals_names)
# save_path=file.path(met_folder_path, paste(experiment_name,'.png',sep=''))

plotdata=data.frame(Xdata,senyal=integrated_signal)
plotdata2=data.frame(Xdata,Ydata)
plotdata3 <- melt(plotdata2, id="Xdata")
plotdata4=data.frame(Xdata,integrated_signal)
plotdata5=melt(plotdata4, id="Xdata") 
ggplot() + 
  geom_line(data = plotdata3,aes(x = Xdata,y = value,colour=variable,group=variable)) + 
  geom_line(data = plotdata5,aes(x = Xdata,y = value,colour='subfraccions',group=variable)) +
  geom_area(data = plotdata,aes(x = Xdata,y = senyal,position = 'fill',fill='Senyal')) +
  scale_x_reverse()
ggsave(file.path(integration_parameters$plot_path,'Fit.jpeg'),width = 10, height = 5)

# if integration_parameters$isroitest=='N' {
#     h=figure('visible','off')
#     area(gca,Xdata,integrated_signal,'FaceColor','r'),set(gca,'NextPlot','add','XDir','reverse')
#     plot(gca,Xdata,RegionY,'b'),set(gca,'NextPlot','replace','XDir','reverse')
#     xlabel('ppm')
#     ylabel('Intensity')
#     M={'Quantification Signal'}
#     legend(M)
#     saveas(gcf, save_path, 'jpg')
#     close(h)
# }
return(output)
}
