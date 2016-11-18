autorun_model_spectrum = function(autorun_data) {

  
  variable = value = signals = . = DT = D3TableFilter = shiny =  bd =label.col = label.col = R = key.row = key.col = elements = env =self = private = .values = ymax= ymin = label.row = x= y=c=integration_parameters = NULL # Setting the variables to NULL 
    #Preparation of necessary variables and folders to store figures and information of the fitting
  # if (is_autorun=='N') {indexes=input$x1_select
  
  ROI_data = read.csv(autorun_data$profile_folder_path, stringsAsFactors = F)
  dummy = which(is.na(ROI_data[, 1]))
  if (length(dummy)==0) dummy=dim(ROI_data)[1]+1
    lal=which(duplicated(ROI_data[-dummy,1:2])==F)
  ROI_separator = cbind(lal, c(lal[-1] - 1, dim(ROI_data[-dummy,])[1]))
  quartile_spectrum = as.numeric(apply(autorun_data$dataset, 2, function(x)
    quantile(x, 0.75,na.rm=T)))
  spectrum_index = which.min(apply(autorun_data$dataset, 1, function(x)
    sqrt(mean((x - quartile_spectrum) ^ 2
    ,na.rm=T))))
  plotdata = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata = as.numeric(autorun_data$dataset[spectrum_index,]))
  plotdata2 = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata=rep(0,length(autorun_data$ppm)))
  plotdata3 = data.frame(Xdata=as.numeric(autorun_data$ppm),Ydata=rep(0,length(autorun_data$ppm)))
    
  for (ROI_index in seq_along(ROI_separator[, 1])) {
    
    #Loading of every ROI parameters
    ROI_profile = ROI_data[ROI_separator[ROI_index, 1]:ROI_separator[ROI_index, 2],]

    ROI_buckets = which.min(abs(as.numeric(ROI_profile[1, 1])-autorun_data$ppm)):which.min(abs(as.numeric(ROI_profile[1, 2])-autorun_data$ppm))
    print(paste(ROI_profile[1,1], ROI_profile[1,2], sep = '-'))
    
    

    #Preparation of necessary parameters
    other_fit_parameters = fitting_variables()
    other_fit_parameters$freq = autorun_data$freq
    other_fit_parameters$ROI_buckets = ROI_buckets
    other_fit_parameters$buck_step = autorun_data$buck_step
    
    
    fitting_type = as.character(ROI_profile[1, 3])
    signals_to_quantify = which(ROI_profile[, 7] >= 1)

      ROI_buckets=which(round(autorun_data$ppm,6)==round(ROI_profile[1,1],6)):which(round(autorun_data$ppm,6)==round(ROI_profile[1,2],6))
  # print(ROI_buckets)
  
    
    
    Xdata= as.numeric(autorun_data$ppm[ROI_buckets])
    Ydata = as.numeric(autorun_data$dataset[spectrum_index, ROI_buckets])
   
    
    
    # signals_codes = replicate(length(signals_to_quantify), NA)
    # signals_names = replicate(length(signals_to_quantify), NA)
    # j = 1
    # for (i in signals_to_quantify) {
    #   k = which(autorun_data$signals_names == pre_import_excel_profile[i,
    #     4] & ROI_data[,7] == pre_import_excel_profile[i,
    #       7])
    #   
    #   signals_codes[j] = autorun_data$signals_codes[k]
    #   signals_names[j] = paste(as.character(autorun_data$signals_names[k]),pre_import_excel_profile[i,7],sep = '')
    #   j = j + 1
    # }
    # 
    # 
    # experiment_name = autorun_data$Experiments[[spectrum_index]]
    # plot_path = file.path(autorun_data$export_path,
    #                       experiment_name,
    #                       signals_names)
    # for (i in seq_along(plot_path))
    #   if (!dir.exists(plot_path[i]))
    #     dir.create(plot_path[i])
    # If the quantification is through integration with or without baseline
    if (fitting_type == "Clean Sum" ||
        fitting_type == "Baseline Sum") {
      is_roi_testing = "N"
      clean_fit = ifelse(fitting_type == "Clean Sum", "Y", "N")
      baseline = replicate(length(Xdata), 0)
      if (clean_fit == 'N')
        baseline = seq(mean(Ydata[1:3]), mean(Ydata[(length(Xdata) - 2):length(Xdata)]), length =
            length(Xdata))
      
      #integration ad chechk that there are no negative values
      integrated_signal = Ydata - baseline
      integrated_signal[integrated_signal<0]=0
      #preparation of output
      plotdata2$Ydata[ROI_buckets]= plotdata2$Ydata[ROI_buckets]+integrated_signal
      plotdata3$Ydata[ROI_buckets]= plotdata3$Ydata[ROI_buckets]+integrated_signal+baseline
      
      
    } else if (fitting_type == "Clean Fitting" || fitting_type ==
               "Baseline Fitting") {
      is_roi_testing = "N"
      clean_fit='N'

      # clean_fit = ifelse(fitting_type == "Clean Fitting", "Y",
      #                    "N")
    # print(ROI_profile)
      #Parameters of every signal necessary for the fitting
      initial_fit_parameters = ROI_profile[, 5:11,drop=F]
      # initial_fit_parameters=as.data.frame(apply(initial_fit_parameters,2,as.numeric))

      # initial_fit_parameters = initial_fit_parameters[complete.cases(initial_fit_parameters),]
      colnames(initial_fit_parameters) = c(
        "positions",
        "widths",
        "quantification_or_not",
        "multiplicities",
        "Jcoupling",
        "roof_effect",
        "shift_tolerance"
      )

      #Ydata is scaled to improve the quality of the fitting

      #Other parameters necessary for the fitting independent of the type of signal

      other_fit_parameters$clean_fit = clean_fit

      #Adaptation of the info of the parameters into a single matrix and preparation (if necessary) of the background signals that will conform the baseline
      FeaturesMatrix = fitting_prep(Xdata,
                                    Ydata,
                                    initial_fit_parameters,
                                    other_fit_parameters)

      #Calculation of the parameters that will achieve the best fitting
      signals_parameters = fittingloop(FeaturesMatrix,
                                       Xdata,
                                       Ydata,
                                       other_fit_parameters)
        
      #Fitting of the signals
      multiplicities=FeaturesMatrix[,11]
      roof_effect=FeaturesMatrix[,12]
      dummy = ROI_data[which(is.na(ROI_data[, 1])),]
      
      dummy2= list()
      for (i in 1:dim(ROI_profile)[1]) {
        
        if (length(which(dummy[,4] == ROI_profile[i,4]))>0 & ROI_profile[i,7] == 1) {
        dummy2[[length(dummy2)+1]]=which(dummy[,4] == ROI_profile[i,4])
        for (j in 1:length(dummy2[[i]])) {
        
          cc= signals_parameters[(5*i-4):(5*i)]
          cc[5]=dummy[dummy2[[i]][j],][9]
          cc[1]=dummy[dummy2[[i]][j],][12]*cc[1]
          cc[2]=as.numeric(dummy[dummy2[[i]][j],][5])+(as.numeric(cc[2])-as.numeric(ROI_profile[i,5]))
          # cc[5]=dummy[dummy2[[i]][j],][9]
          # cc[1]=dummy[dummy2[[i]][j],][12]*cc[1]
          # cc[2]=dummy[dummy2[[i]][j],][5]-(cc[2]-ROI_profile[i,5])
         
         signals_parameters=c(signals_parameters,cc)
         multiplicities=c(multiplicities,dummy[dummy2[[i]][j],][8])
         roof_effect=c(roof_effect,dummy[dummy2[[i]][j],][10])
        }   
        }
      }
      Xdata=autorun_data$ppm
      signals_parameters=unlist(signals_parameters)
      multiplicities=unlist(multiplicities)
      roof_effect=unlist(roof_effect)
     
      fitted_signals = fitting_optimization(signals_parameters,
                                         Xdata,multiplicities,roof_effect,Ydata,other_fit_parameters$freq)
      # signals_parameters=as.matrix(signals_parameters)
      dim(signals_parameters) = c(5, length(signals_parameters)/5)
      rownames(signals_parameters) = c(
        'intensity',
        'shift',
        'width',
        'gaussian',
        'J_coupling'
         )     
      other_fit_parameters$signals_to_quantify=signals_to_quantify
      Ydata = as.numeric(autorun_data$dataset[spectrum_index, ])
      

      #Generation of output data about the fitting and of the necessary variables for the generation ofa figure
      output_data = output_generator(
        signals_to_quantify,
        fitted_signals,
        Ydata,
        Xdata,
        signals_parameters,multiplicities
      )
      
      plotdata2$Ydata= plotdata2$Ydata+output_data$signals_sum
      plotdata3$Ydata= plotdata3$Ydata+output_data$fitted_sum
      
      # for (i in 1:length(dummy2)) {
      #   if (length(dummy2[[i]])>0) {
      #     print('Ha')
      #     dummy4=ROI_buckets+(ROI_profile[i,5]-dummy[dummy2[[i]],5])/autorun_data$buck_step
      #     plotdata2$Ydata[dummy4]= plotdata2$Ydata[dummy4]+output_data$signals_sum
      #     
      #     
      #   }
      # }
      
}
   
    
    }

  # p=plot_ly(plotdata2,x = ~Xdata, y = ~Ydata, type = 'scatter', name= 'Signals',mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~Ydata,name='Fitted spectrum',fill=NULL)  %>% add_trace(data=plotdata,x=~Xdata,y=~Ydata,name='Original spectrum',fill=NULL)  %>%
  #   layout(xaxis = list(range=c(max(autorun_data$ppm,na.rm=T),min(autorun_data$ppm,na.rm=T)),title = 'ppm'),
  #     yaxis = list(title = 'Intensity'))
  p=plot_ly(plotdata2,x = ~Xdata, y = ~Ydata, type = 'scatter', name= 'Signals',mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~Ydata,name='Fitted spectrum',fill=NULL)  %>%
    layout(xaxis = list(autorange = "reversed",title = 'ppm'),
      yaxis = list(title = 'Intensity'))
    
    # blah$finaloutput=finaloutput

    # blah$autorun_data=autorun_data
  return(p)
}
