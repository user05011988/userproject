variable = value = signals = . = DT = D3TableFilter = shiny =  bd =label.col = label.col = R = key.row = key.col = savedreactivedata = env =self = private = .values = ymax= ymin = label.row = x= y=c=t=value=variable=signals=private=self =.=integration_parameters = x= width=height=NULL # Setting the variables to NULL 
library("lazyeval")

f_eval(~ 1 + 2 + 3)


source('packages_sources.R')
packages_sources()
compiler::enableJIT(3)

ui <- fluidPage(  
  
  tags$head(tags$script("
    window.onload = function() {
    $('#mynavlist a:contains(\"ROI Testing\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Quantification Validation\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Uni and multivariate analysis\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"ROI Profiles\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Dendrogram heatmaps\")').parent().addClass('disabled')

}
    
    Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
    $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled')
    })
    ")),
  titlePanel("Dolphin Demo"), 
  tabsetPanel(selected="Data Upload and Processing", id='mynavlist',
    tabPanel("Data Upload and Processing",        
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Load the parameters file. It will automatically do an autorun of the ROI Profiles in the model spectrum. Then click Autorun if you wanna use these profiles for all spectra",
            accept = c("text/csv")
          ),
          
          # ,
          fileInput("file2", "Load RData if you want to reanudate a previous version",
            accept = c("text/RData")),
            actionButton("save_objs", "Save session")
            

        ),
        mainPanel(
          uiOutput('varselect'),
          uiOutput('varselect2'),
          uiOutput('align_button'),
          uiOutput('peak_analysis'),
          
          fluidRow(column(width = 12, h4("You can watch how the signals have been quantified in the spectrum model and, at the same time, an univariate analysis of every bin in the spectrum, according to the metadata given by the user.The idea is that you can analyze other parts of the spectrum with significant differences and add a ROI profile through the 'Profiles' tab."))),
          
          plotlyOutput("autorun_plot")
          
        ))),
    
    tabPanel("ROI Testing",
      fluidRow(column(width = 12, h4("Here you can change the quantifications and save them or remove them if they can't be well quantified. You can also save the edited profile of the ROI"))),
      sidebarLayout(
        
        sidebarPanel(
          
          actionButton("save_results", label = "Save Quantification"),
          actionButton("save_profile", label = "Save profile"),
          actionButton("autorun_signal", label = "Autorun of the signal"),
          actionButton("remove_q", label = "Remove quantification"),
          
          actionButton("action", label = "Quantification (without saving!)"),
          fluidRow(column(width = 12, h4("Select ROI"))),
          selectInput("select",label=NULL,choices=""),
          fluidRow(column(width = 12, h4("Select spectrum"))),
          DT::dataTableOutput('x1')
          
        ),
        
        
        mainPanel(
          fluidRow(column(width = 12, h4("You can watch the spectra here"))),
          plotlyOutput("plot"),
          fluidRow(column(width = 12, h4("You can watch the uploaded 2D file here"))),
          
          plotlyOutput("jres_plot"),
          
          fluidRow(column(width = 12, h4("You can edit the ROI Profile and quantify it"))),
          
          D3TableFilter::d3tfOutput('ROIdata',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You can directly edit the signals parameters if you are not satisfied with the calculated parameters."))),
          actionButton("direct_edition", label = "Direct edition"),
          
          D3TableFilter::d3tfOutput('mtcars2',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You have here some indicators of quality of the quantification"))),
          
          D3TableFilter::d3tfOutput('mtcars3',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("Here you can see the signals in the HMDB Repository located at the same zone of the spectrum, selected by biofluid"))),
          fluidRow(
            column(width = 12,
              DT::dataTableOutput("repository")
            ))
          
          )
        )
    ),
    tabPanel("Quantification Validation",
      fluidRow(column(width = 12, h4("Here you have the fitting error for every quantification. Press one cell to analyze the quantification. Don't click where there are no values"))),
      selectInput("select_validation",label=NULL,choices=c('Fitting error'=1,'Signal to area ratio'=2,'Shift'=3,'Width'=4,'Outliers'=5),selected="1"),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("fit_selection")
        )
        
      )
      
    ),
    tabPanel("ROI Profiles", 
      fluidRow(column(width = 12, h4("Here you can add ROI profiles"))),
      fluidRow(
        column(width = 12,
          D3TableFilter::d3tfOutput('new_roi_profile',width = "100%", height = "auto")
        )
        
      ),
      fluidRow(
        column(width = 12,
          selectInput("roirows",label=NULL,choices=1:10,selected=1)
        )),
      fluidRow(
        column(width = 12,
          actionButton("addroi", label = "Add ROI")
        )),
      fluidRow(
        column(width = 12,
          actionButton("saveroi", label = "Save ROI")
        )), 
          
      fluidRow(column(width = 12, h4("Here you have the current ROI profiles"))),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("roi_profiles")
        ))
      
    ),
    
    tabPanel("Uni and multivariate analysis", 
      fluidRow(column(width = 12, h4("Here you have boxplots for every quantified signal, with p values on the x axis"))),
        plotlyOutput(outputId = "plot_p_value_2"),
      fluidRow(column(width = 12, h4("PCA with loadings and scores"))),
        plotlyOutput(outputId = "pcascores"))
    
    ,tabPanel("Dendrogram heatmaps", 
      fluidRow(column(width = 12, h4("Here you have the dendrogram heatmap of quantification, so you can analyze relationships between spectra and between signals "))),
      plotlyOutput(outputId = "dendheatmapareadata"),
      fluidRow(column(width = 12, h4("Here you have the dendrogram heatmap of chemical shift, so you can analyze relationships between spectra and between signals"))),
      plotlyOutput(outputId = "dendheatmapshiftdata"))
    
 ))
  


server = function(input, output,session) {
  options(shiny.maxRequestSize=1000*1024^2)
  reactiveROItestingdata <- reactiveValues()


  m <- list(l = 150, r = 0, b = 150, t = 0,pad = 4)
  
  reactivequantdata <- reactiveValues(method2=NULL, method1 = NULL,stop3=0)
  
  reactiveprogramdata <- reactiveValues(ROIdata=NULL,ind=NULL,beginning=F,dataset=NULL,finaloutput=NULL,autorun_data=NULL,p_value_final=NULL,ROI_data=NULL,info=c(),ROI_separator=NULL,bucketing=NULL,select_options=NULL,new_roi_profile=NULL,p=NULL,bgColScales=NULL)
  
  observeEvent(input$roirows, {
    reactiveprogramdata$new_roi_profile= as.data.frame(matrix(NA,as.numeric(input$roirows),11))
    colnames(reactiveprogramdata$new_roi_profile)=colnames(reactiveprogramdata$ROI_data)
    
  })
  observeEvent(input$addroi, {
    reactiveprogramdata$ROI_data=rbind(reactiveprogramdata$ROI_data,reactiveprogramdata$new_roi_profile)

    
    
    dummy = which(!is.na(reactiveprogramdata$ROI_data[, 1]))
    reactiveprogramdata$ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(reactiveprogramdata$ROI_data)[1]))
    ROI_names=paste(reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],1],reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],2])
    reactiveprogramdata$select_options=1:length(ROI_names)
    names(reactiveprogramdata$select_options)=ROI_names
  })
  
  output$roi_profiles = DT::renderDataTable(
    
    reactiveprogramdata$ROI_data , server = T)
  proxy = dataTableProxy('roi_profiles')
  observe({
    replaceData(proxy, reactiveprogramdata$ROI_data)
  })
  output$repository = DT::renderDataTable(
    
    reactiveprogramdata$repository[which(reactiveprogramdata$repository[,5]>reactiveROItestingdata$ROIpar[1,2]&reactiveprogramdata$repository[,5]<reactiveROItestingdata$ROIpar[1,1]),] , server = T)
  proxy2 = dataTableProxy('repository')
  observe({
    replaceData(proxy2,  reactiveprogramdata$repository[which(reactiveprogramdata$repository[,5]>reactiveROItestingdata$ROIpar[1,2]&reactiveprogramdata$repository[,5]<reactiveROItestingdata$ROIpar[1,1]),] )
  })
  
  observeEvent(input$saveroi, {
    write.csv(reactiveprogramdata$ROI_data,reactiveprogramdata$autorun_data$profile_folder_path,row.names=F)
    print("Saving environment!")
    savedreactivedata=isolate(reactiveValuesToList(reactiveprogramdata))
    save(savedreactivedata, file = paste(reactiveprogramdata$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    
    print("Done!")
  })
    
    
  observeEvent(input$select, {
  
    if (reactiveprogramdata$beginning ==T) {
    reactiveprogramdata$ROIdata=reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[as.numeric(input$select), 1]:reactiveprogramdata$ROI_separator[as.numeric(input$select), 2],]
    }
    reactiveprogramdata$change=1
    reactiveprogramdata$stop=0
    reactiveprogramdata$change2=1
    reactiveprogramdata$stop2=0
    reactivequantdata$stop3=0
    # reactiveprogramdata$roi=NULL
    reactiveprogramdata$info=c()
    

    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    reactiveROItestingdata$ROIpar <- reactiveprogramdata$ROIdata
    reactiveROItestingdata$signpar <- rbind(rep(NA,7),rep(NA,7))
    colnames(reactiveROItestingdata$signpar)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    reactiveROItestingdata$qualitypar <- rbind(rep(NA,3),rep(NA,3))

    colnames(reactiveROItestingdata$qualitypar)=c('Quantification','fitting error','signal/total area ratio')
    
    output$ROIdata <- renderD3tf({
      
      tableProps <- list(
        btn_reset = F,
        sort = TRUE,
        sort_config = list(
          sort_types = c("String", rep("Number", ncol(reactiveprogramdata$ROIdata)))
        )
      )
      
      observe({
        if(is.null(input$mtcars_edit)|(reactiveprogramdata$stop==1)) {
          reactiveprogramdata$change=0
          print('step1')
          return(NULL)
        }
       
        edit <- input$mtcars_edit
        isolate({
          id <- edit$id
          row <- as.integer(edit$row)
          col <- as.integer(edit$col)
          val <- edit$val
          
          if(col == 0) {
            # rownames
            oldval <- rownames(reactiveROItestingdata$ROIpar)[row]
            # rownames can not start with a digit
            if(grepl('^\\d', val)) {
              rejectEdit(session, tbl = "ROIdata", row = row, col = col,  id = id, value = oldval)
              # reactiveprogramdata$roi=0
              
              return(NULL)
            }
          } else if (col %in% c(1:2,5:11)){
            # numeric columns
            if(is.na(suppressWarnings(as.numeric(val)))) {
              oldval <- reactiveROItestingdata$ROIpar[row, col]
              
              rejectEdit(session, tbl = "ROIdata", row = row, col = col, id = id, value = oldval)
              
              
              # reactiveprogramdata$roi=0
              return(NULL)
            }
          } else if (col %in% c(3)) {
            if(is.na(suppressWarnings(val))) {
              oldval <- reactiveROItestingdata$ROIpar[row, col]
              
              rejectEdit(session, tbl = "ROIdata", row = row, col = col, id = id, value = oldval)
              return(NULL)
            }
          }
          
          if (reactiveprogramdata$change==1){
            reactiveprogramdata$change=0
            reactiveprogramdata$stop=1          
            disableEdit(session, "ROIdata", c(1:11))
            print('step2')
          } else{ 
            if(col == 0) {
            } else if (col %in% c(1:2,5:11)) {
              reactiveROItestingdata$ROIpar[row, col] <- as.numeric(val)
              # reactiveprogramdata$roi=1
              print('step')
            } else if (col %in% c(3)) {
              reactiveROItestingdata$ROIpar[row, col] <- val
            }
            confirmEdit(session, tbl = "ROIdata", row = row, col = col, id = id, value = val)
          }
        })
      })
      
      d3tf(reactiveROItestingdata$ROIpar,
        tableProps = tableProps,
        enableTf = F,
        edit=TRUE,
        
        tableStyle = "table table-bordered")
      
    })
  })
  
  output$mtcars2 <- renderD3tf({
    print(reactiveROItestingdata$signpar)
    print(ncol(reactiveROItestingdata$signpar))
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(reactiveROItestingdata$signpar)))
      )
    )
    
    observe({
      if(is.null(input$mtcars2_edit)|(reactiveprogramdata$stop2==1)) {
        reactiveprogramdata$change2=0
        return(NULL)
      }       
      
      edit <- input$mtcars2_edit
      isolate({
        # need isolate, otherwise this observer would run twice
        # for each edit
        id <- edit$id
        row <- as.integer(edit$row)
        col <- as.integer(edit$col)
        val <- edit$val
        
        # validate input 
        if(col == 0) {
          # rownames
          oldval <- rownames(reactiveROItestingdata$signpar)[row]
          # rownames can not start with a digit
          if(grepl('^\\d', val)) {
            rejectEdit(session, tbl = "mtcars2", row = row, col = col,  id = id, value = oldval)
            # reactiveprogramdata$roi=0
            return(NULL)
          }
        } else if (col %in% c(1:7)){
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- reactiveROItestingdata$signpar[row, col]
            rejectEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = oldval)
            # reactiveprogramdata$roi=0
            return(NULL)
          }
        } 

        if (reactiveprogramdata$change2==1){
          reactiveprogramdata$change2=0
          reactiveprogramdata$stop2=1
        } else {
          reactiveROItestingdata$signpar[row, col] <- as.numeric(val)
          val = round(as.numeric(val), 3)
          confirmEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = val)
        }
      })
              })

      d3tf(reactiveROItestingdata$signpar,
        tableProps = tableProps,
        enableTf = F,
        edit=TRUE,
        rowStyles = reactiveprogramdata$bgColScales,
        tableStyle = "table table-bordered")
          
  })
         
      observeEvent(input$direct_edition, {
        
      
            reactivequantdata$method2=signals_int(reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput,reactiveprogramdata$ind,reactiveROItestingdata$signpar,reactiveROItestingdata$ROIpar) 

          reactiveROItestingdata$qualitypar=cbind(reactivequantdata$method2$results_to_save$Area,reactivequantdata$method2$results_to_save$fitting_error,reactivequantdata$method2$results_to_save$signal_area_ratio)
          colnames(reactiveROItestingdata$qualitypar)=c('Quantification','fitting error','signal/total area ratio')
          
          reactivequantdata$method1$signals_parameters=reactivequantdata$method2$signals_parameters
          reactivequantdata$method1$results_to_save=reactivequantdata$method2$results_to_save
          reactivequantdata$method1$other_fit_parameters=reactivequantdata$method2$other_fit_parameters
          reactivequantdata$method1$p=reactivequantdata$method2$p
          reactivequantdata$method1$p2=reactivequantdata$method2$p2
          
          reactivequantdata$method1$Xdata=reactivequantdata$method2$Xdata
          reactivequantdata$method1$Ydata=reactivequantdata$method2$Ydata
          reactivequantdata$method1$finaloutput=reactivequantdata$method2$finaloutput
          reactivequantdata$method1$fitting_type=reactivequantdata$method2$fitting_type
          reactivequantdata$method1$plot_path=reactivequantdata$method2$plot_path
          reactivequantdata$method1$import_excel_profile=reactivequantdata$method2$ROI_profile
          reactivequantdata$method1$signals_codes=reactivequantdata$method2$signals_codes
         
          
        })
        

  output$autorun_plot <- renderPlotly({
    
    p=plot_ly(data=reactiveprogramdata$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum',fill=NULL)
    p
  })
  observeEvent(input$action, {
    is_autorun='N'
    if(length(reactiveprogramdata$info)==0) reactiveprogramdata$ind=input$x1_rows_selected
    if (length(reactiveprogramdata$ind)!=1|reactiveprogramdata$ind>dim(reactiveprogramdata$autorun_data$dataset)[1]) {
      print('Select one valid spectrum')
      return(NULL)
    }
    reactivequantdata$method1 <- interface_quant(reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput, reactiveprogramdata$ind,reactiveROItestingdata$ROIpar,is_autorun) 
    reactivequantdata$stop3=1
    reactiveprogramdata$alignment_check=1
    
    reactiveROItestingdata$qualitypar=cbind(reactivequantdata$method1$results_to_save$Area,reactivequantdata$method1$results_to_save$fitting_error,reactivequantdata$method1$results_to_save$signal_area_ratio)
    colnames(reactiveROItestingdata$qualitypar)=c('Quantification','fitting error','signal/total area ratio')
    
    if (!is.null(reactivequantdata$method1$signals_parameters)) {
      reactiveprogramdata$bgColScales = c(rep("", dim(reactivequantdata$method1$signals_parameters)[1]), rep("info", dim(reactivequantdata$method1$signals_parameters_2)[1]-dim(reactivequantdata$method1$signals_parameters)[1]))
      reactiveROItestingdata$signpar <- reactivequantdata$method1$signals_parameters_2
      dim(reactivequantdata$method1$signals_parameters_2)
      dim(reactiveROItestingdata$signpar)
    reactiveprogramdata$stop=0
    # reactiveprogramdata$roi=1
    }
  })
  
  observeEvent(input$fit_selection_cell_clicked, {
    reactiveprogramdata$info=input$fit_selection_cell_clicked
    reactiveprogramdata$ind=reactiveprogramdata$info$row
    reactivequantdata$method2=NULL
    reactiveprogramdata$change=1
    reactiveprogramdata$stop=0
    reactiveprogramdata$change2=1
    reactiveprogramdata$stop2=0
    if (length(reactiveprogramdata$info$row)>0) reactivequantdata$stop3=1
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    updateSelectInput(session, "select",selected = NULL)
    

    is_autorun='N'
    if (length(reactiveprogramdata$info$row)!=1) {
      return(NULL)
    }
   
    path=paste(reactiveprogramdata$autorun_data$export_path,reactiveprogramdata$autorun_data$Experiments[reactiveprogramdata$info$row],reactiveprogramdata$autorun_data$signals_names[reactiveprogramdata$info$col],sep='/')
    Xdata=try(as.numeric(
      suppressWarnings(import(file.path(path,'Xdata.csv'))[,-1])),silent=T)
    if (class(Xdata)=="try-error") {
         print('Choose valid quantification')
         return(NULL)
    }
   
    Ydata=as.numeric(suppressWarnings(import(file.path(path,'Ydata.csv')))[,-1])

    dummy=suppressWarnings(import(file.path(path,'plot_data.csv')))
    plot_data=as.matrix(dummy[,-1])
    other_fit_parameters=as.list(suppressWarnings(import(file.path(path,'other_fit_parameters.csv')))[1,])
    ROI_profile=suppressWarnings(import(file.path(path,'import_excel_profile.csv')))[,-1,drop=F]
    other_fit_parameters$signals_to_quantify=ROI_profile[,7]
    plotdata2 = data.frame(Xdata=Xdata,
      Ydata=Ydata,
      plot_data[3, ] ,
      plot_data[2, ])
    colnames(plotdata2)=c('Xdata','Ydata',"fitted_sum","baseline_sum")

    plotdata3 <- melt(plotdata2, id = "Xdata")
    plotdata3$variable = c(
      rep('Original Spectrum', length(Ydata)),
      rep('Generated Spectrum', length(Ydata)),
      rep('Generated Background', length(Ydata))
    )
    plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F])))

    colnames(plotdata4)=c('Xdata',dummy[-c(1, 2, 3),1])
    plotdata5 = melt(plotdata4, id = "Xdata")
    r=which(paste(ROI_profile[,4],ROI_profile[,7],sep='_')==reactiveprogramdata$autorun_data$signals_names[reactiveprogramdata$info$col])
    plotdata = data.frame(Xdata, signals = plot_data[3 + r, ] )
    reactivequantdata$method1$p=plot_ly(plotdata,x = ~Xdata, y = ~signals, type = 'scatter', name= reactiveprogramdata$autorun_data$signals_names[reactiveprogramdata$info$col],mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL)  %>% add_trace(data=plotdata5,x=~Xdata,y=~value,color=~'Surrounding signals',type='scatter',mode='lines',fill=NULL)  %>%
      layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
        yaxis = list(title = 'Intensity'))

    # }
    reactiveROItestingdata$ROIpar=ROI_profile
    reactiveROItestingdata$signpar=t(suppressWarnings(import(file.path(path,'signals_parameters.csv')))[,-1])
    colnames(reactiveROItestingdata$signpar)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    ind=which(reactiveprogramdata$ROI_separator[,2]-reactiveprogramdata$info$col>=0)[1]
    ind=(reactiveprogramdata$ROI_separator[ind, 1]:reactiveprogramdata$ROI_separator[ind, 2])
    reactiveROItestingdata$qualitypar=cbind(reactiveprogramdata$finaloutput$Area[reactiveprogramdata$info$row,ind],reactiveprogramdata$finaloutput$fitting_error[reactiveprogramdata$info$row,ind],reactiveprogramdata$finaloutput$signal_area_ratio[reactiveprogramdata$info$row,ind])
    colnames(reactiveROItestingdata$qualitypar)=c('Quantification','fitting error','signal/total area ratio')

    updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")

    
  })
 
  observeEvent(input$autorun_signal, {
    
    is_autorun='Y'
    reactivequantdata$chor <- interface_quant(reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput, reactiveprogramdata$ind,reactiveROItestingdata$ROIpar,is_autorun) 
    reactiveprogramdata$finaloutput=reactivequantdata$chor$finaloutput

    
  })
  
  
  observeEvent(input$autorun_model, {
    output$autorun_plot <- renderPlotly({
      p=autorun_model_spectrum(reactiveprogramdata$autorun_data)
      p=p %>% add_trace(data=reactiveprogramdata$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum',fill=NULL)
      p
    })
  })
  
  observeEvent(input$alignment, {
    reactiveprogramdata$autorun_data$dataset=alignment(reactiveprogramdata$autorun_data$dataset,reactiveprogramdata$autorun_data$ppm)
    reactiveprogramdata$alignment_check=1
  })
    observeEvent(input$peak_analysis, {
      if (is.null(reactiveprogramdata$alignment_check)) {
        print('Before analysing peaks, I have to align them. Then Ill analyze them')
        dummy=alignment(reactiveprogramdata$autorun_data$dataset,reactiveprogramdata$autorun_data$ppm)
        peak_analysis(dummy,reactiveprogramdata$autorun_data$ppm,reactiveprogramdata$autorun_data$export_path,reactiveprogramdata$autorun_data$Metadata[,1],reactiveprogramdata$repository)
        reactiveprogramdata$alignment_check=1
      } else {
      peak_analysis(reactiveprogramdata$autorun_data$dataset,reactiveprogramdata$autorun_data$ppm,reactiveprogramdata$autorun_data$export_path,reactiveprogramdata$autorun_data$Metadata[,1],reactiveprogramdata$repository)
      }
        })
  
observeEvent(input$autorun, {
  
  reactiveprogramdata$finaloutput = autorun(reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput)
  is_autorun='Y'
  reactiveprogramdata$dataset=rbind(reactiveprogramdata$autorun_data$dataset,colMeans(reactiveprogramdata$autorun_data$dataset),apply(reactiveprogramdata$autorun_data$dataset,2,median))
  rownames(reactiveprogramdata$dataset)[(dim(reactiveprogramdata$autorun_data$dataset)[1]+1):dim(reactiveprogramdata$dataset)[1]]=c('Mean spectrum', 'Median spectrum')
  mm=matrix(NA,2,dim(reactiveprogramdata$autorun_data$Metadata)[2])
  colnames(mm)=colnames(reactiveprogramdata$autorun_data$Metadata)
  spectra=cbind(as.matrix(rownames(reactiveprogramdata$dataset)),rbind(reactiveprogramdata$autorun_data$Metadata,mm))
  colnames(spectra)=c('spectrum','Metadata')
 
  output$x1 = DT::renderDataTable(
    spectra , selection = list(mode = 'multiple', selected = 1),server = T)
  
  output$roi_profiles = DT::renderDataTable(
    reactiveprogramdata$ROI_data , server = T)
  proxy = dataTableProxy('roi_profiles')
  observe({
    replaceData(proxy, reactiveprogramdata$ROI_data)
  })
  
  reactiveprogramdata$beginning =T
  
  # session$sendCustomMessage('activeNavs', 'ROI Testing')
  # session$sendCustomMessage('activeNavs', 'Quantification Validation')
  # session$sendCustomMessage('activeNavs', 'Uni and multivariate analysis')
  # session$sendCustomMessage('activeNavs', 'Dendrogram heatmaps')
  updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")
})
  
  
  observeEvent(input$remove_q, {
    if (!is.null(reactiveprogramdata$autorun_data$signals_names[reactiveprogramdata$info$col])) {
      ind=which(reactiveprogramdata$ROI_data[,4]==reactiveprogramdata$autorun_data$signals_names[reactiveprogramdata$info$col])
    } else {
      ind=as.numeric(input$select)
    }

    reactiveprogramdata$finaloutput <- remove_quant(reactiveprogramdata$info,reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput)
  })

    observeEvent(input$save_results, {
    if (is.null(reactivequantdata$method1$signals_parameters)&is.null(reactivequantdata$method1$integration_parameters)) {
      print('Incorrect action')
        return(NULL)
    }
   
    reactiveprogramdata$finaloutput=save_roi_testing(reactivequantdata$method1,reactiveprogramdata$autorun_data, reactiveprogramdata$finaloutput) 
    
    print("Saving environment")
    savedreactivedata=isolate(reactiveValuesToList(reactiveprogramdata))
    save(savedreactivedata, file = paste(reactiveprogramdata$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    print("Done!")

    
  })
  
  observeEvent(input$save_profile, {
    if (length(reactiveprogramdata$info$col)>0) {
      ind=which(reactiveprogramdata$ROI_separator[,2]-reactiveprogramdata$info$col>=0)[1]
    } else {
      ind=as.numeric(input$select)
    }
    reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[ind, 1]:reactiveprogramdata$ROI_separator[ind, 2],]=reactiveROItestingdata$ROIpar
    write.csv(reactiveprogramdata$ROI_data,reactiveprogramdata$autorun_data$profile_folder_path,row.names=F)
    print("Saving environment")
    savedreactivedata=isolate(reactiveValuesToList(reactiveprogramdata))
    save(savedreactivedata, file = paste(reactiveprogramdata$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    print("Done!")
    
    
  })
  
  
  output$mtcars3 <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(reactiveROItestingdata$qualitypar)))
      )
    )

    d3tf(reactiveROItestingdata$qualitypar,
      tableProps = tableProps,
      enableTf = F,
      edit=F,
      tableStyle = "table table-bordered")
  })
  
  
  output$new_roi_profile <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(reactiveprogramdata$new_roi_profile)))
      )
    )

     
    observe({
      if(is.null(input$new_roi_profile_edit)|(reactiveprogramdata$stop2==1)) {
        reactiveprogramdata$change2=0
        return(NULL)
      }       
      
      edit <- input$new_roi_profile_edit
      isolate({
        id <- edit$id
        row <- as.integer(edit$row)
        col <- as.integer(edit$col)
        val <- edit$val
        if (reactiveprogramdata$change2==1){
          reactiveprogramdata$change2=0
          reactiveprogramdata$stop2=1
        } else {
          reactiveprogramdata$new_roi_profile[row, col] <- val
          confirmEdit(session, tbl = "new_roi_profile", row = row, col = col, id = id, value = val)
        }
      })
    })
    
    d3tf(reactiveprogramdata$new_roi_profile,
      tableProps = tableProps,
      enableTf = F,
      edit=T,
      
      tableStyle = "table table-bordered")
    
  })
  

  output$plot <- renderPlotly({
    if (reactivequantdata$stop3==0&length(reactiveprogramdata$info)==0|reactivequantdata$stop3==0&length(input$x1_rows_selected)>1) {
      ROI_limits=c(which.min(abs(reactiveprogramdata$autorun_data$ppm-reactiveprogramdata$ROIdata[1,1])),which.min(abs(reactiveprogramdata$autorun_data$ppm-reactiveprogramdata$ROIdata[1,2])))
      
      plotdata = data.frame(Xdata=reactiveprogramdata$autorun_data$ppm, t(rbind(reactiveprogramdata$autorun_data$dataset,colMeans(reactiveprogramdata$autorun_data$dataset),apply(reactiveprogramdata$autorun_data$dataset,2,median))[input$x1_rows_selected,,drop=F]))
      plotdata <- melt(plotdata, id = "Xdata")
      plot_ly(data=plotdata,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(range = c(round(reactiveprogramdata$ROIdata[1,1],6), round(reactiveprogramdata$ROIdata[1,2],6))),yaxis = list(range = c(0, max(reactiveprogramdata$autorun_data$dataset[input$x1_rows_selected,ROI_limits[1]:ROI_limits[2]]))) )
    } else {

     print(reactivequantdata$method1$p)

    }
  })


  observeEvent(input$select_validation, {
    validation_data=validation(reactiveprogramdata$finaloutput,reactiveprogramdata$other_fit_parameters,input$select_validation)
  output$fit_selection = DT::renderDataTable({ dat <- datatable(round(validation_data$alarmmatrix,4),selection = list(mode = 'single', target = 'cell')) %>% formatStyle(colnames(validation_data$alarmmatrix), backgroundColor = styleInterval(validation_data$brks, validation_data$clrs))
  return(dat)
  })
  })
  
  output$plot_p_value_2 <- renderPlotly({
    p_value_final=p_values(reactiveprogramdata$finaloutput$Area,reactiveprogramdata$autorun_data$Metadata[,1])
      
    boxplotdata=as.data.frame(reactiveprogramdata$finaloutput$Area)
    colnames(boxplotdata)=paste(colnames(boxplotdata),'(p= ',p_value_final,')',sep='')
    boxplotdata=cbind(boxplotdata,factor(reactiveprogramdata$autorun_data$Metadata[,1]))
    boxplotdata=melt(boxplotdata)
    colnames(boxplotdata)=c('Metadata','Signal','Value')
    
    plot_ly(boxplotdata, x = ~Signal, y = ~Value, color = ~Metadata, type = "box") %>%
      layout(boxmode='group',margin=m)
  })
  output$dendheatmapareadata <- renderPlotly({
    dummy=as.data.frame(reactiveprogramdata$finaloutput$Area)[!apply(as.data.frame(reactiveprogramdata$finaloutput$Area),2,function(x)all(is.na(x)))]
    heatmaply(scale(dummy)) %>% layout(margin = list(l = 130, b = 130))
  })
 
  output$dendheatmapshiftdata <- renderPlotly({
    dummy=as.data.frame(reactiveprogramdata$finaloutput$shift)[!apply(as.data.frame(reactiveprogramdata$finaloutput$shift),2,function(x)all(is.na(x)))]
    heatmaply(scale(dummy,scale=F)) %>% layout(margin = list(l = 130, b = 130))
  })
  output$pcascores <- renderPlotly({
    ind=which(apply(reactiveprogramdata$finaloutput$width,2, function(x) all(is.na(x)))==F)
    
    a=cbind(scale(reactiveprogramdata$finaloutput$Area[,ind]),reactiveprogramdata$autorun_data$Metadata)
    a=missForest(a)$ximp
    b=prcomp(a)
    carsDf2 <- data.frame(b$rotation)
    carsDf <- data.frame(b$x,metadata=reactiveprogramdata$autorun_data$Metadata)
    colnames(carsDf)[length(colnames(carsDf))]='metadata'
      p <- plot_ly(x=~ carsDf2$PC1,y=~ carsDf2$PC2,type='scatter',
        mode=~"markers",text = rownames(carsDf2),color='loadings',marker=list(size=8))%>% add_trace(x=~ carsDf$PC1,y=~ carsDf$PC2,
          mode=~"markers",text = rownames(carsDf),color =~ as.factor(carsDf$metadata),marker=list(size=11))
      p <- layout(p,title="PCA scores and loadings",
        xaxis=list(title="PC1"),
        yaxis=list(title="PC2"),margin=m)
      print(p)
  })

  
  observeEvent(input$x1_rows_selected, {
    if (reactiveprogramdata$beginning ==T) {
      reactiveprogramdata$ROIdata=reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[as.numeric(input$select), 1]:reactiveprogramdata$ROI_separator[as.numeric(input$select), 2],]
    }

    reactiveprogramdata$change=1
    reactiveprogramdata$stop=0
    reactiveprogramdata$change2=1
    reactiveprogramdata$stop2=0
    reactivequantdata$stop3=0
    # reactiveprogramdata$roi=NULL
    reactiveprogramdata$info=c()
    
    
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    
    reactiveROItestingdata$ROIpar <- reactiveprogramdata$ROIdata
    reactiveROItestingdata$signpar <- rbind(rep(NA,7),rep(NA,7))
    colnames(reactiveROItestingdata$signpar)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    reactiveROItestingdata$qualitypar <- rbind(rep(NA,3),rep(NA,3))
    colnames(reactiveROItestingdata$qualitypar)=c('Quantification','fitting error','signal/total area ratio')
    
    
  })
  
  observeEvent(input$file1, {
    reactiveprogramdata$inFile <- input$file1
    
    if (is.null(reactiveprogramdata$inFile))
      return(NULL)
    

    imported_data = import_data(reactiveprogramdata$inFile$datapath)
    
    if (!dir.exists(imported_data$export_path))
      dir.create(imported_data$export_path)
    for (i in seq_along(imported_data$Experiments)) {
      if (!dir.exists(file.path(imported_data$export_path, imported_data$Experiments[i]))) {
        dir.create(file.path(imported_data$export_path, imported_data$Experiments[i]))
      }
    }
    #creation of list with the different final outputs
    reactiveprogramdata$finaloutput = list()
    dummy = matrix(NaN,
      dim(imported_data$dataset)[1],
      length(imported_data$signals_names))
    reactiveprogramdata$ROI_data = read.csv(imported_data$profile_folder_path, stringsAsFactors = F)
    imported_data$signals_names=paste(imported_data$signals_names,reactiveprogramdata$ROI_data[1:dim(dummy)[2],7],sep='_')
    rownames(dummy) = imported_data$Experiments
    colnames(dummy) = imported_data$signals_names
    reactiveprogramdata$finaloutput$Area = reactiveprogramdata$finaloutput$signal_area_ratio = reactiveprogramdata$finaloutput$fitting_error =
      reactiveprogramdata$finaloutput$shift = reactiveprogramdata$finaloutput$intensity = reactiveprogramdata$finaloutput$width = dummy

        write.csv(
      as.data.frame(imported_data$params),
      file.path(imported_data$export_path, 'initial_params.csv'),
      row.names = F
    )
    colnames(imported_data$dataset) = imported_data$ppm
    rownames(imported_data$dataset) = imported_data$Experiments
    write.csv(imported_data$dataset,
      file.path(imported_data$export_path, 'initial_dataset.csv'),row.names=F)
    if ("not_loaded_experiments" %in% names(imported_data))
      write.table(
        imported_data$not_loaded_experiments,
        file.path(imported_data$export_path, 'not_loaded_experiments.csv'),
        row.names = F,
        col.names = F
      )
    #creation of list of necessary parameters for automatic quantification
    reactiveprogramdata$repository=imported_data$repository
    reactiveprogramdata$jres_path=imported_data$jres_path
    if (    reactiveprogramdata$jres_path!='')
      output$jres_plot <- try(renderPlotly({
        pp=fhs(reactiveprogramdata$jres_path)
        pp
      }))
    reactiveprogramdata$autorun_data = list(
      dataset = imported_data$dataset,
      ppm = imported_data$ppm,
      buck_step = imported_data$buck_step,
      profile_folder_path = imported_data$profile_folder_path,
      signals_names = imported_data$signals_names,
      signals_codes = imported_data$signals_codes,
      Experiments = imported_data$Experiments,
      export_path = imported_data$export_path,
      freq = imported_data$freq,
      Metadata=imported_data$Metadata
    )
    rm(imported_data)
    
    reactiveprogramdata$beginning =T
    
    mm=matrix(NA,2,dim(reactiveprogramdata$autorun_data$Metadata)[2])
    colnames(mm)=colnames(reactiveprogramdata$autorun_data$Metadata)
    spectra=cbind(c(rownames(reactiveprogramdata$autorun_data$dataset),'Mean Spectrum','Median Spectrum'),rbind(reactiveprogramdata$autorun_data$Metadata,mm))
    colnames(spectra)=c('spectrum',colnames(mm)) 
    output$x1 = DT::renderDataTable(
      spectra , selection = list(mode = 'multiple', selected = 1),server = T)
    
    other_fit_parameters = fitting_variables()
    reactiveprogramdata$ROI_data = read.csv(reactiveprogramdata$autorun_data$profile_folder_path, stringsAsFactors = F)
    dummy = which(is.na(reactiveprogramdata$ROI_data[, 1]))
    if (length(dummy)==0) dummy=dim(reactiveprogramdata$ROI_data)[1]+1
    lal=which(duplicated(reactiveprogramdata$ROI_data[-dummy,1:2])==F)
    reactiveprogramdata$ROI_separator = cbind(lal, c(lal[-1] - 1, dim(reactiveprogramdata$ROI_data[-dummy,])[1]))
    ROI_names=paste(reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],1],reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],2])
    reactiveprogramdata$select_options=1:length(ROI_names)
    names(reactiveprogramdata$select_options)=ROI_names
    updateSelectInput(session, "select",choices = reactiveprogramdata$select_options,selected = 1)

    p_value_bucketing=as.vector(p_values(reactiveprogramdata$autorun_data$dataset,reactiveprogramdata$autorun_data$Metadata[,1]))
    p_value_bucketing[is.na(p_value_bucketing)]=1
    plotdata = data.frame(Xdata=reactiveprogramdata$autorun_data$ppm, p_value_bucketing)
    output$varselect <- renderUI({
      if(is.null(plotdata)){return()}
      actionButton('autorun', 'Autorun all spectra')
    })
    output$varselect2 <- renderUI({
      if(is.null(plotdata)){return()}
      actionButton('autorun_model', 'Autorun model spectrum')
    })
    output$align_button <- renderUI({
      if(is.null(plotdata)){return()}
      actionButton('alignment', 'Alignment of signals')
    })
    output$peak_analysis <- renderUI({
      if(is.null(plotdata)){return()}
      actionButton('peak_analysis', 'Peak analysis')
    })
    
    quartile_spectrum = as.numeric(apply(reactiveprogramdata$autorun_data$dataset, 2, function(x)
      quantile(x, 0.75,na.rm=T)))
    ref_spectrum = reactiveprogramdata$autorun_data$dataset[which.min(apply(reactiveprogramdata$autorun_data$dataset, 1, function(x) sqrt(mean((x - quartile_spectrum) ^ 2 ,na.rm=T)))),]
    reactiveprogramdata$bucketing <- cbind(melt(plotdata, id = "Xdata"),ref_spectrum)
    reactiveprogramdata$bucketing=reactiveprogramdata$bucketing[complete.cases(reactiveprogramdata$bucketing),]
    colnames(reactiveprogramdata$bucketing)=c('Xdata','variable','pvalue','intensity')
    
    session$sendCustomMessage('activeNavs', 'ROI Profiles')
    session$sendCustomMessage('activeNavs', 'ROI Testing')
    session$sendCustomMessage('activeNavs', 'Quantification Validation')
    session$sendCustomMessage('activeNavs', 'Uni and multivariate analysis')
    session$sendCustomMessage('activeNavs', 'Dendrogram heatmaps')
    
  })
  
  observeEvent(input$file2, {
    reactiveprogramdata$inFile <- input$file2
    if (is.null(reactiveprogramdata$inFile))
      return(NULL)
    load(reactiveprogramdata$inFile$datapath)
    plo=names(sapply(savedreactivedata, names))
    for (i in 1:length(plo)) {
      reactiveprogramdata[[plo[i]]]=savedreactivedata[plo[i]]
    }
    reactiveprogramdata$finaloutput=savedreactivedata$finaloutput
    reactiveprogramdata$jres_path=savedreactivedata$jres_path
    reactiveprogramdata$repository=savedreactivedata$repository
    reactiveprogramdata$autorun_data=savedreactivedata$autorun_data
    reactiveprogramdata$p_value_final=savedreactivedata$p_value_final
    reactiveprogramdata$ROI_data=savedreactivedata$ROI_data
    reactiveprogramdata$ROI_separator=savedreactivedata$ROI_separator
    reactiveprogramdata$bucketing=savedreactivedata$bucketing
    reactiveprogramdata$beginning =T
    
    rm(savedreactivedata)
    is_autorun='Y'
    ROI_names=paste(reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],1],reactiveprogramdata$ROI_data[reactiveprogramdata$ROI_separator[, 1],2])
    reactiveprogramdata$select_options=1:length(ROI_names)
    names(reactiveprogramdata$select_options)=ROI_names
   
    mm=matrix(NA,2,dim(reactiveprogramdata$autorun_data$Metadata)[2])
    colnames(mm)=colnames(reactiveprogramdata$autorun_data$Metadata)
    spectra=cbind(c(rownames(reactiveprogramdata$autorun_data$dataset),'Mean Spectrum','Median Spectrum'),rbind(reactiveprogramdata$autorun_data$Metadata,mm))
    colnames(spectra)=c('spectrum',colnames(mm)) 
    output$x1 = DT::renderDataTable(
      spectra , selection = list(mode = 'multiple', selected = 1),server = T)
    output$autorun_plot <- renderPlotly({
      
      p=plot_ly(data=reactiveprogramdata$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum')
      p
    })
    
    if (reactiveprogramdata$jres_path!='')
      output$jres_plot <- try(renderPlotly({
        pp=fhs(reactiveprogramdata$jres_path)
        pp
      }))
    output$varselect2 <- renderUI({
      if(is.null(p)){return()}
      actionButton('autorun_model', 'Autorun model spectrum again')
      
    })
    output$varselect <- renderUI({
      if(is.null(p)){return()}
      actionButton('autorun', 'Autorun all spectra')
      
    })
    output$align_button <- renderUI({
      actionButton('alignment', 'Alignment of signals')
    })
    output$peak_analysis <- renderUI({
      actionButton('peak_analysis', 'Peak analysis')
    })
     
    updateSelectInput(session, "select",
      choices = reactiveprogramdata$select_options,selected = 1
    )
    session$sendCustomMessage('activeNavs', 'ROI Testing')
    session$sendCustomMessage('activeNavs', 'Quantification Validation')
    session$sendCustomMessage('activeNavs', 'Uni and multivariate analysis')
    session$sendCustomMessage('activeNavs', 'ROI Profiles')
    session$sendCustomMessage('activeNavs', 'Dendrogram heatmaps')
    updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")
  })
  
 
  observeEvent(input$save_objs, {
  print("Saving objects!")
  savedreactivedata=isolate(reactiveValuesToList(reactiveprogramdata))
  save(savedreactivedata, file = paste(reactiveprogramdata$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
  print("Done!")
})
  
  
  
}

shinyApp(ui, server)


