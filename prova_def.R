variable = value = signals = . = DT = D3TableFilter = shiny =  bd =label.col = label.col = R = key.row = key.col = elements = env =self = private = .values = ymax= ymin = label.row = x= y=c=t=value=variable=signals=private=self =.=integration_parameters = NULL # Setting the variables to NULL 
setwd("C:/Users/user/Documents/Dolphin/R")
library("lazyeval")

f_eval(~ 1 + 2 + 3)


library("minpack.lm")
library("reshape")
library("ggplot2")
library("compiler")
library("robustbase")
library("missForest")
library("rio")
library("png")
library(plotly)
library(DT)
library(D3TableFilter)
library(shiny)


source('packages_sources.R')
packages_sources()
compiler::enableJIT(3)

ui <- fluidPage(  
  
  tags$head(tags$script("
    window.onload = function() {
    $('#mynavlist a:contains(\"ROI Testing\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Fitting error values\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Dubious Quantifications\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Uni and multivariate analysis\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"ROI Profiles\")').parent().addClass('disabled')
    
}
    
    Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
    $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled')
    })
    ")),
  titlePanel("Dolphin Demo"), 
  tabsetPanel(selected="Data Upload", id='mynavlist',
    tabPanel("Data Upload",        
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
          fluidRow(column(width = 12, h4("You can watch how the signals have been quantified in the spectrum model and, at the same time, an univariate analysis of every bin in the spectrum, according to the metadata given by the user.The idea is that you can analyze other parts of the spectrum with significant differences and add a ROI profile through the 'Profiles' tab."))),
          
          plotlyOutput("autorun_plot"),
          fluidRow(column(width = 12, h4("Here the spectra have been previously clusterized and the exemplars of every cluster are plotted. This plot tries to give a general idea of how should be the ROI profiles (for example, how the shift tolerance would have to be) without having to analyze all spectra. There are red lines representing the most relevant peaks, to help the user set the peaks to quantify. This peak picking can be improved."))),
          
          plotlyOutput("apcluster_plot")
          
        )
      )
    ),   
    tabPanel("ROI Testing",
      fluidRow(column(width = 12, h4("Here you can change the quantifications and save them or remove them if they can't be well quantified. You can also save the edited profile of the ROI"))),
      sidebarLayout(
        
        sidebarPanel(
          
          actionButton("save_results", label = "Save quantificaiton"),
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
          
          D3TableFilter::d3tfOutput('mtcars',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You can directly edit the signals parameters if you are not satisfied with the calculated parameters."))),
          D3TableFilter::d3tfOutput('mtcars2',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You have here some indicators of quality of the quantification"))),
          
          D3TableFilter::d3tfOutput('mtcars3',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You can see a proposed model for the ROI profile, according to the information acquired during quantifications. I'm not sure it currently works"))),
          D3TableFilter::d3tfOutput('mtcars4',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("Here you can see the signals in the HMDB Repository located at the same zone of the spectrum, selected by biofluid"))),
          fluidRow(
            column(width = 12,
              DT::dataTableOutput("repository")
            ))
          
          )
        )
    ),
    tabPanel("Fitting error values",
      fluidRow(column(width = 12, h4("Here you have the fitting error for every quantification. Press one cell to analyze the quantification. Don't click where there are no values"))),
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
    tabPanel("Dubious Quantifications",
      fluidRow(column(width = 12, h4("Here you have a matrix that sums certain issues withthe quantification (width or shift not consistent, too high fitting error, too low signal to area ratio; the idea is to add analysis of outlers and of inconsistencies of relative intensity of signals. Don't click where there are no values"))),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("quant_selection")
        )
        
      )
      
    ),
    
    
    tabPanel("Uni and multivariate analysis", 
      # pageWithSidebar(headerPanel("Title"),
      #   
      # sidebarPanel(),
      # 
      # mainPanel(
      #   
      fluidRow(column(width = 12, h4("Here you have boxplots for every quantified signal, with p values on the x axis"))),
      
        plotlyOutput(outputId = "plot_p_value_2"),
      fluidRow(column(width = 12, h4("Here you have the dendrogram heatmap of quantification, so you can analyze relationships between spectra and between signals "))),
      
        plotlyOutput(outputId = "corr_area_spectrum"),
      fluidRow(column(width = 12, h4("Here you have the dendrogram heatmap of chemical shift, so you can analyze relationships between spectra and between signals"))),
      
      #   plotlyOutput(outputId = "corr_area_signal"),
      # fluidRow(column(width = 12, h4("Here you have the correlated chemical shifts between signals, to help analyze relationships between signals and make inferences about unknown signals "))),
      # 
        plotlyOutput(outputId = "corr_shift_signal"),
      fluidRow(column(width = 12, h4("PCA with loadings and scores"))),
      
        plotlyOutput(outputId = "pcascores"))
#   )
# )

    # ))
    #   
    #   fluidRow(
    #     column(width = 12, h4("Boxplots with p value in the x axis labels"),
    #       mainPanel(plotlyOutput("plot_p_value_2")))    ),
    #   fluidRow(
    #     column(width = 12, h4("Correrlation between spectra by quantification"),
    #       mainPanel(plotlyOutput("corr_area_spectrum")))    ),
    #   fluidRow(
    #         column(width = 12, h4("Correrlation between signals by quantification"),
    #           mainPanel(plotlyOutput("corr_area_signal")))    ),
    #   fluidRow(
    #             column(width = 12, h4("Correrlation between signals by chemical shift"),
    #               mainPanel(plotlyOutput("corr_shift_signal")))    ),
    #   fluidRow(
    #     column(width = 12, h4("PCA Scores"),
    #       mainPanel(plotlyOutput("pcascores")))    )
    #   # fluidRow(
    #   #   column(width = 12, h4("PCA Loadings"),
    #   #     mainPanel(plotlyOutput("pcaloadings")))    )
    # )
    # 
    
    
    
 ))
  


server = function(input, output,session) {
  options(shiny.maxRequestSize=1000*1024^2)
  revals <- reactiveValues()
  revals2 <- reactiveValues()
  revals3 <- reactiveValues()
  revals4 <- reactiveValues()
  
  m <- list(
    l = 150,
    r = 0,
    b = 150,
    t = 0,
    pad = 4
  )
  
  v <- reactiveValues(meh=NULL, blah = NULL,stop3=0)
  
  sell <- reactiveValues(mtcars=NULL,ind=NULL,beginning=F,dataset=NULL,inFile=NULL,finaloutput=NULL,brks=NULL,brks2=NULL,brks3=NULL,clrs=NULL,clrs2=NULL,clrs3=NULL,autorun_data=NULL,outlier_table=NULL,ab=NULL,p_value_final=NULL,ROI_data=NULL,info=c(),ROI_separator=NULL,bucketing=NULL,mediani=NULL,select_options=NULL,new_roi_profile=NULL,corr_area_matrix=NULL,p=NULL,bgColScales=NULL)
  
  observeEvent(input$roirows, {
    sell$new_roi_profile= as.data.frame(matrix(NA,as.numeric(input$roirows),11))
    # sell$new_roi_profile= matrix(NA,1,11)
    colnames(sell$new_roi_profile)=colnames(sell$ROI_data)
    
  })
  observeEvent(input$addroi, {
    sell$ROI_data=rbind(sell$ROI_data,sell$new_roi_profile)

    
    
    dummy = which(!is.na(sell$ROI_data[, 1]))
    sell$ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(sell$ROI_data)[1]))
    ROI_names=paste(sell$ROI_data[sell$ROI_separator[, 1],1],sell$ROI_data[sell$ROI_separator[, 1],2])
    sell$select_options=1:length(ROI_names)
    names(sell$select_options)=ROI_names
  })
  
  output$roi_profiles = DT::renderDataTable(
    
    sell$ROI_data , server = T)
  proxy = dataTableProxy('roi_profiles')
  observe({
    replaceData(proxy, sell$ROI_data)
  })
  output$repository = DT::renderDataTable(
    
    sell$repository[which(sell$repository[,5]>revals$mtcars[1,2]&sell$repository[,5]<revals$mtcars[1,1]),] , server = T)
  proxy2 = dataTableProxy('repository')
  observe({
    replaceData(proxy2,  sell$repository[which(sell$repository[,5]>revals$mtcars[1,2]&sell$repository[,5]<revals$mtcars[1,1]),] )
  })
  
  observeEvent(input$saveroi, {
   
    write.csv(sell$ROI_data,sell$autorun_data$profile_folder_path,row.names=F)
    print("** saving environment! **")
    elements=isolate(reactiveValuesToList(sell))
    ## save objects in current environment
    save(elements, file = paste(sell$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    
    print("** done saving     **")
  })
    
    
  observeEvent(input$select, {
  
    if (sell$beginning ==T) {
    sell$mtcars=sell$ROI_data[sell$ROI_separator[as.numeric(input$select), 1]:sell$ROI_separator[as.numeric(input$select), 2],]
    }
    
    
    
    # selectCells(dataTableProxy('fit_selection'), NULL)
    # selectCells(dataTableProxy('quant_selection'),NULL)
    
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    v$stop3=0
    sell$roi=NULL
    sell$info=c()
    

    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    revals$mtcars <- sell$mtcars
    revals2$mtcars <- rbind(rep(NA,7),rep(NA,7))
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals3$mtcars <- rbind(rep(NA,3),rep(NA,3))
    revals4$mtcars <- sell$mtcars
    
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    
    output$mtcars <- renderD3tf({
      
      tableProps <- list(
        btn_reset = F,
        sort = TRUE,
        sort_config = list(
          sort_types = c("String", rep("Number", ncol(sell$mtcars)))
        )
      )
      
      observe({
        if(is.null(input$mtcars_edit)|(sell$stop==1)) {
          sell$change=0
          print('step1')
          return(NULL)
          # } 
        }
       
        # }
        edit <- input$mtcars_edit
        #
        isolate({
          id <- edit$id
          row <- as.integer(edit$row)
          col <- as.integer(edit$col)
          val <- edit$val
          
          if(col == 0) {
            # rownames
            oldval <- rownames(revals$mtcars)[row]
            # rownames can not start with a digit
            if(grepl('^\\d', val)) {
              rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval)
              sell$roi=0
              
              return(NULL)
            }
          } else if (col %in% c(1:2,5:11)){
            # numeric columns
            if(is.na(suppressWarnings(as.numeric(val)))) {
              oldval <- revals$mtcars[row, col]
              
              rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval)
              
              
              sell$roi=0
              return(NULL)
            }
          } else if (col %in% c(3)) {
            if(is.na(suppressWarnings(val))) {
              oldval <- revals$mtcars[row, col]
              
              rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval)
              return(NULL)
            }
          }
          
          if (sell$change==1){
         
            
            sell$change=0
            sell$stop=1          # sell$stop=1
            disableEdit(session, "mtcars", c(1:11))
            print('step2')
            

          } else{ 
            if(col == 0) {
              
            } else if (col %in% c(1:2,5:11)) {
              revals$mtcars[row, col] <- as.numeric(val)
              sell$roi=1
              print('step')
              
            } else if (col %in% c(3)) {
              revals$mtcars[row, col] <- val
              
              
            }
            
            confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val)
          }
          
        })
        
      })
      
      d3tf(revals$mtcars,
        tableProps = tableProps,
        enableTf = F,
        edit=TRUE,
        
        tableStyle = "table table-bordered")
      
    })
  })
  
  output$mtcars2 <- renderD3tf({
    
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(revals2$mtcars)))
      )
    )
    
    observe({
      if(is.null(input$mtcars2_edit)|(sell$stop2==1)) {
        sell$change2=0
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
          oldval <- rownames(revals2$mtcars)[row]
          # rownames can not start with a digit
          if(grepl('^\\d', val)) {
            rejectEdit(session, tbl = "mtcars2", row = row, col = col,  id = id, value = oldval)
            sell$roi=0
            
            return(NULL)
          }
        } else if (col %in% c(1:7)){
          # numeric columns
          if(is.na(suppressWarnings(as.numeric(val)))) {
            oldval <- revals2$mtcars[row, col]
            
            rejectEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = oldval)
            
            
            sell$roi=0
            return(NULL)
          }
        } 
        # accept edits
        if (sell$change2==1){

          sell$change2=0
          sell$stop2=1
         
          
        } else {
          
          revals2$mtcars[row, col] <- as.numeric(val)
          val = round(as.numeric(val), 3)
          confirmEdit(session, tbl = "mtcars2", row = row, col = col, id = id, value = val)
          
          if (sell$ss==1) {
            ptm <- proc.time()
            sell$ss=0
          }
          print((proc.time()-ptm))
          
          if ((proc.time()-ptm)[1]<1) {
            v$meh=signals_int(sell$autorun_data, sell$finaloutput,sell$ind,revals2$mtcars,revals$mtcars) 

          revals3$mtcars=cbind(v$meh$results_to_save$Area,v$meh$results_to_save$fitting_error,v$meh$results_to_save$signal_area_ratio)
          colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
          
          v$blah$signals_parameters=v$meh$signals_parameters
          v$blah$results_to_save=v$meh$results_to_save
          v$blah$other_fit_parameters=v$meh$other_fit_parameters
          v$blah$p=v$meh$p
          v$blah$p2=v$meh$p2
          
          v$blah$Xdata=v$meh$Xdata
          v$blah$Ydata=v$meh$Ydata
          v$blah$finaloutput=v$meh$finaloutput
          v$blah$fitting_type=v$meh$fitting_type
          v$blah$plot_path=v$meh$plot_path
          v$blah$import_excel_profile=v$meh$ROI_profile
          v$blah$signals_codes=v$meh$signals_codes
          } else {
            sell$ss=1
            
          }
          
        }
        
      })
     
      
      
    })
    
    d3tf(revals2$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=TRUE,
      rowStyles = sell$bgColScales,
      tableStyle = "table table-bordered")

  })
  
  
  observeEvent(input$action, {
print(input$mynavlist)   
    is_autorun='N'
    if(length(sell$info)==0) sell$ind=input$x1_rows_selected
    if (length(sell$ind)!=1|sell$ind>dim(sell$autorun_data$dataset)[1]) {
      print('Select one valid spectrum')
      return(NULL)
    }
    v$blah <- interface_quant(sell$autorun_data, sell$finaloutput, sell$ind,revals$mtcars,is_autorun) 
    v$stop3=1
    sell$ss=1
    
    revals3$mtcars=cbind(v$blah$results_to_save$Area,v$blah$results_to_save$fitting_error,v$blah$results_to_save$signal_area_ratio)
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    # fo=(dim(v$blah$signals_parameters)[1]+1):dim(v$blah$signals_parameters_2)[1]
    
    # sell$bgColScales=list()
    # for (i in 1:length(fo)) {
    #       sell$bgColScales=c(sell$bgColScales, "auto:white:green")
    #       names(sell$bgColScales)[[i]]=paste('row',fo[i],sep='_')
    #   }
    # print(sell$bgColScales)
    
    if (!is.null(v$blah$signals_parameters)) {
      sell$bgColScales = c(rep("", dim(v$blah$signals_parameters)[1]), rep("info", dim(v$blah$signals_parameters_2)[1]-dim(v$blah$signals_parameters)[1]))
      revals2$mtcars <- v$blah$signals_parameters_2
    revals2$rowIndex <- 1:nrow(revals2$mtcars)
    sell$stop=0
    sell$roi=1
    }
  })
  
  observeEvent(input$fit_selection_cell_clicked, {
    sell$info=input$fit_selection_cell_clicked
    sell$ind=sell$info$row
    v$meh=NULL
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    if (length(sell$info$row)>0) v$stop3=1
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    updateSelectInput(session, "select",selected = NULL
    )
    

    is_autorun='N'
    if (length(sell$info$row)!=1) {
      return(NULL)
    }
   
    # path=paste(sell$autorun_data$export_path,sell$autorun_data$Experiments[sell$info$row],paste(sell$autorun_data$signals_names[sell$info$col],sell$ROI_data[sell$info$col,7],sep='_'),sep='/')
    # 
    path=paste(sell$autorun_data$export_path,sell$autorun_data$Experiments[sell$info$row],sell$autorun_data$signals_names[sell$info$col],sep='/')
    Xdata=as.numeric(import(file.path(path,'Xdata.csv'))[,-1])
    Ydata=as.numeric(import(file.path(path,'Ydata.csv'))[,-1])

    dummy=import(file.path(path,'plot_data.csv'))
    plot_data=as.matrix(dummy[,-1])
    other_fit_parameters=as.list(import(file.path(path,'other_fit_parameters.csv'))[1,])
    ROI_profile=import(file.path(path,'import_excel_profile.csv'))[,-1,drop=F]
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
    r=which(paste(ROI_profile[,4],ROI_profile[,7],sep='_')==sell$autorun_data$signals_names[sell$info$col])
    plotdata = data.frame(Xdata, signals = plot_data[3 + r, ] )
    v$blah$p=plot_ly(plotdata,x = ~Xdata, y = ~signals, type = 'scatter', name= sell$autorun_data$signals_names[sell$info$col],mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL)  %>% add_trace(data=plotdata5,x=~Xdata,y=~value,color=~'Surrounding signals',type='scatter',mode='lines',fill=NULL)  %>%
      layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
        yaxis = list(title = 'Intensity'))

    # }
    revals$mtcars=ROI_profile
    revals2$mtcars=t(import(file.path(path,'signals_parameters.csv'))[,-1])
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals4$mtcars=ROI_profile
    ind=which(sell$ROI_separator[,2]-sell$info$col>=0)[1]
    ind=(sell$ROI_separator[ind, 1]:sell$ROI_separator[ind, 2])
    
    
    revals4$mtcars[,5]=sell$flo[sell$info$row,ind,1]
    revals4$mtcars[,6]=sell$flo2[sell$info$row,ind,1]
    revals4$mtcars[,11]=sell$flo[sell$info$row,ind,3]-sell$flo[sell$info$row,ind,1]

    revals3$mtcars=cbind(sell$finaloutput$Area[sell$info$row,ind],sell$finaloutput$fitting_error[sell$info$row,ind],sell$finaloutput$signal_area_ratio[sell$info$row,ind])
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')

    updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")

    
  })
  observeEvent(input$quant_selection_cell_clicked, {
    sell$info=input$quant_selection_cell_clicked
    sell$ind=sell$info$row
    v$meh=NULL
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    if (length(sell$info$row)>0)       v$stop3=1
    
    
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    updateSelectInput(session, "select",
      selected = NULL
    )
    is_autorun='N'
    if (length(sell$info$row)!=1) {
      return(NULL)
    }
    # path=paste(sell$autorun_data$export_path,sell$autorun_data$Experiments[sell$info$row],paste(sell$autorun_data$signals_names[sell$info$col],sell$ROI_data[sell$info$col,7],sep='_'),sep='/')
    # 
    path=paste(sell$autorun_data$export_path,sell$autorun_data$Experiments[sell$info$row],sell$autorun_data$signals_names[sell$info$col],sep='/')
    
    Xdata=as.numeric(import(file.path(path,'Xdata.csv'))[,-1])
    Ydata=as.numeric(import(file.path(path,'Ydata.csv'))[,-1])
    dummy=import(file.path(path,'plot_data.csv'))
    plot_data=as.matrix(dummy[,-1])
    other_fit_parameters=as.list(import(file.path(path,'other_fit_parameters.csv'))[1,])
    ROI_profile=import(file.path(path,'import_excel_profile.csv'))[,-1,drop=F]
    other_fit_parameters$signals_to_quantify=ROI_profile[,7]
    
    plotdata2 = data.frame(Xdata=Xdata,
      Ydata=Ydata,
      plot_data[3, ],
      plot_data[2, ])
    colnames(plotdata2)=c('Xdata','Ydata',"fitted_sum","baseline_sum")
    plotdata3 <- melt(plotdata2, id = "Xdata")
    plotdata3$variable = c(
      rep('Original Spectrum', length(Ydata)),
      rep('Generated Spectrum', length(Ydata)),
      rep('Generated Background', length(Ydata))
    )
    plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) ))
    colnames(plotdata4)=c('Xdata',dummy[-c(1, 2, 3),1])
    
    plotdata5 = melt(plotdata4, id = "Xdata")
    
    r=which(paste(ROI_profile[,4],ROI_profile[,7],sep='_')==sell$autorun_data$signals_names[sell$info$col])
    plotdata = data.frame(Xdata, signals = plot_data[3 + r, ] )
    
    
    v$blah$p=plot_ly(plotdata,x = ~Xdata, y = ~signals, type = 'scatter', name= sell$autorun_data$signals_names[sell$info$col],mode = 'lines', fill = 'tozeroy') %>% add_trace(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines',fill=NULL)  %>% add_trace(data=plotdata5,x=~Xdata,y=~value,color=~'Surrounding signals',type='scatter',mode='lines',fill=NULL)  %>%
      layout(xaxis = list(range=c(Xdata[1],Xdata[length(Xdata)]),title = 'ppm'),
        yaxis = list(title = 'Intensity'))
    
    # }
    revals$mtcars=ROI_profile
    revals2$mtcars=t(import(file.path(path,'signals_parameters.csv'))[,-1])
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    
    revals4$mtcars=ROI_profile
    ind=which(sell$ROI_separator[,2]-sell$info$col>=0)[1]
    ind=sell$ROI_separator[ind, 1]:sell$ROI_separator[ind, 2]
    revals4$mtcars[,5]=sell$flo[sell$info$row,ind,1]
    revals4$mtcars[,6]=sell$flo2[sell$info$row,ind,1]
    
    revals4$mtcars[,11]=sell$flo[sell$info$row,ind,3]-sell$flo[sell$info$row,ind,1]
    revals3$mtcars=cbind(sell$finaloutput$Area[sell$info$row,ind],sell$finaloutput$fitting_error[sell$info$row,ind],sell$finaloutput$signal_area_ratio[sell$info$row,ind])
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")
    
  })
  
  observeEvent(input$autorun_signal, {
    
    is_autorun='Y'
    v$chor <- interface_quant(sell$autorun_data, sell$finaloutput, sell$ind,revals$mtcars,is_autorun) 
    sell$finaloutput=v$chor$finaloutput

    
  })
  
  
  observeEvent(input$autorun_model, {
    output$autorun_plot <- renderPlotly({
      p=autorun_model_spectrum(sell$autorun_data)
      p=p %>% add_trace(data=sell$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum',fill=NULL)
      p
    })
  })
    
observeEvent(input$autorun, {
  
  sell$finaloutput = autorun(sell$autorun_data, sell$finaloutput)
  
  
  
  # sell$outlier_table=matrix(0,dim(ll)[1],dim(ll)[2])
  # sell$outlier_table=as.data.frame(sell$outlier_table)
  # 
  # colnames(sell$outlier_table)=colnames(t_test_data_2)
  # rownames(sell$outlier_table)=rownames(sell$finaloutput$fitting_error)
  # 
  # 
  # for (j in 1:length(ss)) {
  #   sell$outlier_table[sell$autorun_data$Metadata==ss[j],][sapply(as.data.frame(sell$finaloutput$Area[sell$autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
  #   
  #   # ind=which(autorun_data$Metadata==ss[j])
  #   # sell$outlier_table[ind[sell$finaloutput$Area[autorun_data$Metadata==ss[j],i] %in%  Dubious Quantifications],i]=1
  # }
  
  
  
  

  is_autorun='Y'
  sell$dataset=rbind(sell$autorun_data$dataset,colMeans(sell$autorun_data$dataset),apply(sell$autorun_data$dataset,2,median))
  rownames(sell$dataset)[(dim(sell$autorun_data$dataset)[1]+1):dim(sell$dataset)[1]]=c('Mean spectrum', 'Median spectrum')
  mm=matrix(NA,2,dim(sell$autorun_data$Metadata)[2])
  colnames(mm)=colnames(sell$autorun_data$Metadata)
  spectra=cbind(as.matrix(rownames(sell$dataset)),rbind(sell$autorun_data$Metadata,mm))
  colnames(spectra)=c('spectrum','Metadata')
  sell$brks <- quantile(sell$finaloutput$fitting_error, probs = seq(.05, .95, .05), na.rm = TRUE)
  sell$clrs <- round(seq(255, 40, length.out = length(sell$brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  sell$brks3 <- quantile(sell$corr_area_matrix, probs = seq(.05, .95, .05), na.rm = TRUE)
  sell$clrs3 <- round(seq(255, 40, length.out = length(sell$brks3) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  sell$brks2 <- 0.5
  sell$clrs2 <- round(seq(255, 40, length.out = length(sell$brks2) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  
  output$x1 = DT::renderDataTable(
    
    spectra , selection = list(mode = 'multiple', selected = 1),server = T)
  
  output$roi_profiles = DT::renderDataTable(
    
    sell$ROI_data , server = T)
  proxy = dataTableProxy('roi_profiles')
  observe({
    replaceData(proxy, sell$ROI_data)
  })
  
  sell$beginning =T
  updateSelectInput(session, "select",
    choices = sell$select_options,selected = 1
  )
  session$sendCustomMessage('activeNavs', 'ROI Testing')
  session$sendCustomMessage('activeNavs', 'Fitting error values')
  session$sendCustomMessage('activeNavs', 'Uni and multivariate analysis')
  session$sendCustomMessage('activeNavs', 'Dubious Quantifications')
  updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")
})
  
  
  observeEvent(input$remove_q, {
    if (!is.null(sell$autorun_data$signals_names[sell$info$col])) {
      ind=which(sell$ROI_data[,4]==sell$autorun_data$signals_names[sell$info$col])
    } else {
      ind=as.numeric(input$select)
    }

    sell$finaloutput <- remove_quant(sell$info,sell$autorun_data, sell$finaloutput)

  })
  # } 
  observeEvent(input$save_results, {
    if (is.null(v$blah$signals_parameters)&is.null(v$blah$integration_parameters)) {
      print('Incorrect action')
        return(NULL)
    }
   
    sell$finaloutput=save_roi_testing(v$blah,sell$autorun_data, sell$finaloutput) 
    
    print("** saving environment! **")
    elements=isolate(reactiveValuesToList(sell))
    ## save objects in current environment
    save(elements, file = paste(sell$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    
    print("** done saving     **")

    # sell$outlier_table[,]=0
    # ss=unique(sell$autorun_data$Metadata[,1])
    # 
    # for (j in 1:length(ss)) {
    #   sell$outlier_table[sell$autorun_data$Metadata==ss[j],][sapply(as.data.frame(sell$finaloutput$Area[sell$autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
    #   
    # }
    
  })
  
  observeEvent(input$save_profile, {
    if (length(sell$info$col)>0) {
      ind=which(sell$ROI_separator[,2]-sell$info$col>=0)[1]
    } else {
      ind=as.numeric(input$select)
    }
    print(sell$ROI_separator[ind,])
    sell$ROI_data[sell$ROI_separator[ind, 1]:sell$ROI_separator[ind, 2],]=revals$mtcars
    
    write.csv(sell$ROI_data,sell$autorun_data$profile_folder_path,row.names=F)
    print("** saving environment! **")
    elements=isolate(reactiveValuesToList(sell))
    ## save objects in current environment
    save(elements, file = paste(sell$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
    
    print("** done saving     **")
    
    
  })
  
  
  output$mtcars3 <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(revals3$mtcars)))
      )
    )

    d3tf(revals3$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=F,
      
      tableStyle = "table table-bordered")
    
    # })
  })
  output$mtcars4 <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(revals4$mtcars)))
      )
    )
    
    d3tf(revals4$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=F,
      
      tableStyle = "table table-bordered")
    
    # })
  })
  
  output$new_roi_profile <- renderD3tf({
    tableProps <- list(
      btn_reset = TRUE,
      sort = TRUE,
      sort_config = list(
        sort_types = c("String", rep("Number", ncol(sell$new_roi_profile)))
      )
    )
   
     
        
        
        
     
    observe({
      if(is.null(input$new_roi_profile_edit)|(sell$stop2==1)) {
        sell$change2=0
        return(NULL)
      }       
      
      edit <- input$new_roi_profile_edit
      isolate({
        # need isolate, otherwise this observer would run twice
        # for each edit
        id <- edit$id
        row <- as.integer(edit$row)
        col <- as.integer(edit$col)
        val <- edit$val
        
        # validate input 
        # if(col == 0) {
        #   # rownames
        #   oldval <- rownames(sell$new_roi_profile)[row]
        #   # rownames can not start with a digit
        #   if(grepl('^\\d', val)) {
        #     rejectEdit(session, tbl = "new_roi_profile", row = row, col = col,  id = id, value = oldval)
        #     sell$roi=0
        #     
        #     return(NULL)
        #   }
        # } else if (col %in% c(1:7)){
        #   # numeric columns
        #   if(is.na(suppressWarnings(as.numeric(val)))) {
        #     oldval <- sell$new_roi_profile[row, col]
        #     
        #     rejectEdit(session, tbl = "new_roi_profile", row = row, col = col, id = id, value = oldval)
        #     
        #     
        #     sell$roi=0
        #     return(NULL)
        #   }
        # } 
        # # accept edits
        if (sell$change2==1){
          
          sell$change2=0
          sell$stop2=1
          
          
        } else {
          
          sell$new_roi_profile[row, col] <- val
          confirmEdit(session, tbl = "new_roi_profile", row = row, col = col, id = id, value = val)
         
        }
        
      })
      
      
      
    })
    
    d3tf(sell$new_roi_profile,
      tableProps = tableProps,
      enableTf = F,
      edit=T,
      
      tableStyle = "table table-bordered")
    
    # })
  })
  
  # output$p_value_final = DT::renderDataTable(round(sell$p_value_final,3))
  
  
  
  
  output$plot <- renderPlotly({
    print(v$stop3)
    print(sell$info)
    if ((v$stop3==0&(length(sell$info)==0)|(v$stop3==0&length(input$x1_rows_selected)>1))) {
      lol=which(round(sell$autorun_data$ppm,6)==round(sell$mtcars[1,1],6))
      lol2=which(round(sell$autorun_data$ppm,6)==round(sell$mtcars[1,2],6))
      
      plotdata = data.frame(Xdata=sell$autorun_data$ppm, t(sell$dataset[input$x1_rows_selected,,drop=F]))
      
      plotdata3 <- melt(plotdata, id = "Xdata")
      plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(range = c(round(sell$mtcars[1,1],6), round(sell$mtcars[1,2],6))),yaxis = list(range = c(0, max(sell$dataset[input$x1_rows_selected,lol:lol2]))))
    } else {
     print(v$blah$p)

    }
  })

  
  output$quant_selection = DT::renderDataTable({ 
    dummy=validation(sell$finaloutput,sell$other_fit_parameters)
    
    sell$flo=dummy$flo
    sell$flo2=dummy$flo2
    sell$outlier_table=dummy$alarmmatrix
    print(dim(sell$outlier_table))
    dat <- datatable(sell$outlier_table,selection = list(mode = 'single', target = 'cell')) %>% formatStyle(colnames(sell$outlier_table), backgroundColor = styleInterval(sell$brks2, sell$clrs2))
  return(dat)
  })
  
  output$fit_selection = DT::renderDataTable({ dat <- datatable(round(sell$finaloutput$fitting_error,2),selection = list(mode = 'single', target = 'cell')) %>% formatStyle(colnames(sell$finaloutput$fitting_error), backgroundColor = styleInterval(sell$brks, sell$clrs))
  return(dat)
  })
  
  output$corr_area_matrix = DT::renderDataTable({ dat <- datatable(round(sell$corr_area_matrix,3)) %>% formatStyle(colnames(sell$finaloutput$fitting_error), backgroundColor = styleInterval(sell$brks3, sell$clrs3))
  return(dat)
  })
  
  # output$plot_p_value <- renderPlotly({
  # 
  #   plot_ly(data=sell$bucketing,x=~Xdata,y=~intensity,color=~pvalue,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"),yaxis = list(range = c(0, max(sell$bucketing$intensity))))
  #   
  # })
  
  
  output$plot_p_value_2 <- renderPlotly({
    t_test_data_2=sell$finaloutput$Area
    ss=unique(sell$autorun_data$Metadata[,1])
    tt=matrix(NA,length(ss),dim(t_test_data_2)[2])
    for (ind in seq_along(ss)) {
      for (k in 1:dim(t_test_data_2)[2]) {
        tt[ind,k]=tryCatch(shapiro.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
      }
      
    }
    p_value=rep(NA,dim(t_test_data_2)[2])
    for (k in 1:dim(t_test_data_2)[2]) {
      # if (!any(is.na(t_test_data_2[,k]))) {
      if (!any(tt[,k]<0.05,na.rm=T)) {
        p_value[k]=tryCatch(wilcox.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
      } else {
        p_value[k]=tryCatch(t.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
      }
      
      # }
    }
    p_value_final=round(t(as.matrix(p.adjust(p_value,method="BH"))),3)
    ll=as.data.frame(sell$finaloutput$Area)
    colnames(ll)=paste(colnames(ll),'(p= ',p_value_final,')',sep='')
    Xwit=cbind(ll,factor(sell$autorun_data$Metadata[,1]))
    ab=melt(Xwit)
    colnames(ab)=c('Metadata','Signal','Value')
    
    plot_ly(ab, x = ~Signal, y = ~Value, color = ~Metadata, type = "box") %>%
      layout(boxmode='group',margin=m)
  })
  output$corr_area_spectrum <- renderPlotly({
    # ind=which(apply(sell$finaloutput$width,2, function(x) all(is.na(x)))==F)
    # 
    # cr=cor(t(sell$finaloutput$Area[,ind]),use='pairwise.complete.obs',method='spearman')
    # bb=hclust(dist(cr))$order
    # dr=cr[bb,bb]
    # p= plot_ly(x=rownames(dr),y=colnames(dr), z = dr, type = "heatmap")
    # p<- layout(p, xaxis = list(categoryarray = rownames(dr), categoryorder = "array"),yaxis = list(categoryarray = colnames(dr), categoryorder = "array"),margin=m)
    cc=as.data.frame(sell$finaloutput$Area)[!apply(as.data.frame(sell$finaloutput$Area),2,function(x)all(is.na(x)))]
    heatmaply(scale(cc)) %>% layout(margin = list(l = 130, b = 130))
  })
  output$corr_area_signal <- renderPlotly({
    # ind=which(apply(sell$finaloutput$width,2, function(x) all(is.na(x)))==F)
    # 
    # cr=cor(sell$finaloutput$Area[,ind],use='pairwise.complete.obs',method='spearman')
    # bb=hclust(dist(cr))$order
    # dr=cr[bb,bb]
    # p= plot_ly(x=rownames(dr),y=colnames(dr), z = dr, type = "heatmap")
    # p<- layout(p, xaxis = list(categoryarray = rownames(dr), categoryorder = "array"),yaxis = list(categoryarray = colnames(dr), categoryorder = "array"),margin=m)
  })
  output$corr_shift_signal <- renderPlotly({
    # ind=which(apply(sell$finaloutput$width,2, function(x) all(is.na(x)))==F)
    # 
    # cr=cor(sell$finaloutput$shift[,ind],use='pairwise.complete.obs',method='spearman')
    # bb=hclust(dist(cr))$order
    # dr=cr[bb,bb]
    # p= plot_ly(x=rownames(dr),y=colnames(dr), z = dr, type = "heatmap")
    # p<- layout(p, xaxis = list(categoryarray = rownames(dr), categoryorder = "array"),yaxis = list(categoryarray = colnames(dr), categoryorder = "array"),margin=m)
    cc=as.data.frame(sell$finaloutput$shift)[!apply(as.data.frame(sell$finaloutput$shift),2,function(x)all(is.na(x)))]
    heatmaply(scale(cc,scale=F)) %>% layout(margin = list(l = 130, b = 130))
    # hm2 <- heatmaply(scale(sell$finaloutput$shift)) %>% layout(margin = list(l = 130, b = 130))
    # print(subplot(hm1, hm2, margin = .1))
  })
  output$pcascores <- renderPlotly({
    ind=which(apply(sell$finaloutput$width,2, function(x) all(is.na(x)))==F)
    
    a=cbind(scale(sell$finaloutput$Area[,ind]),sell$autorun_data$Metadata)
    a=missForest(a)$ximp
    b=prcomp(a)
    carsDf2 <- data.frame(b$rotation)
    carsDf <- data.frame(b$x,metadata=sell$autorun_data$Metadata)
    colnames(carsDf)[length(colnames(carsDf))]='metadata'
      p <- plot_ly(x=~ carsDf2$PC1,y=~ carsDf2$PC2,type='scatter',
        mode=~"markers",text = rownames(carsDf2),color='loadings',marker=list(size=8))%>% add_trace(x=~ carsDf$PC1,y=~ carsDf$PC2,
          mode=~"markers",text = rownames(carsDf),color =~ as.factor(carsDf$metadata),marker=list(size=11))
      p <- layout(p,title="PCA scores and loadings",
        xaxis=list(title="PC1"),
        yaxis=list(title="PC2"),margin=m)
      print(p)
  })
  # output$pcaloadings <- renderPlotly({
  #   a=cbind(scale(sell$finaloutput$Area),sell$autorun_data$Metadata)
  #   a=missForest(a)$ximp
  #   b=prcomp(a)
  #   
  #   carsDf <- data.frame(b$rotation)
  #   
  #   
  #   p <- plot_ly(carsDf,x=~ PC1,y=~ PC2,
  #     mode=~"markers",text = rownames(carsDf),marker=list(size=11))
  #   p <- layout(p,title="PCA loadings",
  #     xaxis=list(title="PC1"),
  #     yaxis=list(title="PC2"))
  #   
  # })
  observeEvent(input$x1_rows_selected, {
    if (sell$beginning ==T) {
      sell$mtcars=sell$ROI_data[sell$ROI_separator[as.numeric(input$select), 1]:sell$ROI_separator[as.numeric(input$select), 2],]
    }
    
    
    # selectCells(dataTableProxy('fit_selection'), NULL)
    # selectCells(dataTableProxy('quant_selection'),NULL)
    
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    v$stop3=0
    sell$roi=NULL
    sell$info=c()
    
    
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    
    revals$mtcars <- sell$mtcars
    revals2$mtcars <- rbind(rep(NA,7),rep(NA,7))
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals3$mtcars <- rbind(rep(NA,3),rep(NA,3))
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    
    
  })
  
  observeEvent(input$file1, {
    sell$inFile <- input$file1
    
    if (is.null(sell$inFile))
      return(NULL)
    

    imported_data = import_data(sell$inFile$datapath)
    
    if (!dir.exists(imported_data$export_path))
      dir.create(imported_data$export_path)
    for (i in seq_along(imported_data$Experiments)) {
      if (!dir.exists(file.path(imported_data$export_path, imported_data$Experiments[i]))) {
        dir.create(file.path(imported_data$export_path, imported_data$Experiments[i]))
      }
    }
    #creation of list with the different final outputs
    sell$finaloutput = list()
    dummy = matrix(NaN,
      dim(imported_data$dataset)[1],
      length(imported_data$signals_names))
    sell$ROI_data = read.csv(imported_data$profile_folder_path, stringsAsFactors = F)
    imported_data$signals_names=paste(imported_data$signals_names,sell$ROI_data[1:dim(dummy)[2],7],sep='_')
    rownames(dummy) = imported_data$Experiments
    colnames(dummy) = imported_data$signals_names
    sell$finaloutput$Area = sell$finaloutput$signal_area_ratio = sell$finaloutput$fitting_error =
      sell$finaloutput$shift = sell$finaloutput$intensity = sell$finaloutput$width = dummy
    
    #creation of several outputs with data of interest before beginnig the quantification
    write.csv(
      as.data.frame(imported_data$params),
      file.path(imported_data$export_path, 'initial_params.csv'),
      row.names = F
    )
    colnames(imported_data$dataset) = imported_data$ppm
    rownames(imported_data$dataset) = imported_data$Experiments
    write.csv(imported_data$dataset,
      file.path(imported_data$export_path, 'initial_dataset.csv'))
    if ("not_loaded_experiments" %in% names(imported_data))
      write.table(
        imported_data$not_loaded_experiments,
        file.path(imported_data$export_path, 'not_loaded_experiments.csv'),
        row.names = F,
        col.names = F
      )
    #creation of list of necessary parameters for automatic quantification
    sell$repository=imported_data$repository
    sell$jres_path=imported_data$jres_path
    if (    sell$jres_path!='')
      output$jres_plot <- try(renderPlotly({
        pp=fhs(sell$jres_path)
        pp
      }))
    
    sell$autorun_data = list(
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
    
    
    # sell$p=autorun_model_spectrum(sell$autorun_data)
    other_fit_parameters = fitting_variables()
    
    
    sell$ROI_data = read.csv(sell$autorun_data$profile_folder_path, stringsAsFactors = F)
    dummy = which(is.na(sell$ROI_data[, 1]))
    if (length(dummy)==0) dummy=dim(sell$ROI_data)[1]+1
    lal=which(duplicated(sell$ROI_data[-dummy,1:2])==F)
    sell$ROI_separator = cbind(lal, c(lal[-1] - 1, dim(sell$ROI_data[-dummy,])[1]))
    # mtcars2=ROI_data[1:2,4:11]
    # mtcars=ROI_data[1:2,4:11]
    
    ROI_names=paste(sell$ROI_data[sell$ROI_separator[, 1],1],sell$ROI_data[sell$ROI_separator[, 1],2])
    sell$select_options=1:length(ROI_names)
    names(sell$select_options)=ROI_names
    t_test_data=sell$autorun_data$dataset
    
    ss=unique(sell$autorun_data$Metadata[,1])
    tt=matrix(NA,length(ss),dim(t_test_data)[2])
    for (ind in seq_along(ss)) {
      for (k in 1.:dim(t_test_data)[2]) {
        tt[ind,k]=tryCatch(shapiro.test(t_test_data[sell$autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
      }
      
    }
    p_value_bucketing=rep(NA,dim(t_test_data)[2])
    for (k in 1:dim(t_test_data)[2]) {
      # if (!any(is.na(t_test_data[,k]))) {
      if (!any(tt[,k]<0.05,na.rm=T)) {
        p_value_bucketing[k]=tryCatch(suppressWarnings(wilcox.test(t_test_data[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data[sell$autorun_data$Metadata[,1]==ss[2],k]))$p.value,error=function(e) NA)
      } else {
        p_value_bucketing[k]=tryCatch(t.test(t_test_data[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data[sell$autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
      }
      
      # }
    }
    p_value_bucketing=p.adjust(p_value_bucketing,method="BH")
    p_value_bucketing[is.na(p_value_bucketing)]=1
    plotdata = data.frame(Xdata=sell$autorun_data$ppm, p_value_bucketing)
    # sell$mediani=apply(sell$autorun_data$dataset,2,function(x) median(x,na.rm=T))
    quartile_spectrum = as.numeric(apply(sell$autorun_data$dataset, 2, function(x)
      quantile(x, 0.75,na.rm=T)))
    ref_spectrum = sell$autorun_data$dataset[which.min(apply(sell$autorun_data$dataset, 1, function(x)
      sqrt(mean((x - quartile_spectrum) ^ 2
        ,na.rm=T)))),]
    # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
    sell$bucketing <- cbind(melt(plotdata, id = "Xdata"),ref_spectrum)
    sell$bucketing=sell$bucketing[complete.cases(sell$bucketing),]
    colnames(sell$bucketing)=c('Xdata','variable','pvalue','intensity')
    
    output$autorun_plot <- renderPlotly({
      p=autorun_model_spectrum(sell$autorun_data)
      p=p %>% add_trace(data=sell$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum',fill=NULL)
      p
    })
    output$varselect <- renderUI({
      if(is.null(p)){return()}
      actionButton('autorun', 'Autorun all spectra')
      
    })
    output$varselect2 <- renderUI({
      if(is.null(p)){return()}
      actionButton('autorun_model', 'Autorun model spectrum again')
    })
    output$apcluster_plot <- renderPlotly({
      p=clustspectraplot(sell$autorun_data)
      p
    })
   
    
    session$sendCustomMessage('activeNavs', 'ROI Profiles')
    
    
  })
  
  observeEvent(input$file2, {
    sell$inFile <- input$file2

    if (is.null(sell$inFile))
      return(NULL)

    load(sell$inFile$datapath)
    plo=names(sapply(elements, names))
    for (i in 1:length(plo)) {
      sell[[plo[i]]]=elements[plo[i]]
      
    }
    
   
    # sell$info=elements$info
    # sell$mtcars=elements$mtcars
    # 
    # sell$ind=elements$ind
    sell$finaloutput=elements$finaloutput
    # sell$beginning=elements$beginning
    sell$brks=elements$brks
    sell$brks2=elements$brks2
    sell$brks3=elements$brks3
    sell$clrs=elements$clrs
    sell$clrs2=elements$clrs2
    sell$clrs3=elements$clrs3
    sell$jres_path=elements$jres_path
    sell$repository=elements$repository
    # sell$p=elements$p
    sell$autorun_data=elements$autorun_data
    
    sell$outlier_table=elements$outlier_table
    sell$ab=elements$ab
    sell$p_value_final=elements$p_value_final
    sell$ROI_data=elements$ROI_data
    # sell$fo=elements$fo
    #   sell$fo2=elements$fo2
    sell$flo=elements$flo
      sell$flo2=elements$flo2
    
    sell$ROI_separator=elements$ROI_separator
    sell$bucketing=elements$bucketing
    sell$mediani=elements$mediani

    rm(elements)
    is_autorun='Y'
    ROI_names=paste(sell$ROI_data[sell$ROI_separator[, 1],1],sell$ROI_data[sell$ROI_separator[, 1],2])
    sell$select_options=1:length(ROI_names)
    names(sell$select_options)=ROI_names
    sell$dataset=rbind(sell$autorun_data$dataset,colMeans(sell$autorun_data$dataset),apply(sell$autorun_data$dataset,2,median))
    mm=matrix(NA,2,dim(sell$autorun_data$Metadata)[2])
    colnames(mm)=colnames(sell$autorun_data$Metadata)
    spectra=cbind(as.matrix(rownames(sell$dataset)),rbind(sell$autorun_data$Metadata,mm))
    colnames(spectra)=c('spectrum','Metadata')    # sell<-trek
    # rm(trek)
    output$autorun_plot <- renderPlotly({
      p=autorun_model_spectrum(sell$autorun_data)
      p=p %>% add_trace(data=sell$bucketing,x=~Xdata,y=~intensity,color=~pvalue,scatter='lines',name='Original spectrum',fill=NULL)
      p
    })
    output$apcluster_plot <- renderPlotly({
      p=clustspectraplot(sell$autorun_data)
      print(p)
    })
    if (sell$jres_path!='')
      output$jres_plot <- try(renderPlotly({
        pp=fhs(sell$jres_path)
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
     output$x1 = DT::renderDataTable(
      
      spectra , selection = list(mode = 'multiple', selected = 1),server = T)
    sell$beginning =T
    updateSelectInput(session, "select",
      choices = sell$select_options,selected = 1
    )
    session$sendCustomMessage('activeNavs', 'ROI Testing')
    session$sendCustomMessage('activeNavs', 'Fitting error values')
    session$sendCustomMessage('activeNavs', 'Dubious Quantifications')
    session$sendCustomMessage('activeNavs', 'Uni and multivariate analysis')
    session$sendCustomMessage('activeNavs', 'ROI Profiles')
    updateTabsetPanel(session, "mynavlist",selected = "ROI Testing")
    

  })
  
  # observeEvent(input$download, {
  #   
  # myfile=file.choose()
  # trek<<-sell
  # save.image('man.RData')
  # rm(trek)
  # myfile=NULL
  # })
  
  observeEvent(input$save_objs, {
  # Run whenever save_objs button is pressed
  
  print("** saving objects! **")
  elements=isolate(reactiveValuesToList(sell))
  ## save objects in current environment
  save(elements, file = paste(sell$autorun_data$export_path,"savedenvironment.Rdata",sep='/'), envir = environment())
  
  print("** done saving     **")
})
  
  
  
}

shinyApp(ui, server)


