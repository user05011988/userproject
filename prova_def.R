variable = value = signals = . = DT = D3TableFilter = shiny =  bd = NULL # Setting the variables to NULL 
setwd("C:/Users/user/Documents/Dolphin/R")


library("minpack.lm")
library("reshape")
library("ggplot2")
library("compiler")
library("robust")
library("apcluster")
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
    $('#mynavlist a:contains(\"Outliers\")').parent().addClass('disabled')
    $('#mynavlist a:contains(\"Univariate analysis\")').parent().addClass('disabled')
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
          fileInput("file1", "Load the parameters file. It will automatically do an autorun of some signals of some spectra of the MTBLS1 dataset, so the process will take some time. I have prepared it this way so you can already look how univariate analysis works. The definitive version would let the user check the parameters, the ROI profiles and metadata before letting him make an autorun or a ROI testing",
            accept = c("text/csv")
          )
          # ,
          # fileInput("file2", "Load RData if you have already previously performed quantifications and want to recover them",
          #   accept = c("text/RData")
          # ),
          # ,
          # br(),
          # verbatimTextOutput("Save RData file (.RData extension) when you have ended playing with this demo, so there is some coherence in the data when you play with the demo in the future. I will blame any problem about the demo on you for not following this instruction."),
          # actionButton("download", label = "download")
          
        ),
        mainPanel(
          tableOutput("contents")
        )
      )
    ),   
    tabPanel("ROI Testing",
      fluidRow(column(width = 12, h4("Here you can change the quantifications and save them or remove them if they can't be well quantified. YOu can also save the edited profile of the ROI"))),
      sidebarLayout(
        
        sidebarPanel(
          
          actionButton("save_results", label = "Save quantificaiton"),
          actionButton("save_profile", label = "Save profile"),
          actionButton("autorun", label = "Autorun of the signal"),
          actionButton("remove_q", label = "Remove quantification"),
          
          actionButton("action", label = "Quantification (without saving!)"),
          fluidRow(column(width = 12, h4("Select ROI"))),
          selectInput("select",label=NULL,choices=""),
          fluidRow(column(width = 12, h4("Select spectrum"))),
          DT::dataTableOutput('x1')
          
        ),
        
        
        mainPanel(
          plotlyOutput("plot"),
          fluidRow(column(width = 12, h4("You can edit the ROI Profile and quantify"))),
          
          D3TableFilter::d3tfOutput('mtcars',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You can directly edit the signals parameters if you are not satisfied with the calculated parameters. Right now the numbers are still not consistent with the numbers in ROI Profile"))),
          D3TableFilter::d3tfOutput('mtcars2',width = "100%", height = "auto"),
          fluidRow(column(width = 12, h4("You have here some indicators of quality of the quantification"))),
          
          D3TableFilter::d3tfOutput('mtcars3',width = "100%", height = "auto")
          
          )
        )
    ),
    tabPanel("Fitting error values",
      fluidRow(column(width = 12, h4("Here you have the fitting error for every quantification. Press one and go to ROI Testing to analyze the quantification"))),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("fit_selection")
        )
        
      )
      
    ),
    tabPanel("Outliers",
      fluidRow(column(width = 12, h4("Here you have the outliers for every signal and kind of sample. Press one and go to ROI Testing to analyze the quantification"))),
      fluidRow(
        column(width = 12,
          DT::dataTableOutput("quant_selection")
        )
        
      )
      
    ),
    
    
    tabPanel("Univariate analysis",
      fluidRow(column(width = 12, h4("Here you have different kinds of univariate analysis. By now only can analyze between two groups of samples. It cannot analyze differences by treatment, or compare more than two groups of samples. There is an interactive plot of bucket analysis (without FDR at the moment), the p values of quantifications (tests adjusted to normality and variance) and boxplots for every signal for every kind of sample"))),
      fluidRow(
        column(width = 12, h4("Bucket analysis"),
          mainPanel(plotlyOutput("plot_p_value")))    ),
      fluidRow(
        column(width = 12, h4("p values"),
          mainPanel(DT::dataTableOutput("p_value_final")))    ),
      
      fluidRow(
        column(width = 12, h4("Boxplots"),
          mainPanel(plotlyOutput("plot_p_value_2")))    )
      
    )
    
    
    
    
  )
  )
  


server = function(input, output,session) {
  options(shiny.maxRequestSize=1000*1024^2)
  revals <- reactiveValues()
  revals2 <- reactiveValues()
  revals3 <- reactiveValues()
  
  
  
  v <- reactiveValues(meh=NULL, blah = NULL,stop3=0)
  
  sell <- reactiveValues(mtcars=NULL,ind=NULL,beginning=F,dataset=NULL,inFile=NULL,finaloutput=NULL,brks=NULL,brks2=NULL,clrs=NULL,clrs2=NULL,autorun_data=NULL,outlier_table=NULL,ab=NULL,p_value_final=NULL,ROI_data=NULL,info=NULL,ROI_separator=NULL,bucketing=NULL,mediani=NULL)
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
    sell$info=NULL
    

    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    
    revals$mtcars <- sell$mtcars
    revals2$mtcars <- rbind(rep(NA,7),rep(NA,7))
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals3$mtcars <- rbind(rep(NA,3),rep(NA,3))
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
          # print('step1')
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
            # print('step2')
            

          } else{ 
            if(col == 0) {
              
            } else if (col %in% c(1:2,5:11)) {
              revals$mtcars[row, col] <- as.numeric(val)
              sell$roi=1
              # print('step')
              
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
          v$meh=signals_int(sell$autorun_data, sell$finaloutput,sell$ind,revals2$mtcars,revals$mtcars) 

          revals3$mtcars=cbind(v$meh$results_to_save$Area,v$meh$results_to_save$fitting_error,v$meh$results_to_save$signal_area_ratio)
          colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
          
          v$blah$signals_parameters=v$meh$signals_parameters
          v$blah$results_to_save=v$meh$results_to_save
          v$blah$other_fit_parameters=v$meh$other_fit_parameters
          v$blah$p=v$meh$p
          v$blah$Xdata=v$meh$Xdata
          v$blah$Ydata=v$meh$Ydata
          v$blah$finaloutput=v$meh$finaloutput
          v$blah$fitting_type=v$meh$fitting_type
          v$blah$plot_path=v$meh$plot_path
          v$blah$import_excel_profile=v$meh$ROI_profile
          v$blah$signals_codes=v$meh$signals_codes
          
          
        }
        
      })
     
      
      
    })
    
    d3tf(revals2$mtcars,
      tableProps = tableProps,
      enableTf = F,
      edit=TRUE,
      
      tableStyle = "table table-bordered")
    # if (!is.null(val))
    
  })
  
  
  observeEvent(input$action, {
    is_autorun='N'
    if(is.null(sell$info)) sell$ind=input$x1_rows_selected
    if (length(sell$ind)!=1|sell$ind>dim(sell$autorun_data$dataset)[1]) {
      print('Select one valid spectrum')
      return(NULL)
    }
    
    v$blah <- interface_quant(sell$autorun_data, sell$finaloutput, sell$ind,revals$mtcars,is_autorun) 
    v$stop3=1
    
    revals3$mtcars=cbind(v$blah$results_to_save$Area,v$blah$results_to_save$fitting_error,v$blah$results_to_save$signal_area_ratio)
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    
    
    if (!is.null(v$blah$signals_parameters)) {
      revals2$mtcars <- v$blah$signals_parameters
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
    v$stop3=1
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    updateSelectInput(session, "select",selected = NULL
    )
    
    
    
    is_autorun='N'
    if (length(sell$info$row)!=1) {
      return(NULL)
    }
    
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
      plot_data[3, ] * max(Ydata),
      plot_data[2, ] * max(Ydata))
    colnames(plotdata2)=c('Xdata','Ydata',"fitted_sum","baseline_sum")
    plotdata3 <- melt(plotdata2, id = "Xdata")
    plotdata3$variable = c(
      rep('Original Spectrum', length(Ydata)),
      rep('Generated Spectrum', length(Ydata)),
      rep('Generated Background', length(Ydata))
    )
    plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) *
        max(Ydata)))
    colnames(plotdata4)=c('Xdata',dummy[-c(1, 2, 3),1])
    
    plotdata5 = melt(plotdata4, id = "Xdata")
    v$blah$p=ggplot() +
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
      scale_x_reverse() + labs(x='ppm',y='Intensity') + expand_limits(y=0)
    
    r=which(ROI_profile[,4]==sell$autorun_data$signals_names[sell$info$col])
    plotdata = data.frame(Xdata, signals = plot_data[3 + r, ] * max(Ydata))
      v$blah$p=v$blah$p +
        geom_area(
          data = plotdata,
          aes(
            x = Xdata,
            y = signals,
            position = 'fill',
            fill = 'Quantified Signal'
          )
        ) 
    # }
    revals$mtcars=ROI_profile
    revals2$mtcars=t(import(file.path(path,'signals_parameters.csv'))[,-1])
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
   # print(revals2$mtcars)
    revals3$mtcars=cbind(sell$finaloutput$Area[sell$info$row,sell$info$col],sell$finaloutput$fitting_error[sell$info$row,sell$info$col],sell$finaloutput$signal_area_ratio[sell$info$row,sell$info$col])
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    
    
    
  })
  observeEvent(input$quant_selection_cell_clicked, {
    sell$info=input$quant_selection_cell_clicked
    sell$ind=sell$info$row
    v$meh=NULL
    sell$change=1
    sell$stop=0
    sell$change2=1
    sell$stop2=0
    v$stop3=1
    
    
    resetInput(session, "mtcars_edit")
    resetInput(session, "mtcars2_edit")
    updateSelectInput(session, "select",
      selected = NULL
    )
    is_autorun='N'
    if (length(sell$info$row)!=1) {
      return(NULL)
    }
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
      plot_data[3, ] * max(Ydata),
      plot_data[2, ] * max(Ydata))
    colnames(plotdata2)=c('Xdata','Ydata',"fitted_sum","baseline_sum")
    plotdata3 <- melt(plotdata2, id = "Xdata")
    plotdata3$variable = c(
      rep('Original Spectrum', length(Ydata)),
      rep('Generated Spectrum', length(Ydata)),
      rep('Generated Background', length(Ydata))
    )
    plotdata4 = data.frame(Xdata, (t(plot_data[-c(1, 2, 3), , drop = F]) *
        max(Ydata)))
    colnames(plotdata4)=c('Xdata',dummy[-c(1, 2, 3),1])
    
    plotdata5 = melt(plotdata4, id = "Xdata")
    
    v$blah$p=ggplot() +
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
      scale_x_reverse() + labs(x='ppm',y='Intensity') + expand_limits(y=0)
    
    r=which(ROI_profile[,4]==sell$autorun_data$signals_names[sell$info$col])
      plotdata = data.frame(Xdata, signals = plot_data[3 + r, ] * max(Ydata))
      
      v$blah$p=v$blah$p +
        geom_area(
          data = plotdata,
          aes(
            x = Xdata,
            y = signals,
            position = 'fill',
            fill = 'Quantified Signal'
          )
        ) 
    # }
    revals$mtcars=ROI_profile
    revals2$mtcars=t(import(file.path(path,'signals_parameters.csv'))[,-1])
    colnames(revals2$mtcars)=c("intensity",	"shift",	"width",	"gaussian",	"J_coupling",	"multiplicities",	"roof_effect")
    revals3$mtcars=cbind(sell$finaloutput$Area[sell$info$row,sell$info$col],sell$finaloutput$fitting_error[sell$info$row,sell$info$col],sell$finaloutput$signal_area_ratio[sell$info$row,sell$info$col])
    colnames(revals3$mtcars)=c('Quantification','fitting error','signal/total area ratio')
    
    
    
  })
  
  observeEvent(input$autorun, {
    
    is_autorun='Y'
    v$chor <- interface_quant(sell$autorun_data, sell$finaloutput, sell$ind,revals$mtcars,is_autorun) 
    sell$finaloutput=v$chor$finaloutput

    
  })
  
  observeEvent(input$remove_q, {
    if (!is.null(sell$autorun_data$signals_names[sell$info$col])) {
      ind=which(sell$ROI_data[,4]==sell$autorun_data$signals_names[sell$info$col])
    } else {
      ind=as.numeric(input$select)
    }

    sell$finaloutput <- remove_quant(sell$info,sell$autorun_data, sell$finaloutput) 

    sell$outlier_table[,]=0
    ss=unique(sell$autorun_data$Metadata[,1])

      for (j in 1:length(ss)) {
        sell$outlier_table[sell$autorun_data$Metadata==ss[j],][sapply(as.data.frame(sell$finaloutput$Area[sell$autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
       
      }
    t_test_data_2=as.data.frame(sell$finaloutput$Area)
    
    Xwit=cbind(t_test_data_2,factor(sell$autorun_data$Metadata[,1]))
    sell$ab=melt(Xwit)
    colnames(sell$ab)=c('Metadata','Signal','Value')
    
    tt=matrix(NA,length(ss),dim(t_test_data_2)[2])
    for (ind in seq_along(ss)) {
      for (k in 1:dim(t_test_data_2)[2]) {
        tt[ind,k]=tryCatch(shapiro.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
      }
      
    }
    p_value=rep(NA,dim(t_test_data_2)[2])
    for (k in 1:dim(t_test_data_2)[2]) {
      if (!any(tt[,k]<0.05,na.rm=T)) {
        p_value[k]=tryCatch(wilcox.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
      } else {
        p_value[k]=tryCatch(t.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
      }
      
     
    }
    sell$p_value_final=t(as.matrix(p_value))
    colnames(sell$p_value_final)=colnames(t_test_data_2)
    
    
    
  })
  # } 
  observeEvent(input$save_results, {
    if (is.null(v$blah$signals_parameters)&is.null(v$blah$integration_parameters)) {
      print('Incorrect action')
        return(NULL)
    }
   
    sell$finaloutput=save_roi_testing(v$blah,sell$autorun_data, sell$finaloutput) 

    sell$outlier_table[,]=0
    ss=unique(sell$autorun_data$Metadata[,1])

    for (j in 1:length(ss)) {
      sell$outlier_table[sell$autorun_data$Metadata==ss[j],][sapply(as.data.frame(sell$finaloutput$Area[sell$autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
      
    }
    t_test_data_2=as.data.frame(sell$finaloutput$Area)
    
        Xwit=cbind(t_test_data_2,factor(sell$autorun_data$Metadata[,1]))
        sell$ab=melt(Xwit)
        colnames(sell$ab)=c('Metadata','Signal','Value')
        
        t_test_data_2=sell$finaloutput$Area
        tt=matrix(NA,length(ss),dim(t_test_data_2)[2])
        for (ind in seq_along(ss)) {
          for (k in 1:dim(t_test_data_2)[2]) {
            tt[ind,k]=tryCatch(shapiro.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
          }
          
        }
        p_value=rep(NA,dim(t_test_data_2)[2])
        for (k in 1:dim(t_test_data_2)[2]) {
          if (!any(tt[,k]<0.05,na.rm=T)) {
            p_value[k]=tryCatch(wilcox.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
          } else {
            p_value[k]=tryCatch(t.test(t_test_data_2[sell$autorun_data$Metadata[,1]==ss[1],k],t_test_data_2[sell$autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
          }
          
          # }
        }
        sell$p_value_final=t(as.matrix(p_value))
        colnames(sell$p_value_final)=colnames(t_test_data_2)
  
  })
  
  observeEvent(input$save_profile, {
    if (!is.null(sell$info$col)) {
      ind=which(sell$ROI_separator[,1]-sell$info$col>=0)[1]
    } else {
      ind=as.numeric(input$select)
    }
    p
    sell$ROI_data[sell$ROI_separator[ind, 1]:sell$ROI_separator[ind, 2],]=revals$mtcars
    
    write.csv(sell$ROI_data,sell$autorun_data$profile_folder_path,row.names=F)

    
    
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
  
  
  output$p_value_final = DT::renderDataTable(round(sell$p_value_final,3),selection = list(mode = 'multiple', selected = 1),server = T)
  
  

  
  output$plot <- renderPlotly({
    # 
    # print(sell$info)
    # print(v$stop3)
    if ((v$stop3==0&(is.null(sell$info))|(v$stop3==0&length(input$x1_rows_selected)>1))) {
      lol=which(round(sell$autorun_data$ppm,6)==round(sell$mtcars[1,1],6))
      lol2=which(round(sell$autorun_data$ppm,6)==round(sell$mtcars[1,2],6))
      
      plotdata = data.frame(Xdata=sell$autorun_data$ppm, t(sell$dataset[input$x1_rows_selected,,drop=F]))
      
      plotdata3 <- melt(plotdata, id = "Xdata")
      plot_ly(data=plotdata3,x=~Xdata,y=~value,color=~variable,type='scatter',mode='lines') %>% layout(xaxis = list(range = c(round(sell$mtcars[1,1],6), round(sell$mtcars[1,2],6))),yaxis = list(range = c(0, max(sell$dataset[input$x1_rows_selected,lol:lol2]))))
    } else {
      ggplotly(v$blah$p) 
      

    }
  })
  output$quant_selection = DT::renderDataTable({ dat <- datatable(sell$outlier_table,selection = list(mode = 'single', target = 'cell')) %>% formatStyle(colnames(sell$outlier_table), backgroundColor = styleInterval(sell$brks2, sell$clrs2))
  return(dat)
  })
  
  output$fit_selection = DT::renderDataTable({ dat <- datatable(round(sell$finaloutput$fitting_error,2),selection = list(mode = 'single', target = 'cell')) %>% formatStyle(colnames(sell$finaloutput$fitting_error), backgroundColor = styleInterval(sell$brks, sell$clrs))
  return(dat)
  })
  output$plot_p_value <- renderPlotly({
    plot_ly(data=sell$bucketing,x=~Xdata,y=~intensity,color=~pvalue,type='scatter',mode='lines') %>% layout(xaxis = list(autorange = "reversed"),yaxis = list(range = c(0, max(sell$bucketing$intensity))))
    
  })
  
  
  output$plot_p_value_2 <- renderPlotly({
    plot_ly(sell$ab, x = ~Signal, y = ~Value, color = ~Metadata, type = "box") %>%
      layout(boxmode = "group")
  })
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
    sell$info=NULL
    
    
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
    rownames(dummy) = imported_data$Experiments
    colnames(dummy) = imported_data$signals_names
    sell$finaloutput$Area = sell$finaloutput$signal_area_ratio = sell$finaloutput$fitting_error =
      sell$finaloutput$shift = sell$finaloutput$intensity = sell$finaloutput$width = dummy
    
    #creation of several outputs with data of interest before beginnig the quantification
    write.csv(
      as.data.frame(imported_data$params),
      file.path(imported_data$export_path, 'initialparams.csv'),
      row.names = F
    )
    colnames(imported_data$dataset) = imported_data$ppm
    rownames(imported_data$dataset) = imported_data$Experiments
    write.csv(imported_data$dataset,
      file.path(imported_data$export_path, 'initialdataset.csv'))
    if ("not_loaded_experiments" %in% names(imported_data))
      write.table(
        imported_data$not_loaded_experiments,
        file.path(imported_data$export_path, 'not_loaded_experiments.csv'),
        row.names = F,
        col.names = F
      )
    #creation of list of necessary parameters for automatic quantification
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
    
    
    
    sell$finaloutput = autorun(sell$autorun_data, sell$finaloutput)
      
    other_fit_parameters = fitting_variables()
    
    
    sell$ROI_data = read.csv(sell$autorun_data$profile_folder_path, stringsAsFactors = F)
    dummy = which(!is.na(sell$ROI_data[, 1]))
    sell$ROI_separator = cbind(dummy, c(dummy[-1] - 1, dim(sell$ROI_data)[1]))
    # mtcars2=ROI_data[1:2,4:11]
    # mtcars=ROI_data[1:2,4:11]
    
    ROI_names=paste(sell$ROI_data[sell$ROI_separator[, 1],1],sell$ROI_data[sell$ROI_separator[, 1],2])
    select_options=1:length(ROI_names)
    names(select_options)=ROI_names
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
    p_value_bucketing[is.na(p_value_bucketing)]=0
    plotdata = data.frame(Xdata=sell$autorun_data$ppm, p_value_bucketing)
    sell$mediani=apply(sell$autorun_data$dataset,2,function(x) median(x,na.rm=T))
    # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
    sell$bucketing <- cbind(melt(plotdata, id = "Xdata"),sell$mediani)
    colnames(sell$bucketing)=c('Xdata','variable','pvalue','intensity')
    
    t_test_data_2=sell$finaloutput$Area
    # t_test_data_2[sell$finaloutput$fitting_error>other_fit_parameters$fitting_error_limit]=NA
    # t_test_data_2[sell$finaloutput$signal_area_ratio<other_fit_parameters$signal_area_ratio_limit]=NA
    
    ll=as.data.frame(t_test_data_2)
    Xwit=cbind(ll,factor(sell$autorun_data$Metadata[,1]))
    # rownames(Xwit)=NULL
    sell$ab=melt(Xwit)
    colnames(sell$ab)=c('Metadata','Signal','Value')
    sell$outlier_table=matrix(0,dim(ll)[1],dim(ll)[2])
    sell$outlier_table=as.data.frame(sell$outlier_table)
    
    colnames(sell$outlier_table)=colnames(t_test_data_2)
    rownames(sell$outlier_table)=rownames(sell$finaloutput$fitting_error)
    
    
    for (j in 1:length(ss)) {
      sell$outlier_table[sell$autorun_data$Metadata==ss[j],][sapply(as.data.frame(sell$finaloutput$Area[sell$autorun_data$Metadata==ss[j],]), function(x)x %in% boxplot.stats(x)$out)]=1
      
      # ind=which(autorun_data$Metadata==ss[j])
      # sell$outlier_table[ind[sell$finaloutput$Area[autorun_data$Metadata==ss[j],i] %in%  outliers],i]=1
    }
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
    sell$p_value_final=t(as.matrix(p_value))
    colnames(sell$p_value_final)=colnames(t_test_data_2)
    
    

      is_autorun='Y'
      sell$dataset=rbind(sell$autorun_data$dataset,colMeans(sell$autorun_data$dataset),apply(sell$autorun_data$dataset,2,median))
      rownames(sell$dataset)[(dim(sell$autorun_data$dataset)[1]+1):dim(sell$dataset)[1]]=c('Mean spectrum', 'Median spectrum')
      mm=matrix(NA,2,dim(sell$autorun_data$Metadata)[2])
      colnames(mm)=colnames(sell$autorun_data$Metadata)
      spectra=cbind(as.matrix(rownames(sell$dataset)),rbind(sell$autorun_data$Metadata,mm))
      # rownames(spectra)=ll
      colnames(spectra)=c('spectrum','Metadata')
      sell$brks <- quantile(sell$finaloutput$fitting_error, probs = seq(.05, .95, .05), na.rm = TRUE)
      sell$clrs <- round(seq(255, 40, length.out = length(sell$brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      sell$brks2 <- 0.5
      sell$clrs2 <- round(seq(255, 40, length.out = length(sell$brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      output$x1 = DT::renderDataTable(
        
        spectra , selection = list(mode = 'multiple', selected = 1),server = T)
      
      sell$beginning =T
      updateSelectInput(session, "select",
          choices = select_options,selected = 1
        )
      session$sendCustomMessage('activeNavs', 'ROI Testing')
      session$sendCustomMessage('activeNavs', 'Fitting error values')
      session$sendCustomMessage('activeNavs', 'Outliers')
      session$sendCustomMessage('activeNavs', 'Univariate analysis')
      
  })
  
  # observeEvent(input$file2, {
  #   sell$inFile <- input$file2
  #   
  #   if (is.null(sell$inFile))
  #     return(NULL)
  #   
  #   load(sell$inFile$datapath)
  #   sell<-trek
  #   rm(trek)
  #   
  #   session$sendCustomMessage('activeNavs', 'ROI Testing')
  #   session$sendCustomMessage('activeNavs', 'Fitting error values')
  #   session$sendCustomMessage('activeNavs', 'Outliers')
  #   session$sendCustomMessage('activeNavs', 'Univariate analysis')
  #   
  # })
  
  # observeEvent(input$download, {
  #   
  # myfile=file.choose()
  # trek<<-sell
  # save.image('man.RData')
  # rm(trek)
  # myfile=NULL
  # })
  
}

shinyApp(ui, server)


