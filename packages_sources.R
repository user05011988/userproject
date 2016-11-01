packages_sources = function() {
  #Created by Daniel Ca?ueto 07/09/2016
  #Loading of libraries and functions

  # requireNamespace("minpack.lm", quietly = TRUE)
  # requireNamespace("reshape", quietly = TRUE)
  # requireNamespace("ggplot2", quietly = TRUE)
  # requireNamespace("compiler", quietly = TRUE)
  # requireNamespace("robust", quietly = TRUE)
  # requireNamespace("apcluster", quietly = TRUE)
  # requireNamespace("rio", quietly = TRUE)
  
   library("minpack.lm")
  library("reshape")
  library("ggplot2")
  library("compiler")
  library("robust")
  library("rio")
  library("png")
  library(plotly)
  library(DT)
  library(D3TableFilter)
  library(shiny)
  
  library("lazyeval")
  
  
  source('sign_par.R')
  source('signals_int.R')
  source("interface_quant.R")
  source("save_roi_testing.R")
  source("remove_quant.R")
  source("fittingloop.R")
  source("fittingloop_bg.R")
  source("outputgenerator.R")
  source("plotgenerator.R")
  source("interface_quant.R")
  source("fitting_optimization.R")
  source("peakpvoigt.R")
  source('autorun_model_spectrum.R')
  source("peakdet.R")
  source("fitting_prep.R")
  source("integration.R")
  source("autorun.R")
  source("Metadata2Buckets.R")
  source("readspectrum.R")
  source("jtp.R")
  source("import_data.R")
  source("save_output.R")
  source("fitting_variables.R")
  source("automatic_roi_edition.R")
  source("validation.R")
  source("plele.R")
  source("interface_integration.R")
  
  

}
