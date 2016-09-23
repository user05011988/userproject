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

  source("peakpvoigt.R")
  source("fitting_optimization.R")
  source("fittingloop.R")
  source("fittingloop_bg.R")
  source("definitivefitting.R")
  source("outputgenerator.R")
  source("plotgenerator.R")
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


}
