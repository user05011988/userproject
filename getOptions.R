getOptions=function() {
  control <- nls.lm.control()
  control$factor <- 1e-2
  control$maxiter=500
  control$ftol=1e-10
  control$ptol=1e-10
  # control$diag=scalator
return(control)
}
