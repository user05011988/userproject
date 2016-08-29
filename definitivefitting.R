definitivefitting= function(signals_parameters,Xdata) { 
  #ep! canvi taulada!
# signals_parameters=t(signals_parameters)
multiplicities=signals_parameters[6,]
roof_effect=signals_parameters[7,]
# background_signals = matrix(NaN,dim(signals_parameters)[2],length(Xdata))
fitted_signals = matrix(NaN,dim(signals_parameters)[2],length(Xdata))

#DANIEL - 15/05/17 - Deixo les línies en un format molt més curt i que
#em fa ent}re molt més fàcilment el que està succeint. He tret les 150 línies que havia abans.
for (s in seq_along(multiplicities)) {
 if (roof_effect[s]<0) {# ep! canvi taulada!
     if (multiplicities[s]==1)   {
       fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s], signals_parameters[2,s], signals_parameters[3,s], signals_parameters[4,s]),Xdata)
     } else if (multiplicities[1,s]==2){
       fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s], signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]*(1-abs(roof_effect[s])), (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s], signals_parameters[4,s]),Xdata)
    } else if (multiplicities[s]==3) {
      fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]*(1-abs(roof_effect[s])), (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s], signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]*abs(roof_effect[s]), (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s], signals_parameters[4,s]),Xdata)
    } else if (multiplicities[s]==4) {
      fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]/3, (signals_parameters[2,s]-3*signals_parameters[5,s]), signals_parameters[3,s], signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]/3, (signals_parameters[2,s]+3*signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)
    }
      } else if (roof_effect[s]==0) {
        if (multiplicities[s]==100){
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==1) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==2) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==3) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]/2, (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]/2, (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==4) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]/3, (signals_parameters[2,s]-3*signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s] ,(signals_parameters[2,s]-signals_parameters[5,s]) ,signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]/3, (signals_parameters[2,s]+3*signals_parameters[5,s]) ,signals_parameters[3,s],signals_parameters[4,s]),Xdata)
  } } else if (roof_effect[s]>0) {
        if (multiplicities[s]==1) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)  
        } else if (multiplicities[s]==2) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]*(1-roof_effect[s]), (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s] ,(signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==3) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]*(roof_effect[s]), (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s],signals_parameters[2,s],signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]*(1-roof_effect[s]), (signals_parameters[2,s]+signals_parameters[5,s]) ,signals_parameters[3,s],signals_parameters[4,s]),Xdata)
        } else if (multiplicities[s]==4) {
          fitted_signals[s,] = peakpvoigt(c(signals_parameters[1,s]/3 ,(signals_parameters[2,s]-3*signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]-signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s], (signals_parameters[2,s]+signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)+peakpvoigt(c(signals_parameters[1,s]/3, (signals_parameters[2,s]+3*signals_parameters[5,s]), signals_parameters[3,s],signals_parameters[4,s]),Xdata)
  }
  } 
}
# multiplicities=
# signals2=fitted_signals[complete.cases(fitted_signals),]
# dim(fitted_signals)=c()
# background_signals=background_signals[complete.cases(background_signals),]
# 
# fitted_signals=list(fitted_signals=fitted_signals, background_signals=background_signals)


return(fitted_signals)
}
