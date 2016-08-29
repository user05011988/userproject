#Aquesta és la funció adaptada i R.
fitting_optimization = function(parS,xx) {
  i=as.numeric(parS[seq(1,length(parS)-6,7)])
  p=as.numeric(parS[seq(2,length(parS)-5,7)])
  w=as.numeric(parS[seq(3,length(parS)-4,7)])
  g=as.numeric(parS[seq(4,length(parS)-3,7)])
  j=as.numeric(parS[seq(5,length(parS)-2,7)])
  multiplicities=as.numeric(parS[seq(6,length(parS)-1,7)])
  roof_effect=as.numeric(parS[seq(7,length(parS)-0,7)])
  NumSignals=length(parS)/7
  Xdata=xx
  
  F=0
  
  for (s in 1:NumSignals) {
    
    if (roof_effect[s]<0) { # ep! canvi taulada!
      if(multiplicities[s]==1) {
        signal = peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if(multiplicities[s]==2) {
        signal = peakpvoigt(c(i[s], p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]*(1-abs(roof_effect[s])), p[s]+j[s], w[s], g[s]),Xdata) #ep! canvi taulada!
        F = F+signal
      } else if(multiplicities[s]==3) {
        signal = peakpvoigt(c(i[s]*(1-abs(roof_effect[s])), p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]*abs(roof_effect[s]), p[s]+j[s], w[s], g[s]),Xdata) #ep! canvi taulada!
        F = F+signal
      } else if(multiplicities[s]==4) {
        signal = peakpvoigt(c(i[s]/3, p[s]-3*j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]*(1-abs(roof_effect[s])), p[s]+j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]/3*(1-abs(roof_effect[s])), p[s]+3*j[s], w[s], g[s]),Xdata)
        F = F+signal
      }
    } else if (roof_effect[s]==0) {
      if(multiplicities[s]==100) {
        signal = peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if(multiplicities[s]==1) {
        signal = peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==2) {
        signal = peakpvoigt(c(i[s], p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]+j[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==3) {
        signal = peakpvoigt(c(i[s]/2, p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]/2, p[s]+j[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==4) {
        signal = peakpvoigt(c(i[s]/3, p[s]-3*j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]+j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]/3, p[s]+3*j[s], w[s], g[s]),Xdata)
        F = F+signal
      }
    } else if (roof_effect[s]>0) {
      if(multiplicities[s]==1) {
        signal = peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==2) {
        signal = peakpvoigt(c(i[s]*(1-roof_effect[s]), p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]+j[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==3) {
        signal = peakpvoigt(c(i[s]*(roof_effect[s]), p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]*(1-roof_effect[s]), p[s]+j[s], w[s], g[s]),Xdata)
        F = F+signal
      } else if (multiplicities[s]==4) {
        signal = peakpvoigt(c(i[s]/3*(1-roof_effect[s]), p[s]-3*j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]*(1-roof_effect[s]), p[s]-j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s], p[s]+j[s], w[s], g[s]),Xdata)+peakpvoigt(c(i[s]/3, p[s]+3*j[s], w[s], g[s]),Xdata)
        F = F+signal
      }
    }
  }
    F
    

}