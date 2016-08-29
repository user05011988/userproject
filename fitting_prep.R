fitting_prep=function(Xdata,Ydata,initial_fit_parameters,other_fit_parameters) {
#aquesta funció prepara les condicions necessàries per fer el fitting_parameters de
#senyals

  signals_to_fit = length(initial_fit_parameters$positions)
ROIlength=length(Xdata)
for (i in 1:signals_to_fit) initial_fit_parameters$Jcoupling[i]= ifelse((initial_fit_parameters$multiplicities[i]==1 | initial_fit_parameters$multiplicities[i]==3),initial_fit_parameters$Jcoupling[i]/other_fit_parameters$freq,(initial_fit_parameters$Jcoupling[i]/other_fit_parameters$freq)/2)


#canvio de posició lo de assegurar que tots els punts siguin positius, per
#així assegurar que les senyals de background estan ben fetes.

BGSigNum=ifelse(other_fit_parameters$clean_fit=='N',max(round((Xdata[1]-Xdata[ROIlength])*30),2),0)
 #si hi ha un baseline fitting_parameters
    
		
FeaturesMatrix=matrix(NA,(signals_to_fit+BGSigNum),12)
colnames(FeaturesMatrix)=c('minimum_intensity','maximum_intensity','shift_left_limit','shift_right_limit','minimum_width','maximum_width','minimum_gaussian','maximum_gaussian','minimum_J_coupling','maximum_J_coupling','multiplicities','roof_effect')
        
#preparació dels paràmetres de les senyals a fitar
    FeaturesMatrix[1:signals_to_fit,1]=0
    FeaturesMatrix[1:signals_to_fit,2]=max(Ydata)
    FeaturesMatrix[1:signals_to_fit,3]=initial_fit_parameters$positions-initial_fit_parameters$shift_tolerance
    FeaturesMatrix[1:signals_to_fit,4]=initial_fit_parameters$positions+initial_fit_parameters$shift_tolerance            
    FeaturesMatrix[1:signals_to_fit,5]=initial_fit_parameters$widths/other_fit_parameters$freq-((initial_fit_parameters$widths/other_fit_parameters$freq)/5)
    FeaturesMatrix[1:signals_to_fit,6]=initial_fit_parameters$widths/other_fit_parameters$freq+((initial_fit_parameters$widths/other_fit_parameters$freq)/5)
    FeaturesMatrix[1:signals_to_fit,7]=0
    FeaturesMatrix[1:signals_to_fit,8]=0.2                    
    FeaturesMatrix[1:signals_to_fit,9]=initial_fit_parameters$Jcoupling-0.0003
    FeaturesMatrix[1:signals_to_fit,10]=initial_fit_parameters$Jcoupling+0.0003
    FeaturesMatrix[1:signals_to_fit,11]=initial_fit_parameters$multiplicities
    FeaturesMatrix[1:signals_to_fit,12]=initial_fit_parameters$roof_effect
    

#preparació dels paràmteres de background si hi ha baseline fitting_parameters
if (other_fit_parameters$clean_fit=='N') {
    BGSigrightlimits=seq(Xdata[1],Xdata[ROIlength],length=BGSigNum)-0.005
    BGSigleftlimits=BGSigrightlimits+0.01
    #aquí es troba el màxim possible d'intensitat de background
    peaks=peakdet(Ydata,0.01)
    left=which(peaks$mintab$pos<ROIlength/5)
    right=which(peaks$mintab$pos>4*ROIlength/5)
    # minIBGleft=ifelse(length(left)==0,Ydata[1],min(peaks$mintab$val[left]))
    # minIBGright=ifelse(length(right)==0,Ydata[ROIlength],min(peaks$mintab$val[right]))
    dummy=round(seq(1,ROIlength,length=2*BGSigNum-1))
    BGleftlimits=dummy[c(1,seq(2,length(dummy)-1,2))]
    BGrightlimits=dummy[c(seq(2,length(dummy)-1,2),length(dummy))]
    minims=replicate(BGSigNum,NA)
    for (ss in 1:BGSigNum) minims[ss]=min(Ydata[BGleftlimits[ss]:BGrightlimits[ss]])
    
    
    for (i in 1:BGSigNum) {
      FeaturesMatrix[signals_to_fit+i,1]=0
      # FeaturesMatrix[signals_to_fit+i,2]=max(minIBGleft, minIBGright)
      FeaturesMatrix[signals_to_fit+i,2]=minims[i]
      FeaturesMatrix[signals_to_fit+i,3]=BGSigrightlimits[i]
      FeaturesMatrix[signals_to_fit+i,4]=BGSigleftlimits[i]             
      FeaturesMatrix[signals_to_fit+i,5]=(1.5/other_fit_parameters$freq)*20
      FeaturesMatrix[signals_to_fit+i,6]=(1.5/other_fit_parameters$freq)*25
      FeaturesMatrix[signals_to_fit+i,7]=0
      FeaturesMatrix[signals_to_fit+i,8]=1                    
      FeaturesMatrix[signals_to_fit+i,9]=0
      FeaturesMatrix[signals_to_fit+i,10]=0.001
      FeaturesMatrix[signals_to_fit+i,11]=100
      FeaturesMatrix[signals_to_fit+i,12]=0
        
        
  }
}

    # END OF DANIEL - 2015/22/07 - Change of structure of the FeaturesMatrix of the Background Signals, also with change of the location of the background signals of the maximum intensity and of width of the signals.


		
    # signals_to_quantify=which(initial_fit_parameters$quantification_or_not==1)


#aquí es crida la funció de fitting_parameters
# [~, ~, Area features , ~, errors1 errors2 errors3]=fit_general(RegionX,RegionY,ErrorMax,E_max,...
# P_max,FeaturesMatrix,initial_fit_parameters$multiplicities,dim(FeaturesMatrix,1),ppmX,signal_index2,axe_1D,initial_fit_parameters$roof_effect,codes,RA,index,...
# plots_path,initial_fit_parameters$shift_tolerance,metnames,isroitest)

# fitting_parameters=list(signals_to_quantify=signals_to_quantify,FeaturesMatrix=FeaturesMatrix,initial_fit_parameters$multiplicities=initial_fit_parameters$multiplicities ,initial_fit_parameters$roof_effect=initial_fit_parameters$roof_effect)

return(FeaturesMatrix)
}