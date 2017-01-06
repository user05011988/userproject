save_output=function(spectrum_index,signals_codes,results_to_save,buck_step,finaloutput) {
  #Created by Daniel Cañueto 30/08/2016
  #Save quantification, shift, fitting error and signal area ratio.
  
             finaloutput$Area[spectrum_index,signals_codes]=results_to_save$Area*buck_step
             finaloutput$correlation[spectrum_index,signals_codes]=results_to_save$correlation 
             finaloutput$signal_area_ratio[spectrum_index,signals_codes]=results_to_save$signal_area_ratio
             finaloutput$shift[spectrum_index,signals_codes]=results_to_save$shift
             finaloutput$intensity[spectrum_index,signals_codes]=results_to_save$intensity
             finaloutput$width[spectrum_index,signals_codes]=results_to_save$width
             
      return(finaloutput)         
}