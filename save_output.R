save_output=function(spectrum_index,signals_codes,results_to_save,buck_step,finaloutput) {

  
# if (results_to_save$Area!=0) {


             finaloutput$Area[spectrum_index,signals_codes]=results_to_save$Area*buck_step
             finaloutput$fitting_error[spectrum_index,signals_codes]=results_to_save$fitting_error 
             finaloutput$signal_area_ratio[spectrum_index,signals_codes]=results_to_save$signal_area_ratio
             finaloutput$shift[spectrum_index,signals_codes]=results_to_save$shift
# }
      return(finaloutput)       # RB.data(index,codes(i))=gaussian(1,i);  
}