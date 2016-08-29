Metadata2Buckets <- function(Experiments, params) {
  
  # FUNCTIONALITY Routine used to generate a dataset for further data
  # analysis the parameter metadata_path is an Excel file with the
  # metadata_paths to read directly from topspin and the respective
  # metadata the excel file includes as many rows as experiments to read,
  # plus a first row with the class names of the associated metadata
  # labels. the fist column of the excel file corresponds with the
  # associates topspin experiment metadata_path while the rest of columns
  # are the metadata information PROBLEMS: ?
  
  # AUTHOR: Nicolau Cañellas Modified by: Last modified: 02/09/2008 Based
  # on: Spectra2BucketsDataset
  
  # DEPENDENCY REPORT M-filesep (called functions) Spectra2Dataset
  # current dir : topspin_read_spectrum toolbox :
  # pls_toolbox_420/@dataset/dataset.m
  
  
  CURRENT = list()
  RAW = list()
  not_loaded_experiments = read_spectra = c()
  
  # Meta=read.delim(metadata_path, 1)
  # Meta=Meta[complete.cases(Meta[,1]),]
  Meta = Experiments
  # if (nargs<2) params<-setimportparams
  a = params$dir
  expno = params$expno
  processingno = params$processingno
  left_spectral_border = ifelse(exists("left_spectral_border", where = params), 
                                params$left_spectral_border, 12)
  right_spectral_border = ifelse(exists("right_spectral_border", where = params), 
                                 params$right_spectral_border, -1)
  norm_PEAK = params$norm_PEAK
  RAW$norm_PEAK_left_ppm = params$norm_PEAK_left_ppm
  RAW$norm_PEAK_right_ppm = params$norm_PEAK_right_ppm
  
  AREAnorm = params$norm_AREA
  RAW$norm_AREA_left_ppm = ifelse(toupper(AREAnorm) == "Y", params$norm_AREA_left_ppm, 
                                  0)
  RAW$norm_AREA_right_ppm = ifelse(toupper(AREAnorm) == "Y", params$norm_AREA_right_ppm, 
                                   0)
  
  glucose_alignment = params$glucose_alignment
  tsp_alignment = params$tsp_alignment
  peak_alignment = params$peak_alignment
  ref_peak_pos = ifelse(toupper(peak_alignment) == "Y", params$ref_peak_pos, 
                        0)
  
  water_suppression = params$water_suppression
  RAW$water_suppression_left_ppm = ifelse(toupper(water_suppression) == 
                                            "Y", params$water_suppression_left_ppm, 0)
  RAW$water_suppression_right_ppm = ifelse(toupper(water_suppression) == 
                                             "Y", params$water_suppression_right_ppm, 0)
  disol_suppression = params$disol_suppression
  if (toupper(disol_suppression) == "Y") {
    RAW$disol_suppression_ppm = params$disol_suppression_ppm
  } else {
    RAW$disol_suppression_ppm = matrix(0, 1, 2)
  }
  buck_step = params$buck_step
  name = paste(a, "nmr", sep = "/")
  l1 = as.character(expno)
  l2 = as.character(processingno)
  d = Meta
  maxspec = length(d)
  k2 = 1
  
  RAW$len = 0
  for (k in 1:maxspec) {
    i1 = as.character(d[k])
    filename = paste(name, i1, l1, "pdata", l2, sep = "/")
    partname = paste(name, i1, l1, sep = "/")
    storedpars = topspin_read_spectrum2(partname, filename, -2, 13)
    if (all(is.nan(storedpars$real)) == 0) {
      CURRENT$minppm = storedpars$OFFSET - storedpars$SW
      CURRENT$maxppm = storedpars$OFFSET
      CURRENT$step = storedpars$SW/(length(storedpars$real) - 1)
      CURRENT$ppm = seq(CURRENT$maxppm, CURRENT$minppm, -CURRENT$step)
      
      if (RAW$len == 0) {
        RAW$len = length(storedpars$real)
        RAW$ofs = storedpars$OFFSET
        RAW$sw = storedpars$SW
        RAW$ncproc = storedpars$NC_proc
        RAW$minppm = storedpars$OFFSET - storedpars$SW
        RAW$maxppm = storedpars$OFFSET
        RAW$size = length(storedpars$real)
        RAW$step = RAW$sw/(RAW$size - 1)
        RAW$ppm = seq(RAW$maxppm, RAW$minppm, -RAW$step)
        RAW$buck_step = ifelse(buck_step < RAW$step, RAW$step, 
                               buck_step)
        # redieixo la part dreta de l'espectre de Matlab RAW$ppm_bucks =
        # left_spectral_border:-RAW$buck_step:-2
        RAW$ppm_bucks = seq(left_spectral_border, right_spectral_border, 
                            -RAW$buck_step)
        RAW$len_bucks = length(RAW$ppm_bucks)
        RAW$water_suppression_left_buck = 1 + round(-(RAW$water_suppression_left_ppm - 
                                                        RAW$ppm_bucks[1])/RAW$buck_step)
        RAW$water_suppression_right_buck = 1 + round(-(RAW$water_suppression_right_ppm - 
                                                         RAW$ppm_bucks[1])/RAW$buck_step)
        DIM = dim(RAW$disol_suppression_ppm)
        RAW$disol_suppression_bucks = RAW$disol_suppression_ppm
        for (nr in 1:DIM[1]) {
          for (nc in 1:DIM[2]) {
            RAW$disol_suppression_bucks[nr, nc] = 1 + round(-(RAW$disol_suppression_ppm[nr, 
                                                                                        nc] - RAW$ppm_bucks[1])/RAW$buck_step)
          }
        }
        
        RAW$norm_PEAK_max = 0
        RAW$norm_AREA = 0
        RAW$total_AREA_mean = 0
        RAW$differential_norm_AREA = 0
      }
      # a partir d'ara escalarem pel guany tmp
      # =(storedpars$real.*(2^(storedpars$NC_proc)))
      tmp = (storedpars$real * ((2^storedpars$NC_proc)/storedpars$RG))
      # tmp1 =(storedpars$real.*((2^(storedpars$NC_proc))))
      CURRENT$left_spectral_border = 1 + round(-(left_spectral_border - 
                                                   CURRENT$maxppm)/CURRENT$step)
      CURRENT$norm_PEAK_left = 1 + round(-(RAW$norm_PEAK_left_ppm - 
                                             CURRENT$maxppm)/CURRENT$step)
      CURRENT$norm_PEAK_right = 1 + round(-(RAW$norm_PEAK_right_ppm - 
                                              CURRENT$maxppm)/CURRENT$step)
      CURRENT$norm_AREA_left = 1 + round(-(RAW$norm_AREA_left_ppm - 
                                             CURRENT$maxppm)/CURRENT$step)
      CURRENT$norm_AREA_right = 1 + round(-(RAW$norm_AREA_right_ppm - 
                                              CURRENT$maxppm)/CURRENT$step)
      norm_PEAK_max = max(tmp[CURRENT$norm_PEAK_left:CURRENT$norm_PEAK_right])
      norm_PEAK_max_position = which.max(tmp[CURRENT$norm_PEAK_left:CURRENT$norm_PEAK_right])
      # [norm_PEAK_max, norm_PEAK_max_position] =
      # interp1_max(tmp(CURRENT$norm_PEAK_left:CURRENT$norm_PEAK_right))
      norm_AREA = sum(tmp[CURRENT$norm_AREA_left:CURRENT$norm_AREA_right])
      total_AREA_mean = mean(tmp[1:length(tmp)])
      # normalitzem per pic TSP
      if (RAW$norm_PEAK_max == 0)  RAW$norm_PEAK_max = norm_PEAK_max
      if (RAW$total_AREA_mean == 0) RAW$total_AREA_mean = total_AREA_mean
      if (RAW$norm_AREA == 0) RAW$norm_AREA = norm_AREA
      if (toupper(norm_PEAK) == "Y") {
        if (norm_PEAK_max > 0) 
          tmp = tmp/norm_PEAK_max
      }
      # normalitzem per area
      if (toupper(AREAnorm) == "Y") {
        if (norm_AREA > 0) 
          tmp = tmp/norm_AREA
        # elseif (upper(norm_diff_AREA) == 'Y') if (differential_norm_AREA > 0)
        # tmp = (tmp) ./ differential_norm_AREA end
      }
      # calculem valors despres de normalitzar
      norm_PEAK_max2 = max(tmp[CURRENT$norm_PEAK_left:CURRENT$norm_PEAK_right])
      norm_AREA2 = sum(tmp[CURRENT$norm_AREA_left:CURRENT$norm_AREA_right])
      # referenciem
      LeftEreticBefore=ifelse(left_spectral_border > 11.8,min(CURRENT$ppm[which(CURRENT$ppm > 11.7)]),NaN)
      
      if (toupper(glucose_alignment) == "Y") {
        JTP = JTPcalibrateToGlucose(tmp, CURRENT$ppm)
      }
      if (toupper(tsp_alignment) == "Y") {
        JTP = JTPcalibrateToTSP(tmp, CURRENT$ppm)
      }
      if (toupper(peak_alignment) == "Y") {
        if (nrow(as.matrix(ref_peak_pos)) == maxspec) {
          JTP = JTPcalibrateToPeak2(tmp, CURRENT$ppm, 5.3, 0.4)
          JTP = JTPcalibrateToPeak(tmp, CURRENT$ppm, ref_peak_pos[k])
        } else JTP = JTPcalibrateToPeak(tmp, CURRENT$ppm, ref_peak_pos)
      }
      CURRENT$ppm = JTP$ppm
      # si hi llegim zona d'eretic llavors tornem a posar Eretic a 11 ppm
      if (!is.nan(LeftEreticBefore)) {
        LeftEreticAfter = min(CURRENT$ppm[which(CURRENT$ppm > 11.7)])
        RightEreticAfter = min(CURRENT$ppm[which(CURRENT$ppm > 10.8)])
        RightEreticBefore = LeftEreticBefore + RightEreticAfter - LeftEreticAfter
        tmp[LeftEreticAfter:RightEreticAfter] = tmp[LeftEreticBefore:RightEreticBefore]
      }
      
     
      
      fill2end = 0
      tmp_count = CURRENT$left_spectral_border
      tmp_buck = matrix(0, RAW$len_bucks, 1)
      jumped_bucket = 0
      for (tmp_buck_count in 2:RAW$len_bucks) {
        items = suma = 0
        while (CURRENT$ppm[tmp_count] > RAW$ppm_bucks[tmp_buck_count]) {
          items = items + 1
          suma = suma + tmp[tmp_count]
          tmp_count = tmp_count + 1
          if (tmp_count > length(CURRENT$ppm)) {
            fill2end = 1
            break
          }
        }
        
        # faig una modificació per a corregir buckets sense dades.
        if (items > 0) {
          tmp_buck[tmp_buck_count - 1] = suma/items
          while (jumped_bucket > 0) {
            tmp_buck[tmp_buck_count - 1 - jumped_bucket] = suma/items
            jumped_bucket = jumped_bucket - 1
          }
        } else {
          jumped_bucket = jumped_bucket + 1
        }
        if (fill2end == 1) {
          while (tmp_buck_count <= RAW$len_bucks) {
            tmp_buck[tmp_buck_count] = tmp_buck[tmp_buck_count - 
                                                  1]
            tmp_buck_count = tmp_buck_count + 1
          }
          break
        }
      }
      while (length(tmp_buck) < RAW$len_bucks) tmp_buck = c(tmp_buck, tmp_buck[length(tmp_buck)])
     
            # eliminem aigua i disolvents després de generar buckets si normalitzem
      # per àrea total això caldria fer-ho abans, o no considerar aquestes
      # zones a l'hora de calcular l'àrea si cal eliminem l'aigua if
      
      if (toupper(disol_suppression) == "Y") {
        for (nunrows in 1:D_rows) {
          tmp_buck[RAW$disol_suppression_bucks[nunrows, 1]:RAW$disol_suppression_bucks[nunrows, 
                                                                                       2]] = 0
        }
      }
      
      # calculem el valor d'àrea total després de suprimir l'aigua
      total_AREA_mean2 = mean(tmp_buck[1:length(tmp_buck)])
      
      if (k2 == 1) dataset = matrix(NA, maxspec, RAW$len_bucks)
      dataset[k2, ] = t(tmp_buck[1:RAW$len_bucks])
      k2 = k2 + 1
      read_spectra = append(read_spectra, as.character(d[k]))
      # LocatedSignals=rbind(LocatedSignals, signals[1,]) guardar el factor
      # de normalitzacio
      
      
      
    } else {
      # llista d'objectes no inclosos
      not_loaded_experiments = append(not_loaded_experiments, d[k])
    }
  }
  finaldata = list()
  finaldata$params = params
  finaldata$RAW = RAW
  dataset = dataset[complete.cases(dataset), ]
  rownames(dataset) = read_spectra
  finaldata$dataset = dataset
  finaldata$not_loaded_experiments = not_loaded_experiments
  colnames(finaldata$dataset) = as.character(RAW$ppm_bucks)
  finaldata$ppm = RAW$ppm_bucks
  
  return(finaldata)
}