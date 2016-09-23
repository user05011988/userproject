import_data = function(parameters_path) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Import of variables stored in the parameters file and of the dataset to quantify

  #List of parameters to use to create the dataset
  params = list()

  #Import fo parameters from the csv file
  # TO DO: stringsasfactors=F
  import_profile = read.delim(
    parameters_path,
    sep = ';',
    header = T,
    stringsAsFactors = F
  )
  import_profile = as.data.frame(sapply(import_profile, function(x)
    gsub("\\\\", "/", x)))

  #Getting the names of experiments, signals and ROIs to quantify and use
  metadata_path = as.character(import_profile[3, 2])

  Experiments = read.delim(
    metadata_path,
    sep = ';',
    header = T,
    stringsAsFactors = F
  )[, 1]
  Experiments = as.vector(Experiments[Experiments != ''])

  # signals_names = read.delim(as.character(import_profile[6, 2]),
  #                            header = F,
  #                            stringsAsFactors = F)[, 1]
  # signals_names = as.list(signals_names[signals_names != ''])
  profile_folder_path = as.character(import_profile[7, 2])

  ROI_data=read.csv(profile_folder_path,sep=";")
  signals_names=ROI_data[,4]
  signals_codes = 1:length(signals_names)



  #Preparing the structure of experiments and signals where to store the output
  export_path = as.character(import_profile[8, 2])

  #Criteria for saving or not plots of fit
  E_max = as.numeric(as.character(import_profile[9, 2]))
  P_max = as.numeric(as.character(import_profile[10, 2]))

  #Other necessary variables
  freq = as.numeric(as.character(import_profile[14, 2]))

  #Kind of normalization
  #TO DO: add PQN (but before standardize a way to find the regions to have into account)
  normalization = import_profile[11, 2]
  params$norm_AREA = 'N'
  params$norm_PEAK = 'N'
  params$norm_PEAK_left_ppm = 12
  params$norm_PEAK_right_ppm = -1
  if (normalization == 1) {
    #Eretic
    params$norm_AREA = 'Y'
    params$norm_PEAK_left_ppm = 11.53
    params$norm_PEAK_right_ppm = 10.47
  } else if (normalization == 2) {
    #TSP
    params$norm_AREA = 'Y'
    params$norm_PEAK_left_ppm = 0.1
    params$norm_PEAK_right_ppm = -0.1
  } else if (normalization == 3) {
    #Creatinine (intensity, not area, maybe dangerous for rats because of oxalacetate)
    params$norm_PEAK = 'Y'
    params$norm_PEAK_left_ppm = 3.10
    params$norm_PEAK_right_ppm = 3
  } else if (normalization == 4) {
    #Spectrum AreA
    params$norm_AREA = 'Y'
  } else if (normalization == 5) {
    #No normailzation

  }

  #Alignment
  alignment = import_profile[12, 2]
  params$glucose_alignment = 'N'
  params$tsp_alignment = 'N'
  params$peak_alignment = 'N'
  params$ref_peak_pos = 8.452
  if (alignment == 1) {
    #Glucose
    params$glucose_alignment = 'Y'
  } else if (alignment == 2) {
    #TSP
    params$tsp_alignment = 'Y'
  } else if (alignment == 3) {
    #Formate
    params$peak_alignment = 'Y'
  }

  #Suppresion regions
  suppression = as.character(import_profile[13, 2])
  if (suppression == '') {
    params$disol_suppression = 'N'
  } else {
    params$disol_suppression = 'Y'
    params$disol_suppression_ppm = as.numeric(strsplit(suppression, ';|,')[[1]])
    dim(params$disol_suppression_ppm) = c(length(params$disol_suppression_ppm) /
                                            2, 2)
    params$disol_suppression_ppm = t(params$disol_suppression_ppm)
  }

  #Variables only necessary for reading Bruker files
  bruker_path = import_profile[1, 2]
  expno = as.character(import_profile[4, 2])
  processingno = as.character(import_profile[5, 2])

  #Variables only necessary for reading dataset in csv format
  dataset_path = as.character(import_profile[2, 2])

  if (bruker_path == '' || expno == '' || processingno == '') {
    if (dataset_path != '') {
      #Reading of dataset file (ideally with fread of data.table package, bu seems that the package is not compatible with R 3.3.1)
      imported_data = list()
      dummy = as.matrix(rio::import(dataset_path, sep = ','))
      imported_data$dataset=dummy[-1,]
      imported_data$ppm = colnames(imported_data$dataset) = dummy[1,]
      rownames(imported_data$dataset) = Experiments

      params$buck_step = ifelse(
        as.character(import_profile[21, 2]) == '',
        abs(imported_data$ppm[1] - imported_data$ppm[length(imported_data$ppm)]) /
          length(imported_data$ppm),
        as.numeric(as.character(import_profile[21, 2]))
      )
    } else {
      print('Problem when creating the dataset. Please revise the parameters.')
      return()
    }
  } else {
    #Reading of Bruker files
    params$dir = bruker_path
    params$expno = expno
    params$processingno = processingno
    params$buck_step = as.numeric(as.character(import_profile[21, 2]))
    imported_data = Metadata2Buckets(Experiments, params)

  }

  #Storage of parameters needed to perform the fit in a single variable to return.
  imported_data$buck_step = params$buck_step
  imported_data$profile_folder_path = profile_folder_path
  imported_data$metadata_path = metadata_path
  imported_data$parameters_path = parameters_path
  imported_data$signals_names = signals_names
  imported_data$signals_codes = signals_codes
  imported_data$Experiments = setdiff(Experiments, imported_data$not_loaded_experiments)
  imported_data$E_max = E_max
  imported_data$P_max = P_max
  imported_data$export_path = export_path
  imported_data$freq = freq

  return(imported_data)

}
