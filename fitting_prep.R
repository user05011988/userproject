fitting_prep = function(Xdata,
                        Ydata,
                        initial_fit_parameters,
                        other_fit_parameters) {
  #Created by Daniel Ca?ueto 30/08/2016
  #Preparation of parameters to optimize to achieve the best fitting

  #TO DO: more intuitive way to change how the parameters are prepared previously to optimization

  signals_to_fit = length(initial_fit_parameters$positions)
  ROIlength = length(Xdata)

  #Change of j-coupling from Hz to ppm
  # for (i in 1:signals_to_fit)
  #   initial_fit_parameters$Jcoupling[i] = ifelse((
  #     initial_fit_parameters$multiplicities[i] == 1 |
  #       initial_fit_parameters$multiplicities[i] == 3
  #   ),
  #   initial_fit_parameters$Jcoupling[i] / other_fit_parameters$freq,
  #   (
  #     initial_fit_parameters$Jcoupling[i] / other_fit_parameters$freq
  #   ) / 2
  #   )

  #Calculation of number of background signals, if baseline fitting is performed
  BGSigNum = ifelse(other_fit_parameters$clean_fit == 'N', max(round(abs(Xdata[1] -
                                                                        Xdata[ROIlength]) * other_fit_parameters$BGdensity), 2), 0)

  #Preallocation of parameters to optimize into a matrix of features
  FeaturesMatrix = matrix(NA, (signals_to_fit + BGSigNum), 12)
  colnames(FeaturesMatrix) = c(
    'minimum_intensity',
    'maximum_intensity',
    'shift_left_limit',
    'shift_right_limit',
    'minimum_width',
    'maximum_width',
    'minimum_gaussian',
    'maximum_gaussian',
    'minimum_J_coupling',
    'maximum_J_coupling',
    'multiplicities',
    'roof_effect'
  )

  #Parameters of signals to fit
  FeaturesMatrix[1:signals_to_fit, 1] = 0
  FeaturesMatrix[1:signals_to_fit, 2] = max(Ydata)
  FeaturesMatrix[1:signals_to_fit, 3] = initial_fit_parameters$positions -
    initial_fit_parameters$shift_tolerance
  FeaturesMatrix[1:signals_to_fit, 4] = initial_fit_parameters$positions +
    initial_fit_parameters$shift_tolerance
  FeaturesMatrix[1:signals_to_fit, 5] = initial_fit_parameters$widths- initial_fit_parameters$widths * other_fit_parameters$widthtolerance
  FeaturesMatrix[1:signals_to_fit, 6] = initial_fit_parameters$widths + initial_fit_parameters$widths * other_fit_parameters$widthtolerance
  FeaturesMatrix[1:signals_to_fit, 7] = 0
  FeaturesMatrix[1:signals_to_fit, 8] = other_fit_parameters$gaussian
  FeaturesMatrix[1:signals_to_fit, 9] = initial_fit_parameters$Jcoupling -
    other_fit_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 10] = initial_fit_parameters$Jcoupling +
    other_fit_parameters$j_coupling_variation
  FeaturesMatrix[1:signals_to_fit, 11] = initial_fit_parameters$multiplicities
  FeaturesMatrix[1:signals_to_fit, 12] = initial_fit_parameters$roof_effect


  FeaturesMatrix[initial_fit_parameters$multiplicities==1, 9:10] = 0
  
  #Finding of maximum intensity and shift tolerance of every background signal
  if (BGSigNum>0) {
    BGSigrightlimits = seq(Xdata[1], Xdata[ROIlength], length = BGSigNum) -
      0.005
    BGSigleftlimits = BGSigrightlimits + 0.01

    peaks = peakdet(Ydata, 0.01)
    left = which(peaks$mintab$pos < ROIlength / 5)
    right = which(peaks$mintab$pos > 4 * ROIlength / 5)
    dummy = round(seq(1, ROIlength, length = 2 * BGSigNum - 1))
    BGleftlimits = dummy[c(1, seq(2, length(dummy) - 1, 2))]
    BGrightlimits = dummy[c(seq(2, length(dummy) - 1, 2), length(dummy))]
    BGSig_maximums = replicate(BGSigNum, NA)
    for (ss in 1:BGSigNum)
      BGSig_maximums[ss] = min(Ydata[BGleftlimits[ss]:BGrightlimits[ss]])


    #Parameters of background signals
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 1] = 0
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 2] = BGSig_maximums
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 3] = BGSigrightlimits
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 4] = BGSigleftlimits
    # FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 5] = (1.5 /
    #                                                                     other_fit_parameters$freq) * 10
    # FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 6] = (1.5 /
    #                                                                      other_fit_parameters$freq) * 15
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 5] = other_fit_parameters$BG_width-other_fit_parameters$BG_width*other_fit_parameters$BG_width_tolerance
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 6] = other_fit_parameters$BG_width+other_fit_parameters$BG_width*other_fit_parameters$BG_width_tolerance

    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 7] = 0
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 8] = other_fit_parameters$BG_gaussian_percentage
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 9] = 0
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 10] = 0 #j coupling makes no sense with backgorund signals
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 11] = 0 #arbitrary number used to signal later background signals
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1], 12] = 0


    ss=approx(round((BGSigleftlimits+BGSigrightlimits)/2,3),BGSig_maximums,xout=Xdata)$y/ (max(Ydata))

    # optimization of baseline parameters , to be sure that the algorithm doesn ot try ti fot spurious signals as basleine
    BG_parameters = fittingloop_bg(FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1],],
                                Xdata,
                                ss,
                                other_fit_parameters)
    
    FeaturesMatrix[(signals_to_fit + 1):dim(FeaturesMatrix)[1],2]=BG_parameters[1,]

  }


  return(FeaturesMatrix)
}
