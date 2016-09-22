definitivefitting = function(signals_parameters, Xdata) {
  #Created by Daniel Cañueto 30/08/2016
  #Fitting of every signal according to the parameters calculated previously
  #rownames of signals_parameters = intensity, shift, width, gaussian, jcoupling, multiplicities, roof effect
  
  multiplicities = signals_parameters[6, ]
  roof_effect = signals_parameters[7, ]
  fitted_signals = matrix(NaN, dim(signals_parameters)[2], length(Xdata))
  
  for (s in seq_along(multiplicities)) {
    if (roof_effect[s] > 0) {
      if (multiplicities[s] == 1)   {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s]/(1 - roof_effect[s]),
          (signals_parameters[2, s] - signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(
          c(
            signals_parameters[1, s],
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 3) {
        y=1/(2 + roof_effect[s])
        x= 1-y
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] *x,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] *y,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 4) {
    #     fitted_signals[s, ] = peakpvoigt(
    #       c(
    #         signals_parameters[1, s] / 3,
    #         (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
    #         signals_parameters[3, s],
    #         signals_parameters[4, s]
    #       ),
    #       Xdata
    #     ) + peakpvoigt(c(
    #       signals_parameters[1, s],
    #       (signals_parameters[2, s] - signals_parameters[5, s]),
    #       signals_parameters[3, s],
    #       signals_parameters[4, s]
    #     ),
    #     Xdata) + peakpvoigt(c(
    #       signals_parameters[1, s],
    #       (signals_parameters[2, s] + signals_parameters[5, s]),
    #       signals_parameters[3, s],
    #       signals_parameters[4, s]
    #     ),
    #     Xdata) + peakpvoigt(
    #       c(
    #         signals_parameters[1, s] / 3,
    #         (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
    #         signals_parameters[3, s],
    #         signals_parameters[4, s]
    #       ),
    #       Xdata
    #     )
      }
    } else if (roof_effect[s] == 0) {
      if (multiplicities[s] == 0) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 1) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] - signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata) + peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] + signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata)
      } else if (multiplicities[s] == 3) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 4) {
        # fitted_signals[s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # ) + peakpvoigt(c(
        #   signals_parameters[1, s] ,
        #   (signals_parameters[2, s] - signals_parameters[5, s]) ,
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] + signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] + 3 * signals_parameters[5, s]) ,
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # )
      }
    } else if (roof_effect[s] < 0) {
      if (multiplicities[s] == 1) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(c(
          signals_parameters[1, s] *
            (1 - roof_effect[s]) ,
          (signals_parameters[2, s] + signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
        Xdata)
      } else if (multiplicities[s] == 3) {
        y=1/(1 + roof_effect[s])
        x= 1-y
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s]*x,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s]*y,
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] / 2 * (1 - roof_effect[s]),
            (signals_parameters[2, s] + signals_parameters[5, s]) ,
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          Xdata
        )
      } else if (multiplicities[s] == 4) {
        # fitted_signals[s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3 ,
        #     (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # ) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] - signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] + signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # Xdata) + peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   Xdata
        # )
      }
    }
  }
  
  
  return(fitted_signals)
}
