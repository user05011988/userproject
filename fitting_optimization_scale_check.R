#Aquesta ?s la funci? adaptada i R.
fitting_optimization_scale_check = function(parS, xx,observed,multiplicities,roof_effect,ple) {
  # print(ple)
  # if (exists('ple')) {
  # } else {
  #   parT=parS
    # }
  i = as.numeric(parT[seq(1, length(parS) - 2, 3)])
  p = as.numeric(parT[seq(2, length(parS) - 1, 3)])
  w = as.numeric(parT[seq(3, length(parS) - 0, 3)])
  # i = as.numeric(parT[seq(1, length(parS) - 2, 3)])*ple[1,1]+ple[1,2]
  # p = as.numeric(parT[seq(2, length(parS) - 1, 3)])*ple[2,1]+ple[2,2]
  # w = as.numeric(parT[seq(3, length(parS) - 0, 3)])*ple[3,1]+ple[3,2]
  g = rep(0,length(i))
  j = rep(0,length(i))
  # i = as.numeric(parT[seq(1, length(parS) - 4, 5)])*(ple[1,1]+ple[1,2])
  # p = as.numeric(parT[seq(2, length(parS) - 3, 5)])*ple[2,1]+ple[2,2]
  # w = as.numeric(parT[seq(3, length(parS) - 2, 5)])*ple[3,1]+ple[3,2]
  # g = as.numeric(parT[seq(4, length(parS) - 1, 5)])*ple[4,1]+ple[4,2]
  # j = as.numeric(parT[seq(5, length(parS) - 0, 5)])*ple[5,1]+ple[5,2]
  signals_parameters=rbind(i,p,w,g,j)
  fitted_signals = matrix(NaN, dim(signals_parameters)[2], length(xx))
  
  # multiplicities = as.numeric(parS[seq(6, length(parS) - 1, 7)])
  # roof_effect = as.numeric(parS[seq(7, length(parS) - 0, 7)])
  NumSignals = length(parS) / 5
  
  
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
          xx
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s]/(1 - roof_effect[s]),
          (signals_parameters[2, s] - signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
          xx) + peakpvoigt(
            c(
              signals_parameters[1, s],
              (signals_parameters[2, s] + signals_parameters[5, s]),
              signals_parameters[3, s],
              signals_parameters[4, s]
            ),
            xx
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
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] *y,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        )
      } else if (multiplicities[s] == 4) {
        #     fitted_signals[s, ] = peakpvoigt(
        #       c(
        #         signals_parameters[1, s] / 3,
        #         (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #         signals_parameters[3, s],
        #         signals_parameters[4, s]
        #       ),
        #       xx
        #     ) + peakpvoigt(c(
        #       signals_parameters[1, s],
        #       (signals_parameters[2, s] - signals_parameters[5, s]),
        #       signals_parameters[3, s],
        #       signals_parameters[4, s]
        #     ),
        #     xx) + peakpvoigt(c(
        #       signals_parameters[1, s],
        #       (signals_parameters[2, s] + signals_parameters[5, s]),
        #       signals_parameters[3, s],
        #       signals_parameters[4, s]
        #     ),
        #     xx) + peakpvoigt(
        #       c(
        #         signals_parameters[1, s] / 3,
        #         (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
        #         signals_parameters[3, s],
        #         signals_parameters[4, s]
        #       ),
        #       xx
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
          xx
        )
      } else if (multiplicities[s] == 1) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(c(
          signals_parameters[1, s],
          (signals_parameters[2, s] - signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
          xx) + peakpvoigt(c(
            signals_parameters[1, s],
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
            xx)
      } else if (multiplicities[s] == 3) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] / 2,
            (signals_parameters[2, s] + signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        )
      } else if (multiplicities[s] == 4) {
        # fitted_signals[s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   xx
        # ) + peakpvoigt(c(
        #   signals_parameters[1, s] ,
        #   (signals_parameters[2, s] - signals_parameters[5, s]) ,
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # xx) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] + signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # xx) + peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] + 3 * signals_parameters[5, s]) ,
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   xx
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
          xx
        )
      } else if (multiplicities[s] == 2) {
        fitted_signals[s, ] = peakpvoigt(
          c(
            signals_parameters[1, s],
            (signals_parameters[2, s] - signals_parameters[5, s]),
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        ) + peakpvoigt(c(
          signals_parameters[1, s] *
            (1 - roof_effect[s]) ,
          (signals_parameters[2, s] + signals_parameters[5, s]),
          signals_parameters[3, s],
          signals_parameters[4, s]
        ),
          xx)
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
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s],
            signals_parameters[2, s],
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        ) + peakpvoigt(
          c(
            signals_parameters[1, s] / 2 * (1 - roof_effect[s]),
            (signals_parameters[2, s] + signals_parameters[5, s]) ,
            signals_parameters[3, s],
            signals_parameters[4, s]
          ),
          xx
        )
      } else if (multiplicities[s] == 4) {
        # fitted_signals[s, ] = peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3 ,
        #     (signals_parameters[2, s] - 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   xx
        # ) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] - signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # xx) + peakpvoigt(c(
        #   signals_parameters[1, s],
        #   (signals_parameters[2, s] + signals_parameters[5, s]),
        #   signals_parameters[3, s],
        #   signals_parameters[4, s]
        # ),
        # xx) + peakpvoigt(
        #   c(
        #     signals_parameters[1, s] / 3,
        #     (signals_parameters[2, s] + 3 * signals_parameters[5, s]),
        #     signals_parameters[3, s],
        #     signals_parameters[4, s]
        #   ),
        #   xx
        # )
      }
    }
  }
  F=observed-colSums(fitted_signals)
  
  return(F)
  
  
}