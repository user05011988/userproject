#La funció
peakpvoigt  = function(x,p) {
#PEAKPVOIGT1 Pseudo-Voigt 1 (Gaussian with Lorentzian)
#
#  INPUTS
  #     x = 4 element vector with parameters
#      x[1] = coefficient,
#      x[2] = peak center,
#      x[3] = spread (must be >0), and
#      x[1] = fraction guassian (0<x[1]<1).
#    ax = axis 1xN (independent variable).
#
#  OUTPUTS
  #     z = p-x[2]
#     y = x[1]*( x[1]*exp(-4*ln(2)*z.^2/x[3]^2) + 
                   #               (1-x[1])*x[3]^2./(x[3]^2+z.^2) ); #1xN.
                 #    y1 = dy/dxi,    4xN Jacobian.
                 #    y2 = d2y/dxi^2, 4x4xN Hessian.
                 #
                 #I/O [y,y1,y2] = peakpvoigt1(x,p);
                 #
                 #See also PEAKFUNCTION, PEAKGAUSSIAN, PEAKLORENTZIAN, PEAKPVOIGT2
                 
                 # Copyright © Eigenvector Research, Inc. 2004-2009
                 # Licensee shall not re-compile, translate or convert "M-files" contained
                 #  in PLS_Toolbox for use with any software other than MATLAB®, without
                 #  written permission from Eigenvector Research, Inc.
                 #nbg 12/1/05
                 
                 #No error trapping is used to maximize speed
                 
   z   = p - x[2];
   zz  = z*z;
   x32 = x[3]*x[3];
   a   = 4*log(2);
   d   = zz+x32;
   f1  = exp(-a*zz/x32);  #f1
   f2  = x32/d;          #f2
   
   aa  = x[4]*f1;
   bb  = (1-x[4])*f2;
   y   = x[1]*(aa + bb);  #This is the peak function

  return(y)
}