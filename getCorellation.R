getCorellation <-
function(dpReferencePeak, dpInputPeak, maxShift)
{
if (nargs()<3)
 {
    stop('Invalid input parameters count')
 }
if (length(dpReferencePeak)!=length(dpInputPeak))
 {
    stop('dpReferencePeak and dpInputPeak sizes must agree')
 }

#evaluate lag by Wong
lag <- FFTcorr( dpInputPeak,dpReferencePeak, maxShift)
aligned <- shift(dpInputPeak,lag)

#get corellation coefficients
corellation <- cor(dpReferencePeak, aligned)

return(corellation)
}
