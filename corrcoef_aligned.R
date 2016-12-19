corrcoef_aligned <-
function(sp,ref,step)
{
# Calculation of correlation coefficient:
# Filter scales the spectral regions through specified step to unit variance 
# so as the high intensive and low intensive peaks would contribute 
# equally in the similarity


ilength<-length(sp)
if (ilength<20) {CC<-0}

if (step>=ilength)
 {
    CC<-cor(sp,ref)
    CC<-CC[1]
    return(CC)
}

bin_count<-ceiling(ilength/step)
bin_width<-ceiling(ilength/bin_count)
bins<-seq(1,ilength, bin_width)

if (bins[length(bins)]!=ilength)
{
    bins<-c(bins,ilength)
    bin_count<-bin_count+1
}

for (i in 1:(bin_count-1))

{
    istart<-bins[i]
    iend<-bins[i+1]-1
    sp[istart:iend]<-sp[istart:iend]-mean(sp[istart:iend])
    ref[istart:iend]<-ref[istart:iend]-mean(ref[istart:iend])

 if (istart!= iend)
  {
    if (var(sp[istart:iend])!=0)
    {
        sp[istart:iend]<-sp[istart:iend]/sd(sp[istart:iend])
    }
    if (var(ref[istart:iend])!=0)
     {
        ref[istart:iend]<-ref[istart:iend]/sd(ref[istart:iend])
    }
   }
}

CC<-cor(sp[1:(length(sp)-1)],ref[1:(length(ref)-1)])
CC<-CC[1]

return(CC)

}
