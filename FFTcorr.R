FFTcorr <-
function(testSegment, target,shift)
{
#padding
M<-length(testSegment)
diff <- 2^(ceiling(log2(M))) - M
testSegment<-testSegment-min(testSegment)
target<-target-min(target)

# append our ref & test segments with zeros at the end.
target<-c(target,rep(0,diff))
testSegment<-c(testSegment,rep(0,diff))
M<- M+diff
X<-fft(target)
Y<-fft(testSegment)
R<-X*Conj(Y)
R<-R/(M)
rev<-fft(R, inverse = TRUE)/length(R)
vals<-Re(rev)
maxpos <- 1
maxi <- -1
if (M<shift)
 {
    shift <- M
 }

for (i in 1:shift)
{
    if (vals[i]>maxi)
    {
        maxi <- vals[i]
        maxpos <- i
    }
    if (vals[length(vals)-i+1] > maxi)
     {
        maxi <- vals[length(vals)-i+1]
        maxpos <- length(vals)-i+1
    }
}

if (maxpos > (length(vals)/2))
 {
    lag <- maxpos-length(vals)-1
}else{
    lag <-maxpos-1
}

return(lag)
}
