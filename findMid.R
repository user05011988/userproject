findMid <-
function(testSeg,refSeg)
{
specLn<-length(testSeg)
M<-ceiling(length(testSeg)/2)
specM<-testSeg[(M-floor(M/4)):(M+floor(M/4))]
refM<-refSeg[(M-floor(M/4)):(M+floor(M/4))]

I<-which.min(specM*refM)
#[C,I]<-min(specM)
mid <- I[1]+M-floor(M/4)


#move to a point of a local minima
index<-1

while (TRUE)
{
    if (((mid-1) <= 1)||((mid+1) >= specLn))
     {
        mid<-NA
        break
    }

    if ((testSeg[mid] <= testSeg[mid+1])&&(testSeg[mid] <= testSeg[mid-1]))
       { 
        break}
   
    if (testSeg[mid] >= testSeg[mid+1])
     {
        mid<-mid+1
    }else{ if(testSeg[mid]>=testSeg[mid-1])
        {
         mid<-mid-1}
    }
}

return(mid)

}
