library(R.matlab)
a=readMat("C:/Bruker/TopSpin3.2/data/MDMA2/nmr/dummy.mat")

b=read.csv("C:/Bruker/TopSpin3.2/data/MDMA2/nmr/dummy.csv",header=F)
ppmX=as.vector(read.csv("C:/Bruker/TopSpin3.2/data/MDMA2/nmr/ppmX.csv",header=F))

c=array(NA,dim=c(16,8192,30))

for (i in 1:30) c[,,i]=as.matrix(b[(16*i-15):(16*i),])
  
ppm=seq(12,-1,-0.001)
d=c/quantile(c,0.95,na.rm=T)
d[d<0]=0
d[is.na(d)]=0
e=d
for (i in 1:16) e[i,,]=t(alignment(t(d[i,,]),ppmX))

res=makeSimulatedData()
Y=pqndata
Y[,13001]=0
groupLabel=factor(c(rep(1,48),rep(2,84)))

N = 100
alpha = 0.05
 BW = BWR(Y, groupLabel)
# create sampled H0 and export to file
 H0 = createNullSampling(Y, groupLabel, N = N,verbose=FALSE)
#compute percentile of alpha
  perc = double(ncol(Y));
 alpha_corr = alpha/sum(returnLocalMaxima(Y[2,])$pkMax>50000);
 for (i in 1 : length(perc)){
   perc[i] = quantile(H0[,i],1-alpha_corr, type = 3);
 }
  
  a=readMat("C:/Users/user/Documents/icoshift_v1_3_2/icoshift_v1_3_2/WineData.mat")
  res=makeSimulatedData()
  Y=res$data
  groupLabel=res$label
  
  N = 100
  alpha = 0.05
  BW = BWR(Y, groupLabel)
  # create sampled H0 and export to file
  H0 = createNullSampling(Y, groupLabel, N = N,verbose=FALSE)
  #compute percentile of alpha
  perc = double(ncol(Y));
  alpha_corr = alpha/sum(returnLocalMaxima(Y[2,])$pkMax>50000);
  for (i in 1 : length(perc)){
    perc[i] = quantile(H0[,i],1-alpha_corr, type = 3);
  }