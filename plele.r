plele = function(param,data,vardata3) {



# figure;plot(vardata3');
data3=data[,vardata3<param]; #agafo buckets amb menys variança (7099 buckets)

j=1
quotient=matrix(NA,dim(data3)[1],floor(dim(data3)[2]/20)); #divideixo en buckets de 0.03 ppms
for (i in seq(1,(dim(quotient)[2]*20)-19,20)){
  reference= median(rowMeans(data3[,i:(i+19)],na.rm = T),na.rm=T)#calculo espectre referencia en aquest bucket
  quotient[,j] = rowMeans(data3[,i:(i+19)],na.rm=T)/reference #calculo quocient en aquest bucket
  j=j+1;
}
  


quotientmedian=apply(quotient,1,function(x) median(x,na.rm=T))
s=list()

s$pqndatanoscale = data/quotientmedian; #normalitzo dades per quocient medià;

s$lol2=apply(s$pqndatanoscale,2,function(x) sd(x,na.rm=T)/mean(x,na.rm=T))


return(s)
}