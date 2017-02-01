matplot(t(imported_data$dataset[,8860:8900]),type='l')
CV(rowSums(imported_data$dataset[which(cor(t(imported_data$dataset[,8860:8900]),apply(imported_data$dataset[,8860:8900],2,median))>0.95),8860:8900]))
MADM <- function(x){
(mad(x,na.rm=T)/median(x,na.rm=T))
}
antes=apply(imported_data$dataset,2,MADM)
despues=apply(imported_data$dataset/rep.col(rowSums(fan[,1015:1040]),13001),2,MADM)
plot(sort(antes),type='l',ylim=c(0,0.6))
lines(sort(despues),col='red')
despues2=apply(imported_data$dataset/rep.col(rowSums(imported_data$dataset[,8860:8900]),13001),2,MADM)
lines(sort(despues2),col='blue')
plot(sort(antes/antes),type='l')
lines(sort(despues/antes),col='red')
lines(sort(despues2/antes),col='blue')


###########

library(readr)
initial_dataset <- as.matrix(read_csv("C:/Bruker/TopSpin3.2/data/RuiSimoes/data_analysis/results_0004/initial_dataset.csv"))
ppm=seq(12,-1,length.out=16251)
matplot(t(initial_dataset[,which(ppm<2.48&ppm>2.42)]),type='l')
glut=rowSums(initial_dataset[,which(ppm<2.48&ppm>2.42)])/length(which(ppm<2.48&ppm>2.42))

har=apply(rep.col(glut,16251)+initial_dataset,2,MADM)
har2=apply(initial_dataset,2,MADM)
har2[har2<0]=100000

ha3=har-har2
ha3[which(ha3<(-0.1))]=10

ha4=ppm[sort(ha3,index.return=T)$ix[1:100]]

har=apply(scale(rep.col(glut,16251))+scale(initial_dataset),2,MADM)
har2=apply(scale(initial_dataset),2,MADM)

plot(har/har2)

plot(ppm[which(ppm<4.25&ppm>4.1)],(har*apply(initial_dataset,2,median)/har2)[which(ppm<4.25&ppm>4.1)])



heh=which(apply(initial_dataset,2,median)>median(apply(initial_dataset,2,median)))


ab=peakdet(apply(initial_dataset,2,median),0.1)

ac=scale(initial_dataset[,ab$maxtab$pos])

ad=apply(ac,2,sd)
ae=matrix(NA,ncol(ac),ncol(ac))
for (i in 1:ncol(ac)) {
  for (j in 1:ncol(ac)) {
  ae[i,j]=cor(ac[,i],ac[,j])
    
    }
  
}
al=cor(initial_dataset[,12456],initial_dataset[,13265],method='spearman')
jitter(initial_dataset[,12456],1,0.1*median(initial_dataset[,12456]))
all=cor(jitter(initial_dataset[,12456],1,0.1*median(initial_dataset[,12456])),jitter(initial_dataset[,13265],1,0.1*median(initial_dataset[,13265])),method='spearman')

al=cor(initial_dataset,initial_dataset[,ab$maxtab$pos[139]],method='spearman')
nn=which(al>0.85)
sapply(nn,function(x)coef(summary(rlm(initial_dataset[,x],initial_dataset[,ab$maxtab$pos[139]])))[1])



for(i in 1:102) {
     for (j in 1:102) {
       rer2[i,j]=rer[i,j]*rer[j,i]
     }
  }



def=rep(NA,10)
def2=seq(0.8,0.98,length.out=10)
for (m in 1:10) {
  nn=which(cor(initial_dataset,initial_dataset[,ab$maxtab$pos[155]],method='spearman')>def2[m])
  rer=matrix(NA,length(nn),length(nn))
  for (i in 1:length(nn)) rer[i,]= sapply(nn,function(x)coef(summary(rlm(initial_dataset[,x],initial_dataset[,nn[i]])))[1])
  rer2=matrix(NA,length(nn),length(nn))
  for(i in 1:length(nn)) {
    for (j in 1:length(nn)) {
      if (i<j) rer2[i,j]=rer[i,j]*rer[j,i]
    }
  }
  def[m]=sd(rer2,na.rm=T)
}


aaa=cbind(initial_dataset[,which.min(abs(ppm-2.2624))],initial_dataset[,ab$maxtab$pos[168]])
initio=cor(initial_dataset[,which.min(abs(ppm-2.2624))],initial_dataset[,ab$maxtab$pos[168]],method = 'spearman')
i=0
while(i <10000) {
  aab=jitter(aaa,1,0.01)
  blon=cor(aab[,1],aab[,2],method = 'spearman')
  if (blon>initio) aaa=aab
  i=i+1
}
cor(aaa[,1],aaa[,2],method = 'spearman')
  


aaa=cbind(initial_dataset[,which.min(abs(ppm-2.2928))],initial_dataset[,ab$maxtab$pos[168]])
cor(aaa[,1],aaa[,2],method = 'spearman')

initio=cor(initial_dataset[,which.min(abs(ppm-2.2928))],initial_dataset[,ab$maxtab$pos[168]],method = 'spearman')
i=0
while(i <10000) {
  aab=jitter(aaa,1,0.01)
  blon=cor(aab[,1],aab[,2],method = 'spearman')
  if (blon>initio) aaa=aab
  i=i+1
}
cor(aaa[,1],aaa[,2],method = 'spearman')


