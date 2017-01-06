
data2=data
data2[,c(which(Xdata>9),which(Xdata<0.5))]=0
data2[c(1,2,3,126,127,128),]=0
no=peakdet(colSums(data),program_parameters$peakdet_minimum*max(1e-10,max(data)))
Xdata=seq(storedpars$OFFSET[1],storedpars$OFFSET[1]-storedpars$SW[1],length.out=storedpars$SI[1])
Ydata=seq(storedpars$OFFSET[2],storedpars$OFFSET[2]-storedpars$SW[2],length.out=storedpars$SI[2])


so=list()
for (i in 1:length(no$maxtab$pos)) {
  pla=peakdet(data[,no$maxtab$pos[i]],0.5*max(1e-10,max(data[,no$maxtab$pos[i]])))$maxtab
  so[[i]]=pla[which(pla[,1]>3&pla[,1]<125),]
names(so[[i]])=Xdata[no$maxtab$pos[i]]
}
to=list()
i=1
while (i <=length(so)) {
  # while (i <=9) {
    
  j=1
  stop=0
  while (stop==0) {
    # if (nrow(so[[i]])<j) {
    # i=i+1
    # }   
    ind=which(abs(so[[i]][,1]-65)==abs(so[[i]][j,1]-65))[-1]
    
    ind2=which(so[[i]][,1]==65)
    if (length(ind)>0 & length(ind2)>0) {
      ind3=c(1,1,1.5)
      if (CV(c(ind,ind2)/ind3)>0.1) {
        mm=c(j,ind,ind2)
        to[[(length(to)+1)]]=so[[i]][mm,]
        so[[i]]=so[[i]][-mm,]
        # if (nrow(so[[i]])<j) {
        #   i=i+1
        # }
      } else {
        mm=c(j,ind,ind2)
        to[[(length(to)+1)]]=so[[i]][c(j,ind),]
        to[[(length(to)+1)]]=so[[i]][ind2,]
        
        so[[i]]=so[[i]][-mm,]
        # if (nrow(so[[i]])<j) {
        #   i=i+1
        # }
      } 
      } else if (length(ind)>0 & length(ind2)==0){
        mm=c(j,ind)
        to[[(length(to)+1)]]=so[[i]][mm,]
        so[[i]]=so[[i]][-mm,]
        # if (nrow(so[[i]])<j) {
        #   i=i+1
        # }
      } else if (!is.na(so[[i]][j,1]) & length(ind)==0 && abs(so[[i]][j,1]-65)<3){
        mm=j
        to[[(length(to)+1)]]=so[[i]][mm,]
        so[[i]]=so[[i]][-mm,]
        # if (nrow(so[[i]])<j) {
        #   i=i+1
        # }
      } else if (length(ind)==0 & (is.na(so[[i]][j,1])|so[[i]][j,1]!=65) & j<nrow(so[[i]])) {
        j=j+1
      } else if (length(ind)==0 & (is.na(so[[i]][j,1])|so[[i]][j,1]!=65) & j>=nrow(so[[i]])) {
        # if (nrow(so[[1]])==0) {
        # so[[i]] =NULL
        # } else {
        i=i+1
        stop=1
      # }
    }
    }}
    

so2=list()
i=1
while (i <=length(so)) {
  if (nrow(so[[i]])>0)  {
    so2[[(length(so2)+1)]]=so[[i]]
    so[[i]]=NULL
  } else {
    i=i+1
  }
}
i=1
while (i <=length(so2)) {
  if (nrow(so2[[i]])==1&&abs(so2[[i]][1,1]-65)<3)  {
    to[[(length(to)+1)]]=so2[[i]]
    so2[[i]]=NULL
  } else {
    i=i+1
  }
}

plo=matrix(NA,length(to),4)
for (i in 1:length(to)) {
  plo[i,1]=as.numeric(colnames(to[[i]])[1])
  plo[i,2]=to[[i]][1,2]
  plo[i,3]=nrow(to[[i]])
  if (plo[i,3]==1) {
    plo[i,4]=0
  } else if (plo[i,3]==2) {
    plo[i,4]=(Ydata[to[[i]][1,1]]-Ydata[to[[i]][2,1]])*600.2
  } else if (plo[i,3]==3) {
    plo[i,4]=(Ydata[to[[i]][1,1]]-Ydata[to[[i]][2,1]])*600.2/2
}
}

  
  
  plo5=plo
  plo6=matrix(NA,0,4)
  while (nrow(plo5)>0) {
    rr=which.max(plo5[,2])
    ta=which(abs(plo5[,1]-plo5[rr,1])<0.03)
    ta2=which(plo5[ta,2]>0.1*max(plo5[ta,2]))
    
    plo6=rbind(plo6,plo5[ta[ta2],])
    plo5=plo5[-ta,,drop=F]
    
  }
  plo7=matrix(NA,0,4)
  while (nrow(plo6)>0) {
    rr=which.max(plo6[,2])
    ta=which(abs(plo6[,1]-plo6[rr,1])<0.06)
    ta2=which(plo6[ta,2]>0.01*max(plo6[ta,2]))
    
    plo7=rbind(plo7,plo6[ta[ta2],])
    plo6=plo6[-ta,,drop=F]
    
  }
  
  
  suggested_metabolites=matrix(NA,dim(plo7)[1],5)
  colnames(suggested_metabolites)=paste('Suggested Metabolite',1:5)
  for (i in 1:dim(suggested_metabolites)[1]) {
    a=which(abs(repository[,5]-plo7[i,1])<0.05)
    b=which(as.numeric(repository[a,7])==plo7[i,3])
    d=a[sort(abs(repository[a,5]-plo7[i,1]),index.return=T)$ix[1:5]]
    e=c(setdiff(a[b],d),d)
    e=unique(e)[1:5]
    suggested_metabolites[i,]=paste(repository[e,1],' (', round(abs(repository[e,5]-plo7[i,1])[1:5],3),')',sep='')
  }

ll=cbind(plo7,suggested_metabolites)
