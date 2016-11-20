p_values=function(dataset,metadata) {

ss=as.vector(as.matrix(unique(metadata)))
tt=matrix(NA,length(ss),dim(dataset)[2])
for (ind in seq_along(ss)) {
  for (k in 1:dim(dataset)[2]) {
    tt[ind,k]=tryCatch(shapiro.test(dataset[metadata==ss[ind],k])$p.value,error=function(e) NA)
  }
  
}
p_value=rep(NA,dim(dataset)[2])
for (k in 1:dim(dataset)[2]) {
  # if (!any(is.na(dataset[,k]))) {
  if (!any(tt[,k]<0.05,na.rm=T)) {
    # p_value[k]=tryCatch(wilcox.test(dataset[metadata==ss[1],k],dataset[metadata==ss[2],k])$p.value,warning = function(w) { },error=function(e) NA)
    p_value[k]=withCallingHandlers({ wilcox.test(dataset[metadata==ss[1],k],dataset[metadata==ss[2],k])$p.value}, warning = function(w) {})
  } else {
    p_value[k]=tryCatch(t.test(dataset[metadata==ss[1],k],dataset[metadata==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
  }
  
  # }
}
p_value_final=round(t(as.matrix(p.adjust(p_value,method="BH"))),3)
names(p_value_final)=colnames(dataset)
return(p_value_final)
}