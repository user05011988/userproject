# if (dim(autorun_data$Metadata)[2]==1) {
autorun_data$Metadata[,1]=factor(c(rep(1,48),rep(2,84)))      





ss=unique(autorun_data$Metadata[,1])
      tt=matrix(NA,length(ss),dim(t_test_data)[2])
      for (ind in seq_along(ss)) {
        for (k in 1.:dim(t_test_data)[2]) {
          tt[ind,k]=tryCatch(shapiro.test(t_test_data[autorun_data$Metadata[,1]==ss[ind],k])$p.value,error=function(e) NA)
        }

      }
      uu=rep(NA,dim(t_test_data)[2])
        for (k in 1.:dim(t_test_data)[2]) {
          if (!any(is.na(t_test_data[,k]))) {
            if (!any(tt[,k]<0.05,na.rm=T)) {
              uu[k]=tryCatch(wilcox.test(t_test_data[autorun_data$Metadata[,1]==ss[1],k],t_test_data[autorun_data$Metadata[,1]==ss[2],k])$p.value,error=function(e) NA)
            } else {
              uu[k]=tryCatch(t.test(t_test_data[autorun_data$Metadata[,1]==ss[1],k],t_test_data[autorun_data$Metadata[,1]==ss[2],k],var.equal=F)$p.value,error=function(e) NA)
            }

            }
        }
        
      
      plotdata = data.frame(Xdata=autorun_data$ppm, uu)
      mediani=apply(autorun_data$dataset,2,function(x) median(x,na.rm=T))
      # plot_ly(data=plotdata,x=~Xdata,y=~Ydata)
      plotdata3 <- cbind(melt(plotdata, id = "Xdata"),mediani)
      plot_ly(data=plotdata3,x=~Xdata,y=~mediani,color=~uu,type='scatter',mode='lines') %>% layout(xaxis=xaxis = list(autorange = "reversed"),yaxis = list(range = c(0, max(autorun_data$dataset))))