path='C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/results_csv_132/ADG19007u_245/Citrate'
metadata_path='C:/Bruker/TopSpin3.2/data/MTBLS1/data analysis/Metadata.csv'

library(rio)
a=import(metadata_path)
b=grep('/',path)
which(a==b)
d=import(paste(path,'Xdata.csv',sep='/'))[,2]
e=import(paste(path,'Ydata.csv',sep='/'))
f=import(paste(path,'FeaturesMatrix.csv',sep='/'))
g=import(paste(path,'other_fit_parameters.csv',sep='/'))

