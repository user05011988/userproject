getLUB = function(FeaturesMatrix) {
  bounds=list(ub=matrix(0,dim(FeaturesMatrix)[1],dim(FeaturesMatrix)[2]/2),lb=matrix(0,dim(FeaturesMatrix)[1],dim(FeaturesMatrix)[2]/2))
  bounds$lb=t(FeaturesMatrix[,seq(1,9,2)])
  bounds$ub=t(FeaturesMatrix[,seq(2,10,2)])
  return(bounds)
}