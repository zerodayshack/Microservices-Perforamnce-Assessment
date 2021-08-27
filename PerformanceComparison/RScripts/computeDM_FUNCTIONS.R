#Functions needed before plotting and tables

#Identify selected configurations from dataFile
#mySettings <-unique(dataFile[,1:5])
#print(mySettings)

##END SETTINGS

#FUNCTIONS: aggregatedValues; computeRelativeMass; computeDomainMetrics

#Create aggregated values (by myOrderedSplitting) of the user frequency from "operationalProfile" 
aggregateValues<-function(myOrderedSplitting, accessCount, scaledUsersLoad){
accessFrequency<-accessCount/sum(accessCount)
if(length(myOrderedSplitting)==1){
n<-myOrderedSplitting
#this case happens only when a splitting parameter "n" is given. the experiemnts are run independently 
byN<-which(scaledUsersLoad %% n == 0)
print(byN)
binProb<-c()
for(i in 1:length(byN)){
	if(i==1){
		binProb[i]<-sum(accessFrequency[1:byN[i]])
	}else{
		binProb[i]<-sum(accessFrequency[(byN[i-1]+1): byN[i]])
	}
}
aggregatedValues<-matrix(c(scaledUsersLoad[byN], binProb), ncol=2,nrow=length(binProb), dimnames=list(c(1:length(binProb)), c("Workload (number of users)", "Domain metric per workload")))
print(scaledUsersLoad[byN])
}else{
	print(myOrderedSplitting)
	binProb<-c()
	
	for(i in 1:length(myOrderedSplitting)){
		if(i==1){
		binProb[i]<-sum(accessFrequency[1: which.min(abs(scaledUsersLoad - myOrderedSplitting[i]))])
	}else{
		binProb[i]<-sum(accessFrequency[(which.min(abs(scaledUsersLoad - myOrderedSplitting[i-1]))+1): which.min(abs(scaledUsersLoad - myOrderedSplitting[i]))])
	}
	}
	aggregatedValues<-matrix(c(myOrderedSplitting, binProb), ncol=2,nrow=length(binProb), dimnames=list(c(1:length(binProb)), c("Workload (number of users)", "Domain-based metric per workload")))
}
return(aggregatedValues)
}

#Compute the relative mass for each load and each configuration
computeRelativeMass<-function(threshold, avg, mixTemp,noMicroServices){
#Check pass/fail for each service. the "mix" value is 0 if fail and mixTemp if pass. Compute the relative mass for each configuration
passCriteria<-avg
relativeMass<-c()
mix<-as.data.frame(matrix(nrow=nrow(passCriteria), ncol=ncol(mixTemp)))
failingMS<-NULL
dec<-as.data.frame(matrix(nrow=nrow(passCriteria), ncol=ncol(mixTemp)))
for(j in 1:nrow(passCriteria)){
	mix[j,]<-mixTemp[j,]
	dec[j,]<-mixTemp[j,]
	for(i in 3:(2+noMicroServices)){
		if(passCriteria[j,i]>threshold[i]){
		  mix[j,i]<-0
		  #print(paste(passCriteria$ID[i], passCriteria$Users[i], colnames(avg)[i], sep=" "))
		  failingMS<-rbind(failingMS,cbind(as.character(passCriteria$ID[j]), passCriteria$Users[j], mixTemp[j,i],avg[j,i],colnames(avg)[i]))
		  }else{dec[j,i]<-0}
	}
	relativeMass[j]<-sum(mix[j,3:(2+noMicroServices)])
}
relativeMass<-cbind(mixTemp[,1:2],relativeMass)
#dec1<-cbind(mixTemp[,1],dec[,-1])
#colnames(dec1)<-as.vector(colnames(avg))
return(list(relativeMass,failingMS))
}

#Compute the domain metric for each configuration as a list
computeDomainMetrics<-function(relativeMass, aggregatedValues){
  tempData<-relativeMass
  absoluteMass<-c()
  for(j in 1:nrow(tempData)){	
    absoluteMass[j]<-round(tempData[j,"relativeMass"]*aggregatedValues[match(tempData[j,"Users"], aggregatedValues[,1]),2],3)
  }
  tempData$absoluteMass<-absoluteMass
  type<-unique(tempData$ID)
  # a list with ID, Users, relativeMass, absoluteMass 
  domainMetricList<-list()
  for(index in type){
    domainMetricList[[index]]<-tempData[tempData$ID==index,]
  }
  return(domainMetricList)
}

#THRESHOLD )(deprecated) Use instead computeThreshold.R
#as avg+3*SD for the configuration with mono config   
#computeThreshold<-function(operations,dataBaseline,par){
#   noMicroServices<-length(operations)
#   tempBench<-dataBaseline[dataBaseline$Users==par,]
#   avgVectorB<-tempBench[tempBench$Metric=="Avg (sec)",][,-c(1:3)]
#   SDVectorB<- tempBench[tempBench$Metric=="SD (sec)",][,-c(1:3)]
#   mixB<-tempBench[tempBench$Metric=="Mix % (take failure into account)",][,-c(1:3)]
#   threshold<-data.frame(unique(tempBench$ID), par,avgVectorB+3*SDVectorB)
# }
