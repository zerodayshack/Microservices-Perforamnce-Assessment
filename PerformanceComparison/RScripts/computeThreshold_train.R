#Requires dataFile 

#Identify selected configurations from dataFile
#mySettings <-unique(dataFile[,1:5])
#print(mySettings)

##END SETTINGS

#THRESHOLD
#Define the threshold for each service. The threshold is a vector computed as avg+3*SD for the configuration with mono dataset

#par the load to select
par<-5
noMicroServices<-length(operations)
tempBench<-dataBaseline[dataBaseline$Users==par,]
avgVectorB<-tempBench[tempBench$Metric=="Avg (sec)",][,-c(1:3)]
SDVectorB<- tempBench[tempBench$Metric=="TH (sec)",][,-c(1:3)]
mixB<-tempBench[tempBench$Metric=="Mix % (take failure into account)",][,-c(1:3)]
threshold<-data.frame(unique(tempBench$ID), par, SDVectorB)


#SELECT DATA FROM FILE
#Exclude case with user = 2 from dataFile and  extract the relevant variables form the dataset for threshold check. 
#This check is performed in computeRelativeMass of computeDM_FUNCTIONS.R
#Select relevant rows
#usedSettings<-mySettings[mySettings$Users!=par,]
#usedDataFile<-dataFile[dataFile$Users!=par,]
#average number of access

#UsedDatafile is defined in start.R
avg<-usedDataFile[usedDataFile$Metric=="Avg (sec)",-3]
#standard deviation of access
SD<-usedDataFile[usedDataFile$Metric=="TH (sec)",-3]
#This is the frequency of access to a microservice
mixTemp<-usedDataFile[usedDataFile$Metric=="Mix % (take failure into account)",-3]

