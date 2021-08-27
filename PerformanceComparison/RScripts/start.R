#Script to compare performance under different deployment alternatives.
#Version:  August 8th 2021
#Requires computeThreshold.R; computeDM_FUNCTIONS.R

#optional: clean console variables
#rm(list=ls())

#Supress warnings 
defaultW <- getOption("warn")
options(warn = -1)

# #SETTINGS
# +
#To run from command line uncomment the following lines. 
#args = commandArgs(trailingOnly=TRUE)
#workingDirectory<-args[1]
#RScript start.R <path to workingDirectory> 

#LIBRARIES
#Pre-requisites, comment these lines if you already installed the libraries
# install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
# install.packages("gridExtra", repos = "http://cran.us.r-project.org")
# install.packages("gridBase",repos = "http://cran.us.r-project.org")
# install.packages("ggplot2",repos = "http://cran.us.r-project.org")
# install.packages("imager",repos = "http://cran.us.r-project.org")

library(ggplot2)
library(gridExtra)
library(gridExtra)
library(gridBase)
library(grid)
library(scales)
library(imager)
library(RColorBrewer)
library(readxl)
library(stringr)
library(grid)
library(ggplotify)

#########
#Skip this section if you know the file path of the data files
#########
#Set the working directory containing the directory "RScripts"  
setwd("~/Research/Dropbox/JupyterNotebooks/UNISANNIO/")
workingDirectory<-paste(getwd(), "/PerformanceComparison", sep="")
setwd(workingDirectory)

#The directory where the scripts are
scriptDirectory <- paste(getwd(),"/RScripts/", sep = "")

#Set folder for data and results. Substitute it with yours or comment it if you launch from terminal
dataDirectory <- paste(getwd(), "/DATA/Experiments/", sep = "")
setwd(dataDirectory)

#search for the name of the data files
files <- list.files(dataDirectory, pattern = ".xlsx", recursive = TRUE)
iter1 <- files[grep("iter1", files)]
iter2 <- files[grep("iter2", files)]

#Automatically detect from the file names the path for balance/unbalance iter1/iter2 experiemnts
pathData_I1_UB <-
  paste(dataDirectory, iter1[grep("_unbal", iter1)], sep = "")
pathData_I1_B <-
  paste(dataDirectory, iter1[grep("_bal", iter1)], sep = "")
pathData_I2_UB <-
  paste(dataDirectory, iter2[grep("_unbal", iter2)], sep = "")
pathData_I2_B <-
  paste(dataDirectory, iter2[grep("_bal", iter2)], sep = "")

#############
#CREATE POLYGONS
#pathData* are the paths to the data files for the four cases: balance/unbalance & iter1/iter2
p2List <- list()
polygonList <- list()
for (pathData in c(pathData_I1_UB, pathData_I1_B, pathData_I2_UB, pathData_I2_B)) {
  parx <- NULL
  parx <-
    grep(pathData,
         c(
           pathData_I1_UB,
           pathData_I1_B,
           pathData_I2_UB,
           pathData_I2_B
         ))
  
  #from the path name of the data file, create the path name of the file where to save failing services
  strRem <- str_remove(pathData, ".xlsx")
  strRem <- tail(strsplit(strRem, "/")[[1]], 1)
  #create typeSet and iter from filenams
  temp <- c()
  temp <- as.data.frame(strsplit(strRem, "_"))
  typeSet <- c()
  if (any(temp == "unbal-profile")) {
    typeSet <- "unbal"
  } else{
    typeSet <- "bal"
  }
  iter <- c()
  if (any(temp == "iter2")) {
    iter <- 2
  } else{
    iter <- 1
  }
  
  #Create one dataset per type of deployment architecture mono/cqrs/role by reading the sheet with the corresponding name
  sheets <- excel_sheets(pathData)
  monoDataset <-
    as.data.frame(read_excel(pathData, sheet = grep("mono", sheets)))
  cqrsDataset <-
    as.data.frame(read_excel(pathData, sheet = grep("cqrs", sheets)))
  roleDataset <-
    as.data.frame(read_excel(pathData, sheet = grep("role", sheets)))
  
  #Retrieve configuration and operations names from column headers of each file
  configurationTypes <- colnames(monoDataset)[1:7]
  operations <-
    sort(colnames(monoDataset)[8:length(colnames(monoDataset))])
  
  configurationTypes1 <- colnames(roleDataset)[1:7]
  operations1 <-
    sort(colnames(roleDataset)[8:length(colnames(roleDataset))])
  
  configurationTypes2 <- colnames(cqrsDataset)[1:7]
  operations2 <-
    sort(colnames(cqrsDataset)[8:length(colnames(cqrsDataset))])
  
  #Reorder datsets by ordering columns alphabetically. Skip the following lines if datasets are already ordered
  monoOperationDataset <- monoDataset[, sort(operations)]
  roleOperationDataset <- roleDataset[, sort(operations1)]
  cqrsOperationDataset <- cqrsDataset[, sort(operations2)]
  #
  monoConfigurationsDataSet <- monoDataset[, configurationTypes]
  temp1 <-
    cbind(rep("mono", nrow(monoConfigurationsDataSet)), monoConfigurationsDataSet)
  colnames(temp1)[1] <- "ID"
  colnames(temp1)[2] <- "Users"
  loads1 <- temp1[, c("ID", "Users", "Metric")]
  mono <- cbind(loads1, monoOperationDataset)
  #
  roleConfigurationsDataSet <- roleDataset[, configurationTypes]
  temp2 <-
    cbind(rep("role", nrow(roleConfigurationsDataSet)), roleConfigurationsDataSet)
  colnames(temp2)[1] <- "ID"
  colnames(temp2)[2] <- "Users"
  loads2 <- temp2[, c("ID", "Users", "Metric")]
  role <- cbind(loads2, roleOperationDataset)
  #
  cqrsConfigurationsDataSet <- cqrsDataset[, configurationTypes]
  temp3 <-
    cbind(rep("cqrs", nrow(cqrsConfigurationsDataSet)), cqrsConfigurationsDataSet)
  colnames(temp3)[1] <- "ID"
  colnames(temp3)[2] <- "Users"
  loads3 <- temp3[, c("ID", "Users", "Metric")]
  cqrs <- cbind(loads3, cqrsOperationDataset)
  
  #Retrieve laods in natural order
  myMax <- max(mono$Users)
  mySplitting <- unique(mono$Users)
  myOrderedSplitting <- mySplitting[order(mySplitting)]
  #print(colnames(mono))
  
  #RETRIEVE THE OPERATIONAL PROFILE
  #From actual frequency values of loads
  aggregatedValues <- data.frame()
  aggregatedValues <-
    cbind(
      c(2, 50, 100, 150, 200, 250, 300, 350),
      c(0.008, 0.045, 0.069, 0.207, 0.239, 0.247, 0.155, 0.031)
    )
  
  #Set the directory for the results
  setwd("../../")
  resultsDirectory <- paste(workingDirectory, "/Results/", sep = "")
  if (!dir.exists("Results")) {
    dir.create("Results")
  }
  if (!dir.exists("Results/Plots")) {
    dir.create("Results/Plots")
  }
  
  setwd(scriptDirectory)
  #Run all the functions to compute the relative and the cumulative domain metric
  source("computeDM_FUNCTIONS.R")
  
  #baseline data
  usedDataFile <- rbind(mono, role, cqrs)
  #mean of microservice requests
  avg <- usedDataFile[usedDataFile$Metric == "Avg (sec)", -3]
  #standard deviation of microservice requests
  SD <- usedDataFile[usedDataFile$Metric == "SD (sec)", -3]
  #This is the frequency of microservice requests
  mixTemp <-
    usedDataFile[usedDataFile$Metric == "Mix % (take failure into account)", -3]
  
  #COMPUTE THE THRESHOLD
  noMicroServices <- length(operations)
  # To use with computeThreshold.R. Threshold is derived from the mono dataset
  dataBaseline <- mono
  
  par <- 2
  threshold <- NULL
  failingServices <- NULL
  failures <- NULL
  source("computeThreshold.R")
  
  #COMPUTE THE FAILING MICROSERVICES
  #threshold is the scalability requirement; avg is the response time of the MS; mixTemp is the frequency vector; computeRelativeMass requires to execute "computeDM_FUNCTIONS.R"
  failures <-
    computeRelativeMass(threshold, avg, mixTemp, noMicroServices)
  
  #total probability of non-failure per type of configuration mono/cqrs/role and load
  relativeMass <- failures[[1]]
  
  #micorservices that fail per type of configuration and load
  temp_failingServices <- failures[[2]]
  colnames(temp_failingServices) <-
    c("architectureType",
      "loadIntensity",
      "mix",
      "avg",
      "microservice")
  
  if (length(temp_failingServices[, 1]) > 1) {
    failingServices <-
      temp_failingServices[order(temp_failingServices[, 1]), ]
  } else{
    failingServices <- temp_failingServices
  }
  setwd("../")
  write.csv(
    failingServices,
    paste(
      "Results/",
      par,
      "_",
      strRem,
      "_failingMicroservice.csv",
      sep = ""
    ),
    row.names = F
  )
  
  #merged_title<-paste(iter,typeSet,paste("threshold:",par, sep=" "), sep=", ")
  merged_title <- paste(iter, typeSet, sep = ", ")
  
  #total probability of non-failure times probability of access of load per type of configuration and load
  domainMetricList <-
    computeDomainMetrics(relativeMass, aggregatedValues)
  
  #cumulative (over loads) probability of non-failure per type of configuration
  domainMetric <- lapply(domainMetricList, function(x) {
    sum(x$absoluteMass)
  })
  
  #plotting polygons
  print("Start plotting polygon")
  type <- names(domainMetricList)
  colnames(aggregatedValues) <- c("load intensity", "Domain metric")
  myCol = c("blue", "violet", "darkgreen")
  par(oma = c(0, 0, 0, 0))
  myFilerPol <-
    paste("Results/Plots/pol_iter", iter, typeSet, ".pdf", sep = "")
  pdf(file = myFilerPol)
  plot(
    aggregatedValues,
    xlim = c(-10, aggregatedValues[nrow(aggregatedValues), 1] + 10),
    pch = 19,
    pt.cex = 1,
    cex.axis = 1.3,
    ylim = c(0, max(aggregatedValues[, 2]) + 0.1),
    xaxt = "n",
    cex.lab = 1.5,
    bty = "n"
  ) +
    axis(
      1,
      at = c(8, aggregatedValues[-1, ][, 1]),
      labels = c(2, aggregatedValues[-1, ][, 1]),
      lwd.ticks = 1,
      cex.axis = 1.3
    )
  #print all polygons of the given configurations
  for (index in type) {
    i <- which(type == index)
    polygon(
      c(
        min(aggregatedValues[, 1]),
        t(domainMetricList[[index]]$Users),
        max(aggregatedValues[, 1])
      ),
      c(0, t(domainMetricList[[index]]$absoluteMass), 0),
      col = adjustcolor(myCol[i], alpha.f = 0.1),
      lty = 1,
      lwd = 0.7,
      border = myCol[i]
    )
    lines(
      domainMetricList[[index]][, c("Users", "absoluteMass")],
      type = "p",
      lwd = 0.7,
      pt.cex = 1.3,
      pch = 19,
      col = myCol[i]
    )
  }
  polygon(
    c(
      min(aggregatedValues[, 1]),
      aggregatedValues[, 1],
      max(aggregatedValues[, 1])
    ),
    c(0, aggregatedValues[, 2], 0),
    col = adjustcolor("white", alpha.f = 0.1),
    pch = 19,
    lty = 1,
    lwd = 0.7,
    border = "darkblue",
    cex = 1.3
  )
  #print legend with DM values
  legendNames <- c()
  architectures <- c("Monolith", "RS", "CQRS")
  names(domainMetric) <- architectures
  for (i in 1:length(names(domainMetric))) {
    legendNames[i] <-
      paste(names(domainMetric)[i], " DM: ", domainMetric[i], sep = "")
  }
  text(
    aggregatedValues,
    labels = round(aggregatedValues[1:nrow(aggregatedValues), 2], 3),
    pos = 3,
    col = "black",
    cex = 1.3
  )
  legend(
    200,
    0.265,
    legendNames,
    text.width = 2.3,
    col = myCol,
    lty = 1,
    box.lty = 0,
    lwd = 4,
    yjust = 0,
    cex = 1.3,
    seg.len = 0.8,
    inset = 0,
    y.intersp = 1.5
  )
  graphics.off()
  print("End plotting polygon")
}

setwd(scriptDirectory)
#CREATE SPIDERS, RIDGE PLOTS
source("NewSpiders.R")

#WILCOXON TEST
setwd(scriptDirectory)
source("WilcoxonAnalysis.R")

options(warn = defaultW)

