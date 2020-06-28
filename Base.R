# Included libraries
library("tidyverse") # For all sorts of thing
library("date") #  To work with dates
library("lubridate")
library("SpatialExtremes")
library("extremefit")

library(MASS)
library(fitdistrplus)
library(actuar)
library(laeken) # for pareto shape estimation


#Importing the data

RiverFlowData <- read.table("8006_gdf.csv", header=FALSE, sep=",", 
                            col.names=c("Date","Flow"), skip=20, 
                            colClasses = c("character","numeric"))
attach(RiverFlowData)

# Check data for completeness and prepare for analysis
source("RiverFlowDataPrep.R")

# Splitting out trend and seasonal factors to get noise
source("PatidarSeasonaility.R")

# Create an HMM as per Patidar
source("PatidarHMM.R")

# Reconsidering Patidar trend calcualtion
source("AlteredTrend.R")

# Building a Mixed Model to explain the signal distribution
source("SignalMixedModel.R")



# 
