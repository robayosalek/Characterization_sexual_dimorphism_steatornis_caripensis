################################################################################
#                       Data analysis of oil-bird's data set                   #
#                                 Andres F. Robayo                             #
#                                 Adriana Maldonado                            #
#                                  Version: 8.6                                #
#                                Date: 26/07/2022                              #
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Clean memory and remove all objects
rm(list=ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load libraries
# install.packages("psych")

library(psych)
library(dplyr)
library(readr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load data for AFRS
#
# Define the inputs
#
# Path to raw data and working directory ICN
setwd("C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_ICN")
datapath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_ICN/"

# Path to raw data and working directory Humboldt
setwd("C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_IAVH")
datapath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_IAVH/"

# Path to raw data and working directory wild individuals
setwd("C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_silvestres_2022_1")
datapath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_silvestres_2022_1/"

#
# Define the outputs
#
# ICN
outpath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/derived_data/Guacharos_ICN/"

# Humboldt
outpath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/derived_data/Guacharos_IAVH/"

# Wild individual
outpath <- "C:/Users/piper/Dropbox/AndresRobayo/Tesis/medidas_espectro/derived_data/Guacharos_silvestres/"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Load data for AMC
# Path to raw data for ICN
setwd("~/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_ICN")
datapath <- ("~/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_ICN")

# Path to raw data for IAVH
setwd("~/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_IAVH")
datapath <- "~/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_espectro/raw_data/Guacharos_IAVH/"

# The output of this script 
# ICN
outpath <- "~/Dropbox/Work/UR/Mentoring/2022/AndresRobayo/Tesis/medidas_espectro/derived_data/"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Read files in the folder
#
# We are going to create a list, where it is going to be the data set. So to
# simplify, because the data are many excel files, we put in the list the data path
#
filenamelist <- list.files(path = datapath)

# Create storing object
#
# This is created because we want to store the results in a object. The reason of 
# why it is 8 columns, it's because we have 8 variables
#
reflectancevalues <- data.frame(matrix(nrow = length(filenamelist), ncol = 8, NA))
colnames(reflectancevalues) <- c("name", "museum"  ,"ID", "part" , "peak_nm_all", "peak_int_all", "peak_nm_visible", "peak_int_visible")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# loop through the files to reach the peak in the visible spectrum

for(ii in 1:length(filenamelist)) {
  
  # extract filename
  filename <- filenamelist[ii]
  
  # read the file
  rdata <- as.data.frame(read.table(filename))
  
  # replace comma so it can be read as a number
  rdata$V1 <- gsub(',', '.', rdata$V1) # (X) Wavelengths displays the wavelength of the x-axis in units of nano-meters
  rdata$V2 <- gsub(',', '.', rdata$V2) # (Y) Intensity (counts) 
  
  # convert from text to numeric
  rdata[] <- lapply(rdata, function(x) as.numeric(as.character(x)))
  rdata
  
  # Check
  sapply(rdata, class)
  
  # Check range for both variables  
  # Wavelength
  range(rdata$V1)
  # Intensity
  range(rdata$V2)
  
  # Extract the peak of max intensity
  max_value <- rdata[which.max(rdata$V2),] 
  
  # Corresponding wavelength 
  max_value <- rdata[which.max(rdata$V1),] 
  
  # split file name
  fname <- strsplit(filename, split = "_")

  # add name to data
  reflectancevalues$name[ii] <- substr(filename, 1, (nchar(filename)-7))
  reflectancevalues$ID[ii] <- fname[[1]][5]
  reflectancevalues$part[ii] <- paste(fname[[1]][5], fname[[1]][6], sep = "_")
  reflectancevalues$museum[ii] <- fname[[1]][4]
  
  # max values
  reflectancevalues$peak_nm_all[ii] <- as.numeric(max_value[1])
  reflectancevalues$peak_int_all[ii] <- as.numeric(max_value[2])
  
  # subset only values for visible spectrum  from 380 - 780 nm
  visible <- rdata[ which(rdata$V1 > 380 & rdata$V1 < 780),]
  
  # check range for visible spectrum  
  range(visible$V1)
  range(visible$V2)
  
  # extract the peak of max intensity in the visible
  max_value_vis <- visible[which.max(visible$V2),] 
  
  # store visible spectrum information
  reflectancevalues$peak_nm_visible[ii] <- as.numeric(max_value_vis[1])
  reflectancevalues$peak_int_visible[ii] <- as.numeric(max_value_vis[2])
  
}
# wavelength range of optical radiation
# https://light-measurement.com/wavelength-range/

# ONLY CREATE ONCE AT THE BEGINING 
# create empty object 
reflectanceIndivParts <- NULL
reflectanceIndiv <- NULL

# calculate the average for each individual for each measure and store temp
reflectanceIndivParts_temp <- aggregate(cbind(peak_nm_all, peak_int_all, peak_nm_visible, peak_int_visible) ~ name, data = reflectancevalues, FUN = mean, na.rm = TRUE)
reflectanceIndiv_temp <- aggregate(cbind(peak_nm_all, peak_int_all, peak_nm_visible, peak_int_visible) ~ ID, data = reflectancevalues, FUN = mean, na.rm = TRUE)

# add info to the main dataset
reflectanceIndivParts_temp$IDpart <- unique(reflectancevalues$part)#UV
reflectanceIndivParts_temp$museum <- reflectancevalues$museum[1]#UV
reflectanceIndiv_temp$museum <- reflectancevalues$museum[1]

# Append 
reflectanceIndivParts <- rbind(reflectanceIndivParts, reflectanceIndivParts_temp)
reflectanceIndiv <- rbind(reflectanceIndiv, reflectanceIndiv_temp)

# export results
write.csv(reflectanceIndivParts, file=paste(outpath, "reflectancia.csv", sep = ""), row.names=FALSE)
write.csv(reflectanceIndiv, file=paste(outpath, "reflectancia_Ind.csv", sep = ""), row.names=FALSE)

# export results (Sylvester specimens)
write.csv(reflectanceIndivParts, file=paste(outpath, "reflectancia_s.csv", sep = ""), row.names=FALSE)
write.csv(reflectanceIndiv, file=paste(outpath, "reflectancia_Ind_s.csv", sep = ""), row.names=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#end
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
