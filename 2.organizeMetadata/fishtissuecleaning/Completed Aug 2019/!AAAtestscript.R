#The purpose of this code is to organize the columns of an excel sheet to be in the
#correct order and then export the data as a CSV. My method matches up the columns of the input
#to how we want them to be in the output. You could also do this using the SELECT function from
#tidyverse and that even may be easier, but this works.


rm(list=ls()) #clear all variables

#Make sure to set the path name eachtime you run the code.
#I could not get this to work in the T: drive
path <- "C:/Users/kkn73995/Desktop/Test"
setwd(path)

##########################################
#install(tidyverse)
library("rio")
library(dplyr) 
library(stringr)
library(readxl)

#Collect the filenames in the same folder as this script
filenames<-list.files()
filenames<-filenames[-1] #Do not apply the function to the first or the second filenames
filenames<-filenames[-1] #(That would be this R script and the list of column names)

for(strName in filenames) {
  
  
  path2 <- paste(path, strName, sep='/')
  PCB2000 <- read_excel(path2)
  strName <- str_replace(strName, "xls", "csv")
  strName <- str_replace(strName, "xslx", "csv")
  strName
  
  P <- PCB2000
  new<-read.csv(file="!AAA_lookup.csv",header=F)
  new <- apply(new, c(1,2), as.character)
  colnames(new) <- new[2,]
  
  output <- data.frame(matrix(vector(), nrow(PCB2000), ncol(new), dimnames=list(c(), colnames(new))),
                       stringsAsFactors=F)
  
  for(i in 1:length(colnames(output))) {
    if(isTRUE(as.character(colnames(output)[i]) %in% colnames(P))) {
      output[as.character(colnames(output)[i])] <- P[as.character(colnames(output)[i])]
      P <- P[-grep(as.character(colnames(output)[i]), colnames(P))]
    } else {
      output[as.character(colnames(output)[i])] <- NA
    }
  }
  
  #I don't know if this is necessary, but I appended the extra columns to the end so the information 
  #is not lost.
  output <- cbind(output, PCB2000)
  
  export(output, strName)
  
}