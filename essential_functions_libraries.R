#essential libraries and functions needed
library(tidyverse)
library(data.table)
library(dtplyr)
library(stringr)
library(jsonlite)
library(bit64)

get.dropbox.folder <- function() {
  file_name<-list.files(paste(Sys.getenv(x = "APPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)
  if (length(file_name)==0){
    file_name<-list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),"Dropbox", sep="/"), pattern = "*.json", full.names = T)}
  
  file_content<-fromJSON(txt=file_name)$personal
  file_content<-file_content$path
  return(file_content)
}
db.folder<-get.dropbox.folder()
dir.ddm<-(paste(db.folder,"/DMC_Europe_2016-update/",sep=""))
#setwd("C:/Users/ulf/Dropbox/DMC_Europe_2016-update/")
dir.ddm<-("D:/UlfT/Experimente/Human_experiments/Confidence/Confidence_Task_Magda/DMC_160825/")
# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source (paste(dir.ddm,"/tutorial/file_utils.R",sep=""))
old_wd<-getwd()
setwd(dir.ddm)
load_model("lba_B.R")
setwd(old_wd)
