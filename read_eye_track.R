#######
#read in eye tracking file
#######
# prepare main file (my.data) to hold variables associated with eye gaze
# the following variables will be written into my.data for each trial
# 1. duration left, right, middle non fixation. ratio of left and right
# 2. first fixation and last fixation changes between left and right

#source the necessary functions
source("eye_tracker_functions.R")

#get file names 
files.eye<-list.files(path=data.folder,pattern='gaze.txt',full.names=T)
#excluded players
for(i in 1: length(part.excl)){
  helper<-str_detect(files.eye,pattern=paste(part.excl[i]))
  files.eye<-files.eye[!helper]
}
rm(helper)
#create containers for data frames
eye.data.list<-list()
eye.analysis.list<-list()

#get pp number assuming that pps have a four digit number
eye.participant<-NULL
for(i in 1:length(files.eye)){
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  #first clean data this creates file.txt that is then read by the next function
  #clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  #eye.data.list[[i]]<-create_eye_track_data()
  
  #get finished analysis file
  eye.analysis.list[[i]]<-calc_eye_tracker_values(eye.data.list[[i]])
}
#add these to my.data
#prepare my.data
namevector<-names(eye.analysis.list[[1]])
nv<-which(names(my.data)%in%namevector)
my.data[,(namevector):=NULL]
my.data[,(namevector):=list(0,'','',0,0,0,0,0)]
##############
for (i in 1:length(files.eye)){
  sel<-which(my.data$participant==eye.participant[i])
  my.data[sel,(namevector):=eye.analysis.list[[i]]]
}
#ok now we can start analysing the data
