#####################################
# READ IN EYE TRACKING FILE
####################################

# prepare main file (my.data) to hold variables associated with eye gaze
# the following variables will be written into my.data for each trial
# 1. duration left, right, middle non fixation. ratio of left and right
# 2. first fixation and last fixation changes between left and right

#####################################################################

# clear the workspace
rm(list=ls())

#####################################################################

### PRE-REQs 

# get the functions and libraries
source("essential_functions_libraries.R")
source("eye_tracker_functions.R")
#source("pupil_function.R")

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidence_attention.RData")
  print("loading my.data")
}

# get dropbox folder
db.folder<-get.dropbox.folder()

# current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_CURRENT\\Data",sep=""))

# participants to exclude
part.excl<-c(1811,1821,1851,3352,2462,2482,7851)

#####################################################################

### READ IN EYE TRACK DATA

# get names of the eye traking files
files.eye<-list.files(path=data.folder,pattern='gaze.txt',full.names=T)

# exclude participants
for(i in 1: length(part.excl)){
  helper<-str_detect(files.eye,pattern=paste(part.excl[i]))
  files.eye<-files.eye[!helper]
}
rm(helper)

# create containers for data frames
eye.data.list<-list()
eye.analysis.list<-list()

# get the participant ID (which is 4-5 digits long)
eye.participant<-NULL
for(i in 1:length(files.eye)){
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  
  #Create a file.txt that is then read by the next function
  clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  eye.data.list[[i]]<-create_eye_track_data()
  
  #get finished analysis file
  eye.analysis.list[[i]]<-calc_eye_tracker_values(eye.data.list[[i]])
}
#add these to my.data
#prepare my.data
namevector<-names(eye.analysis.list[[1]])
nv<-which(names(my.data)%in%namevector)
my.data[,(namevector):=NULL]
my.data[,(namevector):=list(0,'','',0,0,0,0,0,0,0,0,0,0)]

##############

for (i in 1:length(files.eye)){
  sel<-which(my.data$participant==eye.participant[i])
  my.data[sel,(namevector):=eye.analysis.list[[i]]]
}

save(my.data,file='confidence_attention_ET.RData') 


# MAKE THE EYE.DATA INTO A DATA.FRAME AS WELL and add to it the timing of fixations
eye.trial.data<-list()

for(i in 1:length(files.eye)){
  print(eye.participant[i])
  eye.participant[i]<-as.numeric(str_extract(files.eye[i],"[[:digit:]]{4,5}"))
  
  eye.trial.data[[i]]<-eye_tracker_fixation(eye.data.list[[i]])
  eye.trial.data[[i]]$participant<-eye.participant[i]
  print(eye.participant[i])
}

my.eye<-rbindlist(eye.trial.data,use.names=TRUE,fill=TRUE)

save(my.eye,file="confidence_attention_ET_trial.RData")

# save only the important bits so it is easier to deal with
my.eye.lt<-select(my.eye,V4,trial,fix,change,fixPos,fixNum,time,timeTrial,participant)

# ADD SOCIAL INFO AND CORRECT/INCORRECT - WILL MAKE THIS INTO A FUNCTION EVENTUALLY

my.eye.lt$social3<-""
my.eye.lt$c_choice<-""

# FOR EACH PARTICIPANT look up in my.data which trials have no social info (social==0),
# select those trial in the my.eye and add none to their social3 column
for (iparticipant in unique(my.eye.lt$participant)){
  
  no.info.trials <- my.data$trial[my.data$social==0&my.data$participant==iparticipant]
  my.eye.lt$social3[my.eye.lt$trial%in%no.info.trials&my.eye.lt$participant==iparticipant] <- "none"
  
  c_choice.trials <- my.data$trial[my.data$c_choice=="correct"&my.data$participant==iparticipant]
  my.eye.lt$c_choice[my.eye.lt$trial%in%c_choice.trials&my.eye.lt$participant==iparticipant] <- "correct"
  
  in_choice.trials<- my.data$trial[my.data$c_choice=="incorrect"&my.data$participant==iparticipant]
  my.eye.lt$c_choice[my.eye.lt$trial%in%in_choice.trials&my.eye.lt$participant==iparticipant] <- "incorrect"
  
}

save(my.eye.lt,file="confidence_attention_ET_trial_LT.RData")
