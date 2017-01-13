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

#create containers for data frames
eye.data.list<-list()
eye.analysis.list<-list()

#get pp number assuming that pps have a four digit number
eye.participant<-NULL
for(i in 1:length(files.eye)){
  eye.participant[i]<-as.numeric(str_extract(files_eye[i],"[[:digit:]]{4}"))
  #first clean data this creates file.txt that is then read by the next function
  clean_eye_tracker_data(i)
  
  #then use this data to create a cleaned eye tracking file with additional calculations
  eye.data.list[[i]]<-create_eye_track_data()
  
  #get finished analysis file
  eye.analysis.list[[i]]<-calc_eye_tracker_values(eye.data.list[[i]])
}
#add these to my.data
#prepare my.data
namevector<-names(eye.analysis.list[[1]])
my.data[,(namevector):=list(0,'','',0,0,0,0,0)]
sel.col<-which(names(my.data)%in%namevector)
##############
for (i in length(files.eye)){
  my.data[my.data$participant==eye.participant[i],(namevector):=eye.analysis.list[[i]]]
}
#ok now we can start analysing the data


help.data<-subset(my.data,participant==1842)
length(help.data$zConf)
selector<-(help.data$trials.thisN+1)%in%ec.wide$trial
help.data<-help.data[selector,]
help.data$ratio<-ec.wide$ratio
help.data$ratio2<-ifelse(help.data$ratio>0.5,help.data$ratio-0.5,0.5-help.data$ratio)




ggplot(aes(y=zConf,x=I(),col=c_choice),data=help.data)+geom_point()+facet_wrap(~social3)+stat_smooth()
ggplot(aes(x=key_resp_direction.rt,col=ratio2,y=zConf),data=subset(help.data,social3="none"&key_resp_direction.rt<2))+geom_point()
m.1<-lm(zConf~ratio2*key_resp_direction.rt,data=help.data)
summary(m.1)
help.data$key_resp_direction.rt



