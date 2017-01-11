#######
#read in eye tracking file
#######
files_eye<-list.files(path=data.folder,pattern='gaze.txt',full.names=T)
#first read in whole text file
text_file<-scan(files_eye[1],character(0),sep="\n")
regexp<-"Trial Started"
regexp2<-"Trial Ended"
regexp3<-"^[[:digit:]]{10}"
#where are trial start commands?
start_trial_lines<-which(str_detect(text_file,regexp))
end_trial_lines<-which(str_detect(text_file,regexp2))
#when did trials start?
trial_start_times<-as.numeric(str_extract(text_file[start_trial_lines],pattern=regexp3))
trial_end_times<-as.numeric(str_extract(text_file[end_trial_lines],pattern=regexp3))
#save remaining data
cat(text_file[-c(start_trial_lines,end_trial_lines)],file="file.txt",sep="\n")
rm(text_file)
eye.data<-fread("file.txt")
#insert trial numbers
eye.data$trial<-0
helper2<-eye.data$trial
helper<-eye.data$V1
for (i in 1:length(trial_start_times))
  helper2[helper>=trial_start_times[i]&helper<=trial_end_times[i]]<-i
eye.data$trial<-helper2
rm(helper,helper2)
#remove all eye tracking not belonging to particular trials
eye.data<-
  eye.data %>% 
  filter(trial>0)
##########
#plot eyetracking of a trial
#########

ggplot(aes(x=as.integer(V1/1000),y=V2),data=subset(eye.data,trial<=20))+geom_line()+facet_wrap(~trial,scales="free_x")
hist(eye.data$V2)

