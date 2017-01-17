############################
#READ IN BEHAVIOURAL DATA
############################

# Reads in behavioural and demographic data from relevant .csv files, 
# adds few additional columns useful for further analysis, and saves the files

#############################################################################

# clear the workspace
rm(list=ls())

#############################################################################

### PRE-REQs 

# get the functions and libraries
source("essential_functions_libraries.R")

#get dropbox folder
db.folder<-get.dropbox.folder()

#current data directory (on dropbox)
data.folder<-c(paste(db.folder,"\\UlfGesaRasmus\\Confidence_Task_Magda\\confidence_grates\\Versions of the Task\\arrow_CURRENT\\Data",sep=""))

# participants to exclude
part.excl<-c(1811,1821,1851,3352)
###############################################################################

### READ IN DEMOGRAPHIC DATA

# Extract data from Participants_Fragebögen and save as my.demographics
files<-list.files(path=data.folder,pattern='*gen.csv',full.names=T)
my.demo<-data.frame(lapply(files, fread))

# Delete unwanted columns
keep <- c("ID.used","Alter","Geschlecht")
my.demo <- my.demo[keep]

# SAVE IT
save(my.data,file='confidence_attention_DM.RData')

################################################################################

### READ IN TRIAL DATA

# Extract data from Phase2 from all participants and save them into a single file: my.data
files<-list.files(path=data.folder,pattern='*phase2.csv',full.names=T)
my.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 

# exclude participants
my.data<-subset(my.data,!participant %in% part.excl)

# SOME BASIC STATS

#how many trials without answer (do not these delete trials as trials will be matched to eye tracking data)
no.ans <- sum(is.na(my.data$key_resp_direction.rt))

# Calculate the number or participants
no.part<-length(unique(my.data$participant))


# ADD NEW COLUMNS

# CONF - confidence corrected  for left and right choice
my.data$conf<-my.data$PDW.response
my.data$conf<-(ifelse(my.data$conf<=0.5,0.5-my.data$conf,my.data$conf-0.5))

# ZCONF - Normalized confidence ratings and put them into a new column
my.data[,zConf:=scale(as.numeric(conf,na.rm=T)),by=participant]

# SI - Was social info presented or not?
my.data$si<-factor(ifelse(str_detect(my.data$condition,c("both*")),0,1),labels=c("no social\ninfo","social\ninfo"))

# SOCIAL - WAS SOCIAL INFO INVALID, VALID or INCONGRUENT in numbers
my.data$social<-0
my.data$social[str_detect(my.data$condition,c("congruent*"))]<-1
my.data$social[str_detect(my.data$condition,c("incongruent*"))]<- -1

# SOCIAL2, 3 - in words
my.data$social2<-factor(my.data$social,labels = c("invalid\nsocial information","no social\ninformation","valid\nsocial information"))
my.data$social3<-factor(my.data$social,labels=c("invalid","none","valid"))

# C_CHOICE - CORREcT or INCORRECT response
my.data$c_choice<-factor(my.data$key_resp_direction.corr,labels=c("incorrect","correct"))

# zRT - normalized RT (????)
my.data[,zRT:=scale(as.numeric(key_resp_direction.rt,na.rm=T)),by=participant]


# SAVE IT
save(my.data,file='confidence_attention.RData') 

################################################################################


