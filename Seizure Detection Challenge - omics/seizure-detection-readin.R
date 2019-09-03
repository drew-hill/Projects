library(R.matlab)
library(plyr)
library(data.table)

# set number of parallel cores to total - 2 (6 on drew's macbook)
library(parallel)
library(doMC)
registerDoMC(detectCores()-2)
# getDoParWorkers()

#
# import all '.mat' files in 'clips' folder as data tables
#

# list all files in all folders in 'clips' folder
clips.path <-'~/Seizure Data/clips' 
clips.folders <- list.files(clips.path,full.names=TRUE)
filenames <- list.files(clips.folders, pattern = '*.mat', full.names=TRUE)

#take 2% random sample
set.seed(6)
file.rand <- sample(filenames,length(filenames)/50,replace=F)

# import function (each llply should probably take ~ 4.5-7 minutes)

# system.time(ldf <- lapply(filenames[1:10], readMat, .parallel= T))
# system.time(ldf_dt	<- llply(ldf,as.data.table, .parallel=T))

import.name.fxn.dog <- function(file){
	 temp <- readMat(file)
	 temp <- as.data.table(temp)
	 temp[,channels := as.numeric(gsub("^.*?_Ecog_c",'',as.character(channels)))]
	 id <- unlist(strsplit(strsplit(file,'/')[[1]][13],'.mat'))
	 temp$type  <- strsplit(id,"_")[[1]][1]
	 temp$id <- as.numeric(strsplit(id,"_")[[1]][2])
	 temp$state <- strsplit(id,"_")[[1]][3]
	 temp$segment_no <- as.numeric(strsplit(id,"_")[[1]][5])
	 temp$index <- seq.int(nrow(temp))

	 # write.csv(temp, paste('~/Seizure Data/csv/', paste(temp$id[1],temp$segment_no[1],sep='_'),'.csv', sep=''), row.names=F)

	 assign(paste(temp$id[1],temp$segment_no[1],sep='_'), temp)
	 as.data.table(get(paste(temp$id[1],temp$segment_no[1],sep='_')))
	}


# file <- file.rand[grep('Patient_1',file.rand,ignore.case=T)][1]

import.name.fxn.hum <- function(file){
	 temp <- readMat(file)
	 temp <- as.data.table(temp)
	 temp[,node_loc:=gsub("_",'',as.character(channels))]
	 temp[,node_loc:=gsub("[0-9]",'',as.character(node_loc))]
	 temp[,channels := gsub("[[:alpha:]]",'',as.character(channels))]
	 temp[,channels:=as.numeric(gsub("_",'',as.character(channels)))]
	 id <- unlist(strsplit(strsplit(file,'/')[[1]][13],'.mat'))
	 temp$type  <- strsplit(id,"_")[[1]][1]
	 temp$id <- as.numeric(strsplit(id,"_")[[1]][2])
	 temp$state <- strsplit(id,"_")[[1]][3]
	 temp$segment_no <- as.numeric(strsplit(id,"_")[[1]][5])
	 temp$index <- seq.int(nrow(temp))
	 temp[state=='interictal',state:= '0']
	 temp[state=='ictal',state:= '1']
	 temp[,state:=as.numeric(state)]

	 # write.csv(temp, paste('~/Seizure Data/csv/', paste(temp$id[1],temp$segment_no[1],sep='_'),'.csv', sep=''), row.names=F)

	 assign(paste(temp$id[1],temp$segment_no[1],sep='_'), temp)
	 as.data.table(get(paste(temp$id[1],temp$segment_no[1],sep='_')))
	}


system.time(mylist.dog <- llply(file.rand[grep('Dog',file.rand,ignore.case=T)], import.name.fxn, .parallel=TRUE))
system.time(mydata.dog <- rbindlist(mylist.dog, fill= T))
saveRDS(mydata.dog, file='~/Desktop/seizure_dog.rds')
rm(mydata.dog)
rm(mylist.dog)

system.time(mylist.patient <- llply(file.rand[grep('Patient',file.rand,ignore.case=T)], import.name.fxn.hum, .progress='text'))
system.time(mydata.patient <- rbindlist(mylist.patient, fill= T))
system.time(saveRDS(mydata.patient, file='~/Desktop/seizure_patient.rds'))
rm(mydata.patient)
rm(mylist.patient)

