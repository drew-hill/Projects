library(data.table)
library(lubridate)
library(plyr)

# set number of parallel cores to total - 2 (6 on drew's macbook)
library(parallel)
library(doMC)
registerDoMC(detectCores()-2)
# getDoParWorkers()



##################### Accelerometry (5.7gb)
######### 
# Situating the files of interest
# set path to file repository
files.path <- '~/Parkinsons Data/accel_only'
# list all folders in directory
files.folders <- list.files(files.path,full.names=TRUE)
# list all .csv filenames
filenames <- list.files(files.folders, pattern = '*.csv', full.names=TRUE)

# # prepare a list for 10% random sampling
# set.seed(6)
# file.rand <- sample(filenames,length(filenames)/10,replace=F)

######### 
# Importing the accel files of interest

import.accel.fxn <- function(file){
	temp <- fread(file)
	temp$id <- as.factor(strsplit(file,'_')[[1]][3])
	temp$device <- as.factor(strsplit(file,'_')[[1]][4])
	temp[,time := ymd_hms(time)]

	 assign(paste(temp$id[1],strsplit(file,'_')[[1]][13],gsub('.csv','',strsplit(file,'_')[[1]][14]),sep='_'), temp)
	}

# read files in as list and modify using the function prepared above (80sec)
list.accel <- llply(filenames, import.accel.fxn, .parallel=TRUE)

#combine all files, row-wise, to produce one long data table
dt.master.accel <- rbindlist(list.accel, fill=T)

# sort by datetime for each individual
dt.master.accel <- dt.master.accel[order(id,time)]

# adjust typo-ed id names
dt.master.accel[id=='DAISEY', id := 'DAISY']
dt.master.accel[id=='LILLY', id := 'LILY']

# take a look at odd ids
grep('TESTCLIQ',filenames)
	# filenames[5235]
	# filenames[5236]

grep('DEFAULT',filenames)
	# filenames[2116]

# drop ids TESTCLIQ and DEFAULT
setkey(dt.master.accel,id)
dt.master.accel <- dt.master.accel[!"TESTCLIQ"]
dt.master.accel <- dt.master.accel[!"DEFAULT"]



##################### GPS (751 mb)
######### 
# Situating the files of interest
# set path to file repository
files.path <- '~/Parkinsons Data/gps_only'
# list all folders in directory
files.folders <- list.files(files.path,full.names=TRUE)
# list all .csv filenames
filenames <- list.files(files.folders, pattern = '*.csv', full.names=TRUE)

# # prepare a list for 10% random sampling
# set.seed(6)
# file.rand <- sample(filenames,length(filenames)/10,replace=F)

######### 
# Importing the gps files of interest

import.gps.fxn <- function(file){
	temp <- fread(file)
	temp$id <- as.factor(strsplit(file,'_')[[1]][3])
	temp[,time := ymd_hms(time)]
	temp$diffSecs <- NULL

	 assign(paste(temp$id[1],strsplit(file,'_')[[1]][13],gsub('.csv','',strsplit(file,'_')[[1]][14]),sep='_'), temp)
	}

# read files in as list and modify using the function prepared above
list.gps <- llply(filenames, import.gps.fxn, .parallel=TRUE)

#combine all files, row-wise, to produce one long data table
dt.master.gps <- rbindlist(list.gps, fill=T)

# sort by datetime for each individual
dt.master.gps <- dt.master.gps[order(id,time)]

# adjust typo-ed id names
dt.master.gps[id=='DAISEY', id := 'DAISY']
dt.master.gps[id=='LILLY', id := 'LILY']


# drop ids TESTCLIQ and DEFAULT
setkey(dt.master.gps,id)
dt.master.gps <- dt.master.gps[!"TESTCLIQ"]





##################### Participant info
#########
# read in the csv
info <- as.data.table(read.csv('~/Parkinsons Data/ParticipantInfo.csv'))

# assign disease status
info[Age.when.Diagnosed.with.Parkinson.s.Disease %in% c('control','Control'), pd:= 0]
info[!Age.when.Diagnosed.with.Parkinson.s.Disease %in% c('control','Control'), pd:= 1]

# gender
info[Gender=='Male', male := 1]
info[Gender=='Female', male := 0]

# years since diagnosis
info[pd==1,time_since_diag := as.numeric(Current.Age) - as.numeric(Age.when.Diagnosed.with.Parkinson.s.Disease)]

#rename id
info <- rename(info,c("Code.Name"="id"))



###################### Combine datasets
#########
# Full outer join of accelerometry and gps data
dt <- merge(dt.master.accel,dt.master.gps, by = c('id','time'), all= TRUE)

# Full outer join of accelerometry and gps data
dt <- merge(dt,info[,c('id','pd','male','time_since_diag','score'),with=FALSE], by = c('id'), all= TRUE)



# save as RDS
system.time(saveRDS(dt, file='~/Parkinsons Data/RDS files/dt.master.RDS'))
