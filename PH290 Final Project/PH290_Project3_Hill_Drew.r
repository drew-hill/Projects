library(data.table)
library(plyr)
require(bit64)
library(ff)
library(ggplot2)
library(biglm)

# library(parallel)
# library(doMC)
# registerDoMC(detectCores()-2)

##################################################
##
## Read in the dataset
## a random subset of 1,000,000 rows
## no more than 100,000 lines at a time
## only the columns of interest
##
##################################################

targetfile <- '~/Data/Computing Project/ss13hus.csv.bz2'

######### the columns of interest
# read in first line of zipped file to get column names & positions
header <- read.csv(targetfile, nrows=100, header=T, sep=',')

	# name columns of interest
	col_list <- c('REGION', 'ST', 'ADJHSG', 'ADJINC', 'NP', 'ACR', 'BDSP', 'ELEP', 'GASP', 'RMSP', 'VEH', 'WATP', 'FINCP', 'HINCP')

	# find index of columns of interest
	# 7  8  9 10 12 14 17 21 24 32 45 46 49 55
	which(colnames(header) %in% col_list)

	# create vector to be used in "col_clases" to select only for the 14 columns
	# of interest. colClasses argument can be used to leave some columns unread (
	# any marked "NULL"); NA will use the default approach of including the column
	# and determining class, but we know they're all numeric, so we'll specify 
	# because it apparently makes the process run faster
	str(header)
	class_vec <- c(
		rep("NULL",6),
		rep('numeric',4),
		"NULL",
		'numeric',
		"NULL",
		'numeric',
		rep("NULL",2),
		'numeric',
		rep("NULL",3),
		'numeric',
		rep("NULL",2),
		'numeric',
		rep("NULL",7),
		'numeric',
		rep("NULL",12),
		rep('numeric',2),
		rep("NULL",2),
		'numeric',
		rep("NULL",5),
		'numeric',
		rep("NULL",150)
		)

	#confirm length
	count(class_vec)


########## create index of 1 mil random samples
# read number of rows (in unix)
# 7219000, excluding colnames)
	# bzcat ~/Computing\ Project/ss13hus.csv.bz2 | wc -l 
row_num_total <- 7219000

# Randomly select 1 mil row indices, seed @ 1000
samp_size <- 1000000 
set.seed(1000)
samp <- as.data.table(sample(7219000, samp_size, replace = FALSE))
samp <- rename(samp,c('V1'='row'))

subset <- sort(as.numeric(samp$row))

########## read.csv
# (elapsed: 1031 sec)

# create 100 manageable chunks (Read < 100,000 rows into memory at a time)
index <- 0
chunksize <- row_num_total/100
number_of_chunks_to_do <- 100
con <- file(description=targetfile,"r")

system.time(
repeat {
        index <- index + 1
        print(paste('Processing rows:', index * chunksize))
        print(paste('iteration #',index,sep=" "))

        # stop/break after 100 chunks 
        if (index > number_of_chunks_to_do){
                print('Processed all files!')
                break}
        
        else {
        	# if first chunk, header=T and append = F
        	if (index==1){

        		# read the 14 columns of interst into memory, 'chunksize' # of rows at a time
		        dataChunk <- read.csv(con, nrows=chunksize, header = T, skip=0, colClasses=class_vec, sep=",")

		        # write to CSV only those rows that correspond to our random subset, keeping column names
		        write.csv(dataChunk[rownames(dataChunk) %in% subset,],'~/Computing Project/csv/dataChunk.csv', row.names=F)

		        # print to keep a record of activity
		        print(head(dataChunk,5))}

		    # if 2nd chunk or later, header = F and append = T
			else {
				    # read the 14 columns of interst into memory, 'chunksize' # of rows at a time
			        dataChunk <- read.csv(con, nrows=chunksize, header = F, skip=0, colClasses=class_vec, sep=",")

				    # append to the original CSV (chunk # 1) only those rows that correspond to our random subset
		         	# because rowname restarts every iteration, add chunksize*index-1
			        write.csv(dataChunk[(as.numeric(rownames(dataChunk)) + (chunksize*(index-1))) %in% subset,],'~/Computing Project/csv/dataChunk.csv', append=T, row.names=F)
		        
		        
		        	# print to keep a record of activity
			        print(head(dataChunk,5))}
		        }}
)

close(con)

# check for consistency
str(fread('~/Computing Project/csv/dataChunk.csv'))


	######### alternate method... too slow (3 times slower)
		# # create a sorted, incremental list of the offset of each subset value
		# # account for header by adding 1 to the first row
		# incremental_subset <- c((sort(subset)[1]+1),diff(sort(subset)))

		# # function to read each individually subset row
		# csv.fxn <- function(x){
		# 	as.data.table(read.csv(con, skip=x, nrows=1, header = F, colClasses=class_vec, sep=","))
		# }

		# con <- file(description=targetfile,"r")
		# system.time({
		# my.list <- llply(incremental_subset, csv.fxn, .progress='text')
		# ;
		# boop <- rbindlist(my.list, fill= T)
		# })
		# close(con) 



########## scan
# column names
# (elapsed: 1054 sec)
dimnames <- list(NULL, col_list)

# tweak class_vec to fit scan()'s "what=' expression, and skip appropriate cols
	class_vec_scan <- c(
		rep(list(NULL),6),
		rep(0,4),
		list(NULL),
		0,
		list(NULL),
		0,
		rep(list(NULL),2),
		0,
		rep(list(NULL),3),
		0,
		rep(list(NULL),2),
		0,
		rep(list(NULL),7),
		0,
		rep(list(NULL),12),
		rep(0,2),
		rep(list(NULL),2),
		0,
		rep(list(NULL),5),
		0,
		rep(list(NULL),150)
		)

# create 100 manageable chunks (Read < 100,000 rows into memory at a time)
index <- 0
chunksize <- row_num_total/100
number_of_chunks_to_do <- 100

con <- file(description=targetfile,"r")

# run a looping code

system.time(
repeat {
        index <- index + 1
        print(paste('iteration #',index,sep=" "))

        # stop/break after 100 chunks 
        if (index > number_of_chunks_to_do){
                print('Processed all files!')
                break}
        
        else {
        	# if first chunk, append = F
        	if (index==1){

        		# read the 14 columns of interst into memory, 'chunksize' # of rows at a time
		         df_s <- scan(con, nlines=chunksize, skip=1, what=class_vec_scan, sep=",",quiet=TRUE)
		        
		        # convert from vector into data.table
		         df <- as.data.table(matrix(unlist(df_s), ncol=14, dimnames=dimnames))

		        # write to CSV only those rows that correspond to our random subset, keeping column names
		        write.csv(df[rownames(df) %in% subset],'~/Computing Project/csv/dataChunk_scan.csv', row.names=F)

		        # print to keep a record of activity
		        print("OK")}

		    # if 2nd chunk or later, header = F and append = T
			else {
				    # read the 14 columns of interst into memory, 'chunksize' # of rows at a time
		         df_s <- scan(con, nlines=chunksize, skip=0, what=class_vec_scan, sep=",",quiet=TRUE)
		        
		        # convert from vector into data.table
		         df <- as.data.table(matrix(unlist(df_s), ncol=14, dimnames=dimnames))

				    # append to the original CSV (chunk # 1) only those rows that correspond to our random subset
		         	# because rowname restarts every iteration, add chunksize*index-1
			        write.csv(df[(as.numeric(rownames(df)) + (chunksize*(index-1))) %in% subset],'~/Computing Project/csv/dataChunk_scan.csv', append=T, row.names=F)
		        
		        	# print to keep a record of activity
			        print("OK")}
		        }}
)

close(con)

# check for consistency
str(fread('~/Computing Project/csv/dataChunk_scan.csv'))

	######## alternative to scan repeat loop
		# chunksize <- row_num_total/100
		# con <- file(description=targetfile,"r")

		# # remove header
		# d=scan(con,what='a',nlines=1,sep=",") 

		# system.time(
		# for(i in seq(1,row_num_total,chunksize)){

		#     d=scan(con,what=class_vec_scan,nlines=1000,sep=",",quiet=TRUE)
		#     d = as.data.table(matrix(unlist(d), ncol=14, dimnames=dimnames))
		#     d = as.data.table(d)
		# }
		# )


########## readLines
# (elapsed: 2107 sec)

# create 100 manageable chunks (Read < 100,000 rows into memory at a time)
index <- 0
chunksize <- row_num_total/100
number_of_chunks_to_do <- 100
con <- file(description=targetfile,"r")

# run a looping code

system.time(
repeat {
        index <- index + 1
        print(paste('iteration #',index,sep=" "))

        # stop/break after 100 chunks 
        if (index > number_of_chunks_to_do){
                print('Processed all files!')
                break}
        
        else {
        	# if first chunk, append = F
        	if (index==1){

        		# read all data into memory, 'chunksize' # of rows at a time
		        df_rl <- readLines(con=con, n=chunksize)
		        
				# split readLines output
		        df_rl_s <- strsplit(df_rl,',')

		        # function to read only the columns of interest
		        # names them based on position of "i" in col_nums as it corresponds to position of names in col_list
		        # avoid parallelization in this first round to keep column names in place
 				col_nums<- c(7,8,9,10,12,14,17,21,24,32,45,46,49,55)

		        l_ply(col_nums,
		        	function(i){
			        	temp <- sapply(df_rl_s, "[[", i) 

			        	assign(col_list[match(i,col_nums)],temp,envir=.GlobalEnv)} 
		        	,.progress='text')

				# unlist readLines output and form a matrix with 205 columns
				df <- as.data.table(do.call(cbind, list(REGION, ST, ADJHSG, ADJINC, NP, ACR, BDSP, ELEP, GASP, RMSP, VEH, WATP, FINCP, HINCP)))

				#assign name from first row
				colnames(df) <- unlist(df[1, ])
				df = df[-1,]

		        # write to CSV only those rows that correspond to our random subset, keeping column names
		        write.csv(df[rownames(df) %in% subset],'/Computing Project/csv/dataChunk_rl.csv', row.names=F)

		        # print to keep a record of activity
		        print("OK")}


		    # if 2nd chunk or later, header = F and append = T
			else {
        		# read all data into memory, 'chunksize' # of rows at a time
		        df_rl <- readLines(con=con, n=chunksize)
		        
				# split readLines output
		        df_rl_s <- strsplit(df_rl,',')

		        # function to read only the columns of interest
		        # names them based on position of "i" in col_nums as it corresponds to position of names in col_list
 				col_nums<- c(7,8,9,10,12,14,17,21,24,32,45,46,49,55)

		        l_ply(col_nums,
		        	function(i){
			        	temp <- sapply(df_rl_s, "[[", i) 

			        	assign(col_list[match(i,col_nums)],temp,envir=.GlobalEnv)} 
		        	,.progress='text')

				# unlist readLines output and form a matrix with 205 columns
				df <- as.data.table(do.call(cbind, list(REGION, ST, ADJHSG, ADJINC, NP, ACR, BDSP, ELEP, GASP, RMSP, VEH, WATP, FINCP, HINCP)))

				    # append to the original CSV (chunk # 1) only those rows that correspond to our random subset
		         	# because rowname restarts every iteration, add chunksize*index-1
			        write.csv(df[(as.numeric(rownames(df)) + (chunksize*(index-1))) %in% subset],'/Computing Project/csv/dataChunk_rl.csv', append=T, row.names=F)
		        
		        	# print to keep a record of activity
			        print("OK")}
		        }}
)

close(con)

# check for consistency
str(fread('/Computing Project/csv/dataChunk_rl.csv'))





##################################################
##
## Try reading in final.dt.csv using 3 alternate approaches
##
##################################################

########
# read.csv (elapsed: 9 sec)
system.time(
	final.dt <- read.csv("/Computing Project/csv/dataChunk.csv")
	)

########
# data.table (elapsed: 0.6 sec)
system.time(
	final.dt <- fread("/Computing Project/csv/dataChunk.csv")
	)

########
# ff (elapsed: 9 sec)
system.time(
	final.dt <- read.csv.ffdf(file="/Computing Project/csv/dataChunk.csv",header=TRUE,colClasses=NA)
	)



##################################################
##
## Plotting FINCP*ADJINC ~ BDSP
##
##################################################

########
# Adjust income to 2013 levels
final.dt <- final.dt[,FINCP_adj := FINCP*(ADJINC/1000000)]

########
# take random sample to allow for reasonable timing of LOESS smoothing
# assign row index
final.dt <- final.dt[,index := seq.int(nrow(final.dt))]

# Randomly select 50 thou rows from index, seed @ 1000
samp_size <- 50000
set.seed(1000)
samp <- as.data.table(sample(nrow(final.dt), samp_size, replace = FALSE))
samp <- rename(samp,c('V1'='index'))

# exclude all non-selected rows via leftwise merge with vector of randomly sampled 
# index numbers
final.dt.new <- merge(samp,final.dt,all.x=TRUE, by='index')


########
# Loess plotting with 95% CI
ggplot() + 
	geom_point(data=final.dt.new,aes(x=BDSP, y=FINCP_adj)) +
		ggtitle('Family Income (2013 Dollars) by Household Size (# Bedrooms)\non 50,000 Random Samples with LOESS Smoothing, 99% CI Bounds') +
		xlab('# Bedrooms') +
		ylab('Family Income (2013 Dollars)') +
	geom_smooth(data=final.dt.new,stat = "smooth", method = "loess", formula = y ~ x, se = TRUE, fullrange = T, level = 0.99, na.rm=F, aes(x=BDSP,y=FINCP_adj)) 



##################################################
##
## Fit linear regression model FINC_adj ~ BDSP + VEH
##
##################################################

#######
# biglm model on 1 mil dataset
model <- biglm( FINCP_adj ~ BDSP + VEH , data = final.dt)
summary(model)
summary(model)$rsq
AIC(model)

#######
# biglm model on 50 thou dataset	
model.new <- biglm( FINCP_adj ~ BDSP + VEH , data = final.dt.new)
summary(model.new)
summary(model.new)$rsq











