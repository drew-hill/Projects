# library(R.matlab)
library(plyr)
library(data.table)
# library(ggplot2)

# # set number of parallel cores to total - 2 (6 on drew's macbook)
# library(parallel)
# library(doMC)
# registerDoMC(detectCores()-2)
# # getDoParWorkers()

# read in people
system.time(peep <- readRDS('~/Documents/UC Berkeley/Coursework/PH290-overflow/Projects - overflow/Data/Seizure Data/seizure_patient.rds'))
peep$type <- NULL
peep$index <- NULL
peep$latency <- NULL

# adjust node_loc from character to integer
# count(peep$node_loc)
peep[node_loc=='ATD', node_loc:='1']
peep[node_loc=='ITS', node_loc:='2']
peep[node_loc=='LAF', node_loc:='3']
peep[node_loc=='LAG', node_loc:='4']
peep[node_loc=='LD', node_loc:='5']
peep[node_loc=='LFG', node_loc:='6']
peep[node_loc=='LFS', node_loc:='7']
peep[node_loc=='LG', node_loc:='8']
peep[node_loc=='LMacro', node_loc:='9']
peep[node_loc=='LPG', node_loc:='10']
peep[node_loc=='LTG', node_loc:='11']
peep[node_loc=='PTD', node_loc:='12']
peep[node_loc=='RD', node_loc:='13']
peep[node_loc=='RIS', node_loc:='14']
peep[node_loc=='RMacro', node_loc:='15']
peep[node_loc=='RPD', node_loc:='16']
peep[node_loc=='RSS', node_loc:='17']
peep[node_loc=='RTG', node_loc:='18']
peep[node_loc=='STS', node_loc:='19']
#make integer
peep[,node_loc := as.integer(node_loc)]



# read-in dogs
dogs <- readRDS('~/Documents/UC Berkeley/Coursework/PH290-overflow/Projects - overflow/Data/Seizure Data/seizure_dog.rds')

dogs <- dogs[state!='test']
dogs[state=='ictal',state := '1']
dogs[state=='interictal',state := '0']
dogs <- dogs[order(rank(as.numeric(segment_no)))]
dogs[,state := as.numeric(state)]
dogs$index <- NULL


##
## Feature selection

# mean across all channels



# ##
# ## Deep Learning via h2o
# library(h2o)
# library(h2oEnsemble)
# library(SuperLearner)
# library(cvAUC)

# # split into training and test sets
# train_idx <- sample(1:nrow(dogs),(nrow(dogs)/10*9))
# 	# training... 10.5% of samples were seizure state
# train <- dogs[train_idx,]
# 	# test... 10.4% of samples were seizure state
# test <- dogs[-train_idx,]

# # begin local H2O instance
# localH2O = h2o.init()

# # transform dataset to H2O data objects 
# training_set <- as.h2o(train)
# validation_set  <- as.h2o(test)
# y <- "data"
# x <- names(training_set)[-1]
# family <- 'binomial'

# training_set[,c(y)] <- as.factor(training_set[,c(y)]) #Force Binary classification
# validation_set[,c(y)] <- as.factor(validation_set[,c(y)]) # check to validate that this guarantee

# #define machine learning methods to include in ensemble
# learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
# "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
# # set meta learner
# metalearner <- "SL.glm"

# # define the parameters and build our model.extract
# fit <- h2o.ensemble(x = x, y= y, data = training_set,
# 	family = family,
# 	learner = learner,
# 	metalearner = metalearner,
# 	cvControl = list(V = 5, shuffle = TRUE))


# ## manual selection of features
# for(V in 1:10){
# 	valid <- valid
# 	train <- train

# 	system.time(EstA <- glm(state~ data + data*segment_no, family='binomial', data=train))
# 	EstB <- glm(state~ sindatasq + sindatasq*segment_no, family='binomial', data=train)
# 	EstC <- glm(state~ data + data*segment_no + channels, family='binomial', data=train)
# 	EstD <- glm(state~ sindatasq + sindatasq*segment_no + channels, family='binomial', data=train)

# 	PredA <- predict(EstA, newdata = valid, type = 'response')
# 	PredB <- predict(EstB, newdata = valid, type = 'response')
# 	PredC <- predict(EstC, newdata = valid, type = 'response')
# 	PredD <- predict(EstD, newdata = valid, type = 'response')

# 	CV.risk[V,] <- c(mean((valid$data - PredA)^2), mean((valid$data - PredB)^2),
# + mean((valid$data - PredC)^2), mean((valid$data - PredD)^2))

# }



##
## Deep learning in dogs via CV SuperLearner using L2 loss function (Least Squares Error)
library(SuperLearner)
library(parallel)
n <- nrow(dogs)
sl <- dogs

# split into training and test sets
sl$type <- NULL
sl$latency <- NULL


train_idx <- sample(1:n,(n/10*9))
	# training... 10.5% of samples were seizure state
train <- sl[train_idx,]
	# test... 10.4% of samples were seizure state
valid <- sl[-train_idx,]

# set library
# SL.library <- c("SL.glm")
SL.library <- c("SL.nnet","SL.randomForest")

#create dataframe X with the predictor variable
X <- subset(valid, select= -state)
X$freq <- NULL

#remove factors
X[,channels:= as.numeric(gsub("^.*?_Ecog_c",'',channels))]
X[,id:=as.numeric(gsub("Dog","",id))]
X[,segment_no := as.numeric(segment_no)]

#run SuperLearner - 3193.167 seconds
system.time(
	CV.out <- CV.SuperLearner(Y=valid$state, X=X, V=10, SL.library=SL.library, family='binomial', cvControl=list(V=10), parallel = 'multicore', method='method.AUC')
	)

summary(CV.out)
coef(CV.out)
summary.CV.SuperLearner(CV.out)
summary(CV.out$SL.predict)

# ROC
library(pROC)
mroc1 <- roc(valid$state, CV.out$SL.predict, plot=T)


## Prediction in dogs using regular SUperLearner
	# produce prediction superlearner object - 600 seconds
system.time(
	SL.out <- SuperLearner(Y=valid$state, X=X, SL.library=SL.library, family='binomial', cvControl=list(V=10), method='method.AUC')
	)

# create new validation set
new_valid <- train
new_valid[, channels:= as.numeric(gsub("^.*?_Ecog_c",'',channels))]
new_valid[,id:=as.numeric(gsub("Dog","",id))]
new_valid[,segment_no := as.numeric(segment_no)]
new_valid$freq <- NULL
new_valid_s <- subset(new_valid, select= -state)

#predict - 61 seconds 1.2million
system.time(pred.out <- predict(SL.out, new_valid_s))

# ROC - AUC = 0.9967
library(pROC)
mroc <- roc(train$state, as.vector(pred.out$pred), plot=T)


##
##
## Apply Dogs prediction to humans via super learner

##reshape human data
# exclude to 400 frequency to make applicable to dog data
new_peep <- peep[freq < 1000,]
new_peep$freq <- NULL
#drop node_loc
new_peep$node_loc <- NULL
# remove state and id
new_peep_s <- subset(new_peep, select = -state)
new_peep_s <- subset(new_peep_s, select = -id)


## rerun dog SV, excluding the variable for dog ID
X2 <- subset(X, select = -id)
new_valid_s2 <- subset(new_valid_s, select = -id)

system.time(
	SL.out.d2 <- SuperLearner(Y=valid$state, X=X2, SL.library=SL.library, family='binomial', cvControl=list(V=10), method='method.AUC')
	)
system.time(pred.out.d2 <- predict(SL.out.d2, new_valid_s2))
mroc.d2 <- roc(train$state, as.vector(pred.out$pred), plot=T)


## apply new dog SL to humans - 46 seconds
system.time(pred.out.dh <- predict(SL.out.d2, new_peep_s))
mroc.dh <- roc(new_peep$state, as.vector(pred.out.dh$pred), plot=T)



##
##
## Apply Dogs prediction to humans via H2O

## adjust doggle data
n <- nrow(dogs)
h_dog <- dogs

# split into training and test sets
h_dog$type <- NULL
h_dog$latency <- NULL

#create dataframe X with the predictor variable
h_dog$freq <- NULL

#remove factors and ID
h_dog[,channels:= as.numeric(gsub("^.*?_Ecog_c",'',channels))]
h_dog[,id:=as.numeric(gsub("Dog","",id))]
h_dog[,segment_no := as.numeric(segment_no)]
h_dog$id <- NULL
# drop segment number and channel number... probably not useful
h_dog$segment_no <- NULL
h_dog$channels <- NULL


##
##reshape human data
# exclude to 400 frequency to make applicable to dog data
new_peep <- peep[freq < 1000,]
new_peep$freq <- NULL
#drop node_loc
new_peep$node_loc <- NULL
# remove id
new_peep_s <- subset(new_peep, select = -id)
# drop segment number and channel number... probably not useful
new_peep_s$segment_no <- NULL
new_peep_s$channels <- NULL


##
## H2O
library(h2o)
library(pROC)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,max_mem_size = '10G', nthreads=8)

# set train (dog) and test sets
train <- h_dog
pred <- new_peep_s

train_idx_dog_hum <- sample(1:nrow(h_dog),(nrow(h_dog)/2))
train <- h_dog[train_idx_dog_hum,]
test <- h_dog[-train_idx_dog_hum,]

# read data in
dat_train_h2o <- as.h2o(train)
dat_test_h2o <- as.h2o(test)
dat_pred_set_h2o <- as.h2o(pred)

#run model
system.time(model <- 
  h2o.deeplearning(x = 1,  # column numbers for predictors
                   y = 2,   # column number for label
                   training_frame = dat_train_h2o, # data in H2O format
                   validation_frame = dat_test_h2o,
                   distribution= 'AUTO',
                   activation = "TanhWithDropout", # or 'Tanh'
                   train_samples_per_iteration = -2 #autoselection of training samples per iteration
                   # input_dropout_ratio = 0.2, # % of inputs dropout
                   # hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   # hidden = c(50,50,50), # three layers of 50 nodes
                   , epochs = 8 # no. of epochs
                   , max_w2 = 10
                   # , l1=1e-5
)
)

# predict with H2O model
system.time(h2o_yhat_pred <- h2o.predict(model, dat_pred_set_h2o))
# convert to data.table -- long time
system.time(df_yhat_pred <- as.data.table(h2o_yhat_pred))

#AUC
new <- as.data.table(cbind(pred$state,df_yhat_pred$predict))
set.seed(1000)
tenk <- sample(1:nrow(new),10000)
new_10k <- new[tenk]

system.time(mroc.h <- roc(new_10k$V1, new_10k$V2, plot=T))

# CI of sensitivity and specificity (through 2000 bootstrap samplings)
mroc.h.cis <- ci.thresholds(mroc.h, conf.level=0.95)



##
##
## Completely new prediction model for Humans
library(h2o)
library(pROC)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,max_mem_size = '10G', nthreads=8)
# h2o.shutdown(prompt=TRUE)

#create training and validation sets
train_idx_h <- sample(1:nrow(peep),(nrow(peep)/2*1))
train <- peep[train_idx_h,]
test <- peep[-train_idx_h,]

# read data in
dat_train_h2o <- as.h2o(train)
dat_test_h2o <- as.h2o(test)

#run model - 52.6 seconds
system.time(model <- 
  h2o.deeplearning(x = c(1:5,7),  # column numbers for predictors
                   y = 6,   # column number for label
                   training_frame = dat_train_h2o, # data in H2O format
                   validation_frame = dat_test_h2o,
                   distribution= 'AUTO',
                   activation = "TanhWithDropout", # or 'Tanh'
                   train_samples_per_iteration = -2 #autoselection of training samples per iteration
                   # input_dropout_ratio = 0.2, # % of inputs dropout
                   # hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   # hidden = c(50,50,50), # three layers of 50 nodes
                   , epochs = 4 # no. of epochs
)
)

# predict with H2O model - 130 seconds
system.time(h2o_yhat_test <- h2o.predict(model, dat_test_h2o))
# convert to data.table -- long time
system.time(df_yhat_test <- as.data.table(h2o_yhat_test))

#AUC - lnog time
new <- as.data.table(cbind(test$state,df_yhat_test$predict))
tenk <- sample(1:nrow(new),(nrow(peep)/(2853.55*2)))
new_10k <- new[tenk]

system.time(mroc.h <- roc(new_10k$V1, new_10k$V2, plot=T))


saveRDS(model,'~/Desktop/model.rds')
saveRDS(h2o_yhat_test,'~/Desktop/h2o_yhat_test.rds')
saveRDS(df_yhat_test,'~/Desktop/df_yhat_test.rds')
saveRDS(mroc.h,'~/Desktop/mroc.h.rds')



# dogs$seg_index <- as.numeric(paste(dogs$segment_no,dogs$index,sep=""))

# plot
ggplot(dogs[id=='Dog1' & state==1 & segment_no=='166']) + geom_line(aes(x=index,y=data)) + facet_wrap(~channels)


# dictionary file
	#data: a matrix of EEG sample values arranged row x column as electrode x time.
	
	#data_length_sec: the time duration of each data row (1 second for all data in this case)
	
	#latency: the time in seconds between the expert-marked seizure onset and the first data point in the data segment (in ictal training segments only) 
	
	#sampling_frequency: the number of data samples representing 1 second of EEG data. (Non-integer values represent an average across the full data record and may reflect missing EEG samples)
	
	#channels: a list of electrode names corresponding to the rows in the data field


# predict the probability that a given clip is a seizure (AUCseizure)