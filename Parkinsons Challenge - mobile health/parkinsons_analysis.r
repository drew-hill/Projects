library(data.table)
library(lubridate)
library(plyr)
library(ggplot2)
library(biglm)
# library(accelerometry)
# library(GGIR)
# library(XML)
# library(raster)
# library(OpenStreetMap)

#The goal is to use the provided data to distinguish PD patients from control subjects and/or to quantify PD symptoms in a way that could enable the measurement of disease progression

########
# readin master file (23 sec)
dt <- readRDS(file='~/Parkinsons Data/RDS files/dt.master.RDS')

######## variable maniuplation
# drop variables
# Keep PSD.1, Low-frequency motion energy, because this corresponds better to human movement apparently
dt.sm <- dt[,c("id","time","latitude","longitude","altitude","N.samples","pd","male","time_since_diag","altitude","score","x.PSD.1","y.PSD.1","z.PSD.1","x.mean","y.mean","z.mean"),with=FALSE]

# drop where accelerometry data are invalid
dt.sm <- dt.sm[!is.na(x.PSD.1)]




#########
### Means, huge differences, especially by z.mean, x.PSD.1, and y.PSD.1
dt[,mean(x.PSD.1,na.rm=T), by = pd]
dt[,mean(y.PSD.1,na.rm=T), by = pd]
dt[,mean(z.PSD.1,na.rm=T), by = pd]

t.test(dt.sm[pd==0,x.mean],dt.sm[pd==1,x.mean])
t.test(dt.sm[pd==0,y.mean],dt.sm[pd==1,y.mean])
t.test(dt.sm[pd==0,z.mean],dt.sm[pd==1,z.mean])

t.test(dt.sm[pd==0,x.PSD.1],dt.sm[pd==1,x.PSD.1])
t.test(dt.sm[pd==0,y.PSD.1],dt.sm[pd==1,y.PSD.1])
t.test(dt.sm[pd==0,z.PSD.1],dt.sm[pd==1,z.PSD.1])



### Max
dt[,max(x.PSD.1,na.rm=T), by = pd]
dt[,max(y.PSD.1,na.rm=T), by = pd]
dt[,max(z.PSD.1,na.rm=T), by = pd]

### Variation
dt[,sd(x.PSD.1,na.rm=T), by = pd]
dt[,sd(y.PSD.1,na.rm=T), by = pd]
dt[,sd(z.PSD.1,na.rm=T), by = pd]

###
# Exploratory regressions

# mean acceleromter values
lm1 <- bigglm(pd ~ sma,family=binomial(link="logit"),data=dt.sm)

lm2 <- biglm(pd ~ x.mean + y.mean + z.mean + male, data=dt)




#########
# Number of samples
dt.sm$sec15 <- floor(unclass((dt.sm$time)) / (15)) * (15)
no_id_sec15.1 <-dt.sm[,mean(N.samples),by=c("id","sec15")]
no_id_sec15.1[, feature_Nsamp := 0]
no_id_sec15.1[V1 > 15 & V1 < 25, feature_Nsamp := 1]

no_id_sec15.2 <- merge(no_id_sec15.1,dt.sm[,c("id","pd","sec15"),with=FALSE],by=c("id","sec15"))
no_id_sec15 <- no_id_sec15.2[!duplicated(no_id_sec15.2)]


ggplot(no_id_sec15) + geom_density(aes(x=V1,shape=as.factor(id),colour=as.factor(pd))) + ggtitle("Kernel Density of Sample Number per Second\n by Disease Status") + xlab('15 - Second Average Sample Number')

t.test(no_id_sec15[pd==0,feature_Nsamp],no_id_sec15[pd==1,feature_Nsamp])

## Big GLM with 15-SNA
glm_Nsamp <- bigglm(pd ~ feature_Nsamp, data=no_id_sec15, family=binomial(link="logit"), maxit= 100, chunksize=10000)
summary(glm_Nsamp)


#########
## daily Signal Magnitude Area (SMA) per Subject
##
# ## by day and month
# # month
# dt.sm[,month := month(time)]
# dt.sm[,jday := yday(time)]
# sma_id_day.1 <-dt.sm[,sum(abs(x.mean+y.mean+z.mean))/sum(abs(length(time))),by=c("id","jday")]
# # sma <- sma_id_day[,mean(V1),by=id]  

# sma_id_day.2 <- merge(sma_id_day.1,dt.sm[,c("id","pd","jday"),with=FALSE],by=c("id","jday"))
# sma_id_day <- sma_id_day.2[!duplicated(sma_id.day.2)]

# # t.test by sma... seems about the same
# t.test(sma_id_day[pd==0,V1],sma_id_day[pd==1,V1])

# ggplot(sma_id_day, aes(x=V1,color=as.factor(pd))) + geom_density(aes(group=pd)) + ggtitle('Kernel Density of Daily Avg. SMA\n by Disease Status') + xlab('Daily Avg. Signal Magnitude Area (SMA)')


## SMA per several minute period 
##

# 5 minute bins
dt.sm$min5 <- floor(unclass((dt.sm$time)) / (5*60)) *(5*60)
sma_id_min.5.1 <-dt.sm[,sum(abs(x.mean+y.mean+z.mean))/sum(abs(length(time))),by=c("id","min5")]
sma_id_min.5.2 <- merge(sma_id_min.5.1,dt.sm[,c("id","pd","min5"),with=FALSE],by=c("id","min5"))
sma_id_min.5 <- sma_id_min.5.2[!duplicated(sma_id_min.5.2)]

ggplot(sma_id_min.5, aes(x=V1,color=as.factor(pd))) + geom_density(aes(group=pd)) + ggtitle('Kernel Density of 5-min Avg. SMA\n by Disease Status') + xlab('5-minute Avg. Signal Magnitude Area (SMA)')

# t.test by sma... seems about the same
t.test(sma_id_min.5[pd==0,V1],sma_id_min.5[pd==1,V1])

## Big GLM with 5-SMA
glm_SMA5 <- bigglm(pd ~ V1, data=sma_id_min.5, family=binomial(link="logit"), maxit= 100, chunksize=10000)
summary(glm_SMA5)



# # 15 minute bins
# dt.sm$min15 <- floor(unclass((dt.sm$time)) / (15*60)) *(15*60)
# sma_id_min.15 <-dt.sm[,sum(abs(x.mean+y.mean+z.mean))/sum(abs(length(time))),by=c("id","min15")]
# sma_id_min.15.2 <- merge(sma_id_min.15,dt.sm[,c("id","pd","min15"),with=FALSE],by=c("id","min15"))

# ggplot(sma_id_min.15.2, aes(x=V1,color=as.factor(pd))) + geom_density(aes(group=pd)) + ggtitle('Kernel Density of 15-min Avg. SMA\n by Disease Status') + xlab('15-minute Avg. Signal Magnitude Area (SMA)')

# t.test(sma_id_min.15.2[pd==0,V1],sma_id_min.15.2[pd==1,V1])



# # 30 minute bins
# dt.sm$min30 <- floor(unclass((dt.sm$time)) / (30*60)) *(30*60)
# sma_id_min.30 <-dt.sm[,sum(abs(x.mean+y.mean+z.mean))/sum(abs(length(time))),by=c("id","min30")]
# sma_id_min.30.2 <- merge(sma_id_min.30,dt.sm[,c("id","pd","min30"),with=FALSE],by=c("id","min30"))

# ggplot(sma_id_min.30.2, aes(x=V1,color=as.factor(pd))) + geom_density(aes(group=pd)) + ggtitle('Kernel Density of 30-min Avg. SMA\n by Disease Status') + xlab('30-minute Avg. Signal Magnitude Area (SMA)')

# t.test(sma_id_min.30.2[pd==0,V1],sma_id_min.30.2[pd==1,V1])


########
# get familiar

### Plot
# patient without parkinsons
ggplot(data=dt.sm[id=='APPLE' & time > ymd_hms('2012-01-08 00:00:01') & time < ymd_hms('2012-01-08 23:59:59'),], aes(x=time)) + 
	geom_line(aes(y=sma, color='x')) +
	geom_line(aes(y=y.PSD.1,color='y')) +
	geom_line(aes(y=z.PSD.1,color='z')) +
	ggtitle('Control') + ylim(c(0,8000))

# patient with parkinsons
ggplot(data=dt.sm[id=='CHERRY' & time > ymd_hms('2012-01-24 00:00:01') & time < ymd_hms('2012-01-24 23:59:59'),], aes(x=time)) + 
	geom_line(aes(y=x.PSD.1, color='x')) +
	geom_line(aes(y=y.PSD.1,color='y')) +
	geom_line(aes(y=z.PSD.1,color='z')) +
	ggtitle('PD Patient') +
	ylim(c(0,8000))


### Breakout detection (40 seconds per 10000)
# Experiment on a subset of 10,000 rows
library(BreakoutDetection)

# system.time(bo.x <- breakout(dt.sm$x.mean[1:10000], min.size=10, method='multi', beta=.001, degree=1, plot=TRUE))
# system.time(bo.y <- breakout(dt.sm$y.mean[1:10000], min.size=10, method='multi', beta=.001, degree=1, plot=TRUE))
# system.time(bo.z <- breakout(dt.sm$z.mean[1:10000], min.size=10, method='multi', beta=.001, degree=1, plot=TRUE))

# system.time(bo.x.15 <- breakout(dt.sm$x.mean[1:10000], min.size=10, method='multi', beta=.002, degree=1, plot=TRUE))
# system.time(bo.y.15 <- breakout(dt.sm$y.mean[1:10000], min.size=10, method='multi', beta=.002, degree=1, plot=TRUE))
# system.time(bo.z.15 <- breakout(dt.sm$z.mean[1:10000], min.size=10, method='multi', beta=.002, degree=1, plot=TRUE))

# 1) Play with beta at 0.001, *0.002*
bo.x.15$plot
bo.x$plot

# 2) look for major differences in the number of breakouts by x,y,z
bo.x.15$plot
bo.y.15$plot
bo.z.15$plot

bo.x.15$loc
bo.y.15$loc
bo.z.15$loc


########## Fast fourier transform? 




########## detection theories
#### breakouts per minute?
## write my own function to flag mean shifts
perhaps a abs(diff(dt.sm$x.mean)) greater than some number, followed by an abs(diff(dt.sm$x.mean)) lower than some number for x period of time... make this the rule for switching state of the flag column perhaps

## change point is VERY fast for this 
library(changepoint)
# requires no missing values
# experiment with different methods
	# AMOC is too insensitive
	# PELT is too sensitive
	# BinSeg seems better, but slightly too insensitive
system.time(mvalue  <-  cpt.mean(dt.sm$x.mean,method="BinSeg"))

length(cpts(mvalue))/(length(dt.sm$time[1:100000])/60)


## Loop to do breakout detection in 100,000 row chunks

i = 
x = ids

bo.function <- function(x){
for i in 1:nrow(dt[id==x]){

bo.y.15 <- breakout(dt.sm$x.mean[((i-1)*100000)+1):(i*100000) & id==x], min.size=10, method='multi', beta=.002, degree=1, plot=TRUE)
}

}

# General Hidden Markov Models with Poisson distribution?
# witowski et al


# Hidden Markov Models with Gaussian distribution?




#########
######### 	Analysis
#########



#########
# Create new dataset with feature_Nsamp
dt.sm$min5 <- floor(unclass((dt.sm$time)) / (5*60)) *(5*60)
dt.sm$sec15 <- floor(unclass((dt.sm$time)) / (15)) * (15)

dt.sm[,Nsamp := mean(N.samples),by=c("id","sec15")]
dt.sm[, SMA5:= sum(abs(x.mean+y.mean+z.mean))/sum(abs(length(time))),by=c("id","min5")]
dt.sm[, feature_Nsamp := 0]
dt.sm[Nsamp > 15 & Nsamp < 25, feature_Nsamp := 1]

# drop extranneous variables
dt.sm[,c("min5","sec15") := NULL, with= FALSE]


#### t-test
t.test(dt.sm[pd==0,feature_Nsamp],dt.sm[pd==1,feature_Nsamp])


#### Big GLM
bg1 <- bigglm(pd ~ feature_Nsamp + SMA5 + male, data=dt.sm, family=binomial(link="logit"), maxit= 10, chunksize=10000)
summary(bg1)

bg2 <- bigglm(pd ~  SMA5 + male, data=dt.sm, family=binomial(link="logit"), maxit= 10, chunksize=10000)
summary(bg2)

bg3 <- bigglm(pd ~ feature_Nsamp + male, data=dt.sm, family=binomial(link="logit"), maxit= 10, chunksize=10000)
summary(bg3)


####
## H2O
library(h2o)
library(pROC)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE,max_mem_size = '10G', nthreads=8)

# set train (dog) and test sets
train <- dt.sm[!id %in% c("APPLE","CHERRY","CROCUS")]
pred <- dt.sm[id %in% c("APPLE","CHERRY","CROCUS")]

# prep for h20 by dropping NAs and "time"
train <- train[,c("time"):=NULL, with=FALSE]
pred <- pred[,c("time"):=NULL, with=FALSE]
# drop duplicate "altitude"
train <- train[,-4,with=F]
pred <- pred[,-4,with=F]


# remove original data tables to make space
rm(dt)
rm(dt.sm)

# read data in
dat_train_h2o <- as.h2o(train)
dat_pred_set_h2o <- as.h2o(pred)

#run model
system.time(model <- 
  h2o.deeplearning(x = c(17,18,6),  # column numbers for predictors
                   y = 5,   # column number for label
                   training_frame = dat_train_h2o, # data in H2O format
                   validation_frame = dat_pred_set_h2o,
                   distribution= 'AUTO',
                   activation = "TanhWithDropout", # or 'Tanh'
                   train_samples_per_iteration = -2 #autoselection of training samples per iteration
                   # input_dropout_ratio = 0.2, # % of inputs dropout
                   # hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   # hidden = c(50,50,50), # three layers of 50 nodes
                   , epochs = 50 # no. of epochs
                   , max_w2 = 10
                   # , l1=1e-5
)
)


# predict with H2O model
h2o_yhat_pred <- h2o.predict(model, dat_pred_set_h2o)
# convert to data.table -- long time
df_yhat_pred <- as.data.table(h2o_yhat_pred)

#AUC
new <- as.data.table(cbind(pred$pd,df_yhat_pred$predict))
set.seed(1000)
tenk <- sample(1:nrow(new),10000)
new_10k <- new[tenk]

#0.8242
mroc.h <- roc(new_10k$V1, new_10k$V2, plot=T)


# CI of sensitivity and specificity (through 2000 bootstrap samplings)
mroc.h.cis <- ci.thresholds(mroc.h, conf.level=0.95)







#########
# GIS analysis
# 
library(geosphere)
library(pROC)



### Apple
dt.geo <- na.omit(dt.sm[id=="APPLE",c("id","feature_Nsamp","SMA5","latitude","longitude","pd",'Nsamp'), with=F])
dt.geo <- dt.geo[c(1:20000)]


shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Shift vectors for lat and lon so that each row also contains the next position.
dt.geo$lat.p1 <- shift.vec(dt.geo$latitude, -1)
dt.geo$lon.p1 <- shift.vec(dt.geo$longitude, -1)

## Create distance matrix, use distHaversine for computational efficiency
system.time(mat <- distm(dt.geo[,c('lon.p1','lat.p1'),with=F],dt.geo[,c('longitude','latitude'),with=F], fun=distHaversine))

# calculate absolute value of marginal distance travelled each time interval
dt.geo[,dist:= rbind(as.data.table(c("NA")),as.data.table(abs(diff(mat[1,],lag=1))))] 




### DAFODIL
dt.geo1 <- na.omit(dt.sm[id=="DAFODIL",c("id","feature_Nsamp","SMA5","latitude","longitude","pd",'Nsamp'), with=F])
dt.geo1 <- dt.geo1[c(1:20000)]


shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Shift vectors for lat and lon so that each row also contains the next position.
dt.geo1$lat.p1 <- shift.vec(dt.geo1$latitude, -1)
dt.geo1$lon.p1 <- shift.vec(dt.geo1$longitude, -1)

## Create distance matrix, use distHaversine for computational efficiency
system.time(mat <- distm(dt.geo1[,c('lon.p1','lat.p1'),with=F],dt.geo1[,c('longitude','latitude'),with=F], fun=distHaversine))

# calculate absolute value of marginal distance travelled each time interval
dt.geo1[,dist:= rbind(as.data.table(c("NA")),as.data.table(abs(diff(mat[1,],lag=1))))] 



### DAISY
dt.geo2 <- na.omit(dt.sm[id=="DAISY",c("id","feature_Nsamp","SMA5","latitude","longitude","pd",'Nsamp'), with=F])
dt.geo2 <- dt.geo2[c(1:20000)]


shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Shift vectors for lat and lon so that each row also contains the next position.
dt.geo2$lat.p1 <- shift.vec(dt.geo2$latitude, -1)
dt.geo2$lon.p1 <- shift.vec(dt.geo2$longitude, -1)

## Create distance matrix, use distHaversine for computational efficiency
system.time(mat <- distm(dt.geo2[,c('lon.p1','lat.p1'),with=F],dt.geo2[,c('longitude','latitude'),with=F], fun=distHaversine))

# calculate absolute value of marginal distance travelled each time interval
dt.geo2[,dist:= rbind(as.data.table(c("NA")),as.data.table(abs(diff(mat[1,],lag=1))))] 



### FLOX
dt.geo3 <- na.omit(dt.sm[id=="FLOX",c("id","feature_Nsamp","SMA5","latitude","longitude","pd",'Nsamp'), with=F])
dt.geo3 <- dt.geo3[c(1:20000)]


shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# Shift vectors for lat and lon so that each row also contains the next position.
dt.geo3$lat.p1 <- shift.vec(dt.geo3$latitude, -1)
dt.geo3$lon.p1 <- shift.vec(dt.geo3$longitude, -1)

## Create distance matrix, use distHaversine for computational efficiency
system.time(mat <- distm(dt.geo3[,c('lon.p1','lat.p1'),with=F],dt.geo3[,c('longitude','latitude'),with=F], fun=distHaversine))

# calculate absolute value of marginal distance travelled each time interval
dt.geo3[,dist:= rbind(as.data.table(c("NA")),as.data.table(abs(diff(mat[1,],lag=1))))] 



### rbind
dt.geo.f <- rbind(dt.geo,dt.geo1)
dt.geo.f <- rbind(dt.geo.f,dt.geo2)
dt.geo.f <- rbind(dt.geo.f,dt.geo3)

# correct the class of distance
dt.geo.f[,dist := as.numeric(dist)]

### Analysis
geolm <- biglm(dist ~ Nsamp + SMA5 + pd, data = dt.geo.f) 
summary(geolm)

## cv
geolm.cv <- biglm(dist ~ Nsamp + SMA5 + pd, data = dt.geo.f[id!="APPLE",])
summary(geolm.cv)
  # predict for APPLE
  dt.geo.f[,pred:=as.numeric(NA)]
  dt.geo.f[id=="APPLE",pred:= 3.7774 + (1.0644*Nsamp) + (-7.4828*SMA5) + (18.7978*pd)]
  #  
  lm.cv <- lm(dt.geo.f[id=="APPLE",dist] ~ dt.geo.f[id=="APPLE",pred])
  ggplot(data=dt.geo.f[id=="APPLE"], aes(x=dist,y=pred)) + geom_point() + xlim(c(0,50)) + ylim(c(0,50)) + ylab("Predicted Distance") + xlab("Actual Distance")
