# This code is intented to 1) explore the Heritage Health Prize data and 2) establish an algorithm to predict how many days a patient will spend in a hospital in the next year
#
#load packages
library(data.table)
library(plyr)
library(biglm)
library(randomForest)
library(MASS)
# library(Rcmdr)
library(caret)
library(doMC)
registerDoMC()

# Set working directory
setwd('~/Heritage Health Prize Data')

#read in dataset
dt <- fread('final.csv',stringsAsFactors=T)
target <- fread('Target.csv')
# dt_w <- fread('final_wide.csv',stringsAsFactors=T)
# dt_w_y2 <- fread('final_wide_Y2.csv',stringsAsFactors=T)
# dt_w_y3 <- fread('final_wide_Y3.csv',stringsAsFactors=T)
# dt_w_n <- fread('final_wide_nona.csv',stringsAsFactors=T)
dt_w_y2_n <- fread('final_wide_Y2_nona.csv',stringsAsFactors=T)
dt_w_y2_y3_n <- fread('final_wide_Y3_Y2_nona.csv',stringsAsFactors=T)
to_predict <- merge(target, dt_w_y2_y3_n, all.y=T, by = 'MemberID')
to_predict$ClaimsTruncated <- NULL
to_predict$DaysInHospital <- NULL
dt_w_y3_only_n <- fread('final_wide_y3_only_nona.csv',stringsAsFactors=F)
dt_w_y2_only_n <- fread('final_wide_Y2_only_nona.csv',stringsAsFactors=F) 

#total size in MB
print(object.size(x=lapply(ls(), get)), units="Mb")


##
## Missing Data ... imputation unsuccessful
##
# Assume data are missing completely at random (MCAR)

# determine fraction of each variable missing
# the largest percentages are msfs_sum 12%, lab_count_sum (21%), drug_count_sum (28%), male (24%), and age_firstclaim (8%). All other variables were missing less no data. No major difference by year.
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt,2,pMiss)
# apply(dt_w,2,pMiss)

# visualize the missing data
library(VIM)
aggr_plot <- aggr(dt[, ! 92:93,with=F], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#output
 # Missings in variables:
 #            Variable Count
 #            msfs_sum 13904
 #      drug_count_sum 33280
 #       lab_count_sum 25285
 #                male 28899
 #      age_firstclaim  9330

# Impute using "predictive mean matching"
# library(mice)
# dt_imputed <- mice(data = dt_w, m = 1, method = "pmm", maxit = 50, seed = 500)


# # try with parallel
# library(parallel)
# # Using all cores can slow down the computer
# # significantly, I therefore try to leave one
# # core alone in order to be able to do something 
# # else during the time the code runs
# cores_2_use <- detectCores() - 1

# cl <- makeCluster(cores_2_use)
# clusterSetRNGStream(cl, 9956)
# clusterExport(cl, "dt")
# clusterEvalQ(cl, library(mice))
# imp_pars <- 
#   parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
#     mice(dt, m = 1, printFlag = FALSE)
#   })
# stopCluster(cl)


##
##	Consideration for outliers of hospital count data
##
ggplot(dt_w_y2_n, aes(x=DaysInHospital_3)) + geom_histogram() + ggtitle(label="Histogram of the Outcome Variable") + xlab(labe="Days in Hospital (Year 3)")

# cap outcome variable at 5
dt_w_y2_n[DaysInHospital_3 > 5, DaysInHospital_3 := 5]

# replot
ggplot(dt_w_y2_n, aes(x=DaysInHospital_3)) + geom_histogram() + ggtitle(label="Histogram of the Outcome Variable - Capped at 5 Days") + xlab(labe="Days in Hospital (Year 3)")


##
## Random Forests for variable importance
##
# 

# set outcome and dependent variables for testing
dependents <- dt_w_y2_n[,3:(nrow(count(names(dt_w_y2_n)))-1),with=FALSE]
outcome <- dt_w_y2_n[,nrow(count(names(dt_w_y2_n))),with=FALSE]

# #random forests on a single core
# remove column 1 (V1) from dataset because it's a huge factor variable and useless
fit_test1 <- randomForest(DaysInHospital_3 ~ . , data=dt_w_y2_n[,!1,with=F], na.action=na.omit, type = 'regression',replace=T, ntree=1000,importance = T,importanceSD=T,interact=T,do.trace= T)

# # rf with parallel computing
# system.time(fit_test2 <- foreach(ntree=rep(250, 4), .combine=combine, .multicombine=TRUE, .verbose = T,
#               .packages='randomForest') %dopar% {
#     randomForest(DaysInHospital_3 ~ . , data=dt_w_y2_n[,!1,with=F], na.action=na.omit, type = 'regression',replace=T,importance = T,importanceSD=T,interact=T,do.trace= F, ntree=ntree)
# })

print(fit_test1)
str(fit_test1)


imp1 <- as.data.frame(varImp(fit_test1,scale=F))
imp1 <- setNames(cbind(rownames(imp1), imp1, row.names = NULL), c("Variable", "Change.in.MSE"))
imp1[with(imp1, order(-Change.in.MSE)), ]

write.csv(imp1[with(imp1, order(-Change.in.MSE)), ],'~/Desktop/rf1.csv')
varImpPlot(fit_test1,n.var=15,main='Variable Importance for top Variables')
plot(fit_test1,main='Test Error Observed by Number of Trees Generated - Fit 1')


# important variables (top 15)
# msfs_sum_2
# pay_delay_sum_2
# pay_delay_sum_1
# msfs_sum_1
# Office_2
# PL_2
# charlson_sum_2
# lab_count_sum_2
# EM_2
# Independent_Lab_2
# Internal_2
# METAB3_1
# lab_count_sum_1
# Diagnostic_Imaging_2


#
# Density/Distribution Plot to check for linearity
#
library(ggplot2)


#
# Linear Regression
#



glm <- glm( DaysInHospital_3 ~ msfs_sum_2 + pay_delay_sum_2 + pay_delay_sum_1 + msfs_sum_1 + Office_2 + PL_2 + charlson_sum_2 + lab_count_sum_2 + EM_2 + Independent_Lab_2 + Internal_2 + METAB3_1 + lab_count_sum_1 + Diagnostic_Imaging_2 , data = dt_w_y2_n, family="poisson")

plot(glm)

write.csv(coef(summary(glm)),'~/Desktop/glm_sm.csv')

response_pred <- predict(glm, to_predict, type="response")
response_pred.df <- data.frame(
	DaysInHospital_Y4= response_pred, 
	id = c(1:length(response_pred)))

ggplot(response_pred.df, aes(x=DaysInHospital_Y4)) + geom_histogram() + ggtitle(label="Histogram, Predicted Days in Hospital Year 4") + xlab(labe="Predicted Days in Hospital (Year 4)")

# glm <- glm(DaysInHospital_3 ~ . , data=dt_wide)
# AIC <- stepAIC(glm, trace = FALSE)



##
## Cross validation
##
library(boot)

cv <- cv.glm(dt_w_y2_n,glm,K=20)
sqrt(mean(cv$delta[2]^2))






###
### My question: pay delay
###


##
##	Consideration for outliers of hospital count data
##
ggplot(dt_w_y2_only_n, aes(x=pay_delay_sum_2)) + geom_histogram() + ggtitle(label="Histogram of the Outcome Variable") + xlab(labe="Pay Delay by Year")

##
## Random Forests for variable importance
##
# 

# rf with parallel computing
system.time(fit_test3 <- foreach(ntree=rep(250, 4), .combine=combine, .multicombine=TRUE, .verbose = T,
              .packages='randomForest') %dopar% {
    randomForest(pay_delay_sum_2 ~ . , data=dt_w_y2_only_n[,!1:2,with=F], na.action=na.omit, type = 'regression',replace=T,importance = T,importanceSD=T,interact=T,do.trace= F, ntree=ntree)
})

print(fit_test3)
str(fit_test3)

imp3 <- as.data.frame(varImp(fit_test3,scale=F))
imp3 <- setNames(cbind(rownames(imp3), imp3, row.names = NULL), c("Variable", "Change.in.MSE"))
imp3[with(imp3, order(-Change.in.MSE)), ]

write.csv(imp3[with(imp3, order(-Change.in.MSE)), ],'~/Desktop/rf1.csv')
varImpPlot(fit_test3,n.var=10,main='Variable Importance for top Variables')
# plot(fit_test3,main='Test Error Observed by Number of Trees Generated - Fit 1')


# important variables (top 10)
#  msfs_sum_2 +Office_2 +charlson_sum_2  +EM_2 + Urgent_Care_2 +PL_2 +Internal_2 + Independent_Lab_2 +Laboratory_2  

#
# Density/Distribution Plot to check for linearity
#
library(ggplot2)


#
# Linear Regression
#
glm2 <- glm( pay_delay_sum_2 ~ msfs_sum_2 +Office_2 +charlson_sum_2  +EM_2 + Urgent_Care_2 +PL_2 +Internal_2 + Independent_Lab_2 +Laboratory_2, data = dt_w_y2_only_n, family="poisson")

summary(glm2)
plot(glm2)

write.csv(coef(summary(glm2)),'~/Desktop/glm2_sm.csv')

# glm <- glm(DaysInHospital_3 ~ . , data=dt_wide)
# AIC <- stepAIC(glm, trace = FALSE)

#
# Cross validation
#
library(boot)

cv2 <- cv.glm(dt_w_y2_only_n,glm2,K=20)
sqrt(mean(cv2$delta[2]^2))

#
# Predict Year 3
#
names(dt_w_y3_only_n) <- sub("_3", "_2", names(dt_w_y3_only_n))
to_predict2 <- dt_w_y3_only_n

response_pred2 <- predict(glm2, to_predict2, type="response")
response_pred2.df <- data.frame(
	pay_delay_sum_2_p= response_pred, 
	id = c(1:length(response_pred)))

ggplot(response_pred.df, aes(x=pay_delay_sum_2_p)) + geom_histogram() + ggtitle(label="Histogram, Predicted Days in Hospital Year 4") + xlab(labe="Predicted Days in Hospital (Year 4)")

# combine and compare predicted to actual
comp <- as.data.table(cbind(response_pred2.df,dt_w_y3_only_n))
comp <- comp[, delta := pay_delay_sum_2_p - pay_delay_sum_2]
rmse = sqrt(mean(comp$delta^2))




