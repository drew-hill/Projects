# This code is intented to 1) explore the Heritage Health Prize data and 2) establish an algorithm to predict how many days a patient will spend in a hospital in the next year
#
#load packages
library(data.table)
library(plyr)
library(tidyr)


# Set working directory
setwd('~/Heritage Health Prize Data')

#
# List and import data files
#

#list

csv.files <- list.files(getwd(),pattern="*.csv")

#import

#some columns could not be read properly due to problem automagically reading class type, specifically if a "+" was present; to avoid the error that is thrown, I actively assign them as character class

#all.dt <- lapply(csv.files, fread)

Claims <- fread('Claims.csv',colClasses=list(character=c("PayDelay","LengthOfStay")))
DaysInHospital_Y2 <- fread('DaysInHospital_Y2.csv')
DaysInHospital_Y3 <- fread('DaysInHospital_Y3.csv')
DrugCount <- fread('DrugCount.csv')
LabCount <- fread('LabCount.csv',colClasses=list(character="LabCount"))
Members <- fread('Members.csv')
#Target <- fread('Target.csv')


# After some exploration, it seems that the most approrpiate variables by which to combine these data are MemberID and year. Note that "Target" and "Members" apply across all years and so are not year-specific. These will be handled later

#
# Ajdust variables
#

#Year

#Make year numerical where it already exists, making sure to remove the irrelevant character "Y"
Claims[, Year := as.numeric(gsub("Y","",Year))]
DrugCount[, Year := as.numeric(gsub("Y","",Year))]
LabCount[, Year := as.numeric(gsub("Y","",Year))]

#Add Year where necessary
DaysInHospital_Y2[, Year := as.integer(2)]
DaysInHospital_Y3[, Year := as.integer(3)]

#
# Check for and remove duplicated rows
#
#Step 1, Remove key / key to all columns
#Step 2, check for duplication

dt.list <- list(Claims,DaysInHospital_Y2,DaysInHospital_Y3,DrugCount,LabCount,Members)

dup.check <- function(dt){
	setkeyv(dt,names(dt))
	summary(duplicated(dt))
}

l_ply(dt.list,dup.check, .print=TRUE)

# The only file with duplicated rows is "Claims"
# Step 3, remove all duplicates
#
# setkeyv(Claims,names(dt))
# Claims <- Claims[!duplicated(Claims)]


# Remove Members without all 3 years worth of data
	# make variable for this
	# then drop

Claims[, year_1 := as.numeric(NA)]
Claims[MemberID %in% unique(Claims[Year ==1,MemberID]) , year_1 := 1]
Claims[, year_2 := as.numeric(NA)]
Claims[MemberID %in% unique(Claims[Year ==2,MemberID]) , year_2 := 1]
Claims[, year_3 := as.numeric(NA)]
Claims[MemberID %in% unique(Claims[Year ==3,MemberID]) , year_3 := 1]
Claims[, year_all := as.numeric(NA)]
Claims[MemberID %in% unique(Claims[year_1 ==1 & year_2 ==1 & year_3 ==1,MemberID]) , year_all := 1]

Claims <- Claims[year_all == 1]
Claims[,year_1 := NULL]
Claims[,year_2 := NULL]
Claims[,year_3 := NULL]
Claims[,year_all := NULL]


#
# Claims data table
#

#
# LengthOfStay

# Quantify "LengthOfStay" in days. Ranges receive the rounded up middle value (e.g. 1-2 weeks = 1.5 weeks = 11), and 26+ weeks is assigned 182 days

count(Claims$LengthOfStay)

Claims[,stay_days := as.numeric(0)]
Claims[LengthOfStay == "1 day",stay_days := as.numeric(1)]
Claims[LengthOfStay == "2 days",stay_days := as.numeric(2)]
Claims[LengthOfStay == "3 days",stay_days := as.numeric(3)]
Claims[LengthOfStay == "4 days",stay_days := as.numeric(4)]
Claims[LengthOfStay == "5 days",stay_days := as.numeric(5)]
Claims[LengthOfStay == "6 days",stay_days := as.numeric(6)]
Claims[LengthOfStay == "1- 2 weeks",stay_days := as.numeric(11)]
Claims[LengthOfStay == "2- 4 weeks",stay_days := as.numeric(21)]
Claims[LengthOfStay == "4- 8 weeks",stay_days := as.numeric(42)]
Claims[LengthOfStay == "26+ weeks",stay_days := as.numeric(182)]

count(Claims$stay_days)

#remove original variable

Claims$LengthOfStay <- NULL


#
# Charlson

# Quantify "CharlsonIndex" in days. Ranges receive the rounded up middle value (e.g. 1-2 weeks = 1.5 weeks = 11), and 26+ weeks is assigned 182 days

Claims[,charlson := as.numeric(NA)]
Claims[CharlsonIndex == "0",charlson := as.numeric(0)]
Claims[CharlsonIndex == "1-2",charlson := as.numeric(1.5)]
Claims[CharlsonIndex == "3-4",charlson := as.numeric(3.5)]
Claims[CharlsonIndex == "5+",charlson := as.numeric(5.5)]

count(Claims$charlson)

#remove original variable

Claims[, CharlsonIndex := NULL]


#
# DSFS (Days Since First Service)
count(Claims$DSFS)

#remove all values up to and includeing hyphen

Claims[, msfs := gsub(".*-","",DSFS)]

#remove all characters after and including the "m" in month(s) and make it numeric

Claims[, msfs := as.numeric(gsub("m.*","",msfs))]

#remove original vriable

Claims$DSFS <- NULL


# PayDelay
#

Claims[PayDelay == "162+", PayDelay := "162"]
Claims[, PayDelay := as.numeric(PayDelay)]


#
# Aggregate Claims Data by MemberID and Year of Visit
#

#
# Make columns for count of each categorical variable

# unique_spec <- unique(list(unique(Claims$Specialty)))
# unique_vendor <- unique(list(unique(Claims$Vendor)))
# unique_pcp <- unique(list(unique(Claims$PCP)))
# unique_provID <- unique(list(unique(Claims$ProviderID)))
# unique_plsvc <- unique(list(unique(Claims$PlaceSvc)))
# unique_primcondgrp <- unique(list(unique(Claims$PrimaryConditionGroup)))
# unique_procgrp <- unique(list(unique(Claims$ProcedureGroup)))

new.spec <- dcast.data.table(Claims, MemberID + Year ~ Specialty, value.var = "Specialty", length)

# new.ven <- dcast.data.table(Claims, MemberID + Year ~ Vendor)
#drop vendor instead
Claims[,Vendor := NULL]

#new.pcp <- dcast.data.table(Claims, MemberID + Year ~ PCP,length)
#drop vendor instead
Claims[,PCP := NULL]

# new.pid <- dcast.data.table(Claims, MemberID + Year ~ ProviderID)
#drop vendor instead
Claims[,ProviderID := NULL]

new.plsvc <- dcast.data.table(Claims, MemberID + Year ~ PlaceSvc, value.var = "PlaceSvc",length)

new.pcg <- dcast.data.table(Claims, MemberID + Year ~ PrimaryConditionGroup,value.var="PrimaryConditionGroup",length)


new.pg <- dcast.data.table(Claims, MemberID + Year ~ ProcedureGroup, value.var= "ProcedureGroup",length)

new1 <- merge(new.spec, new.plsvc , all=T, by=c("MemberID","Year"), allow.cartesian=TRUE)
new2 <- merge(new1, new.pcg , all=T, by=c("MemberID","Year"), allow.cartesian=TRUE)
new3 <- merge(new2, new.pg , all=T, by=c("MemberID","Year"), allow.cartesian=TRUE)
# new4 <- merge(new3, new.pcp , all=T, by=c("MemberID","Year"), allow.cartesian=TRUE)

# Clean up
new3[,V1.y := NULL]
new3[,V1.y := NULL]
new3[,V1.x := NULL]
new3[,V1.x := NULL]
new3 <- rename(new3,c("Other.x" = "Other_specialty"))
new3 <- rename(new3,c("Other.y" = "Other_placesvc"))
Claims[,Specialty := NULL]
Claims[,PlaceSvc := NULL]
Claims[,PrimaryConditionGroup := NULL]
Claims[,ProcedureGroup := NULL]
Claims[,SupLOS := NULL]


# Aggregate numerical claims by MemberID and Year
# ?? consider adding "na.rm"

Claims[,pay_delay_sum := sum(PayDelay), by= c("MemberID","Year")]
Claims[,stay_day_sum := sum(stay_days), by= c("MemberID","Year")]
Claims[,charlson_sum := sum(charlson), by= c("MemberID","Year")]
Claims[,msfs_sum := sum(msfs), by= c("MemberID","Year")]

#Clean up
Claims[,PayDelay := NULL]
Claims[,stay_days := NULL]
Claims[,charlson := NULL]
Claims[,msfs := NULL]

#drop duplicates
claims_interim <- Claims[!duplicated(Claims)]


# 
# Full join!

Claims <- merge(claims_interim, new3 , all=T, by=c("MemberID","Year"), allow.cartesian=TRUE)


#
# Drug Count and Lab Count Data datatables
#

# Convert DSFS to MSFS
# DrugCount[, msfs_drug := gsub(".*-","",DSFS)]
# DrugCount[, msfs_drug := as.numeric(gsub("m.*","",msfs_drug))]
DrugCount$DSFS <- NULL
# LabCount[, msfs_lab := gsub(".*-","",DSFS)]
# LabCount[, msfs_lab := as.numeric(gsub("m.*","",msfs_lab))]
LabCount$DSFS <- NULL

# make LabCount numeric
LabCount[LabCount=="10+", LabCount := "10"]
LabCount[, LabCount := as.numeric(LabCount)]

# make DrugCount numeric
DrugCount[DrugCount=="7+", DrugCount := "7"]
DrugCount[, DrugCount := as.numeric(DrugCount)]

# # Aggregate msfs by MemberID and year
# #
# DrugCount[,msfs_drug_sum := sum(msfs_drug), by= c("MemberID","Year")]
# DrugCount[,msfs_drug_max := max(msfs_drug), by= c("MemberID","Year")]
# DrugCount[,msfs_drug_min := min(msfs_drug), by= c("MemberID","Year")]
# DrugCount[,msfs_drug := NULL]

# LabCount[,msfs_lab_sum := sum(msfs_lab), by= c("MemberID","Year")]
# LabCount[,msfs_lab_max := max(msfs_lab), by= c("MemberID","Year")]
# LabCount[,msfs_lab_min := min(msfs_lab), by= c("MemberID","Year")]
# LabCount[,msfs_lab := NULL]

# Aggregate counts data to be by Member ID and Year Only
#
DrugCount[,drug_count_sum := sum(DrugCount), by= c("MemberID","Year")]
DrugCount[,DrugCount := NULL]

LabCount[,lab_count_sum := sum(LabCount), by= c("MemberID","Year")]
LabCount[,LabCount := NULL]

#
# Drop duplicate rows
LabCount <- LabCount[!duplicated(LabCount)]
DrugCount <- DrugCount[!duplicated(DrugCount)]


# Combine by MemberID, Year and DSFS (rows with same value must be the same visit) 
setkeyv(DrugCount, c("MemberID","Year"))
setkeyv(LabCount, c("MemberID","Year"))
counts <- merge(DrugCount, LabCount, all = T, by=c("MemberID","Year"), allow.cartesian=TRUE)

#unload from memory
rm(list='DrugCount','LabCount')


#
# Outcome datatables
#

# # Make headings year-specific
# setnames(DaysInHospital_Y2,"ClaimsTruncated","ClaimsTruncated_Y2")
# setnames(DaysInHospital_Y2,"DaysInHospital","DaysInHospital_Y2")
# #
# setnames(DaysInHospital_Y3,"ClaimsTruncated","ClaimsTruncated_Y3")
# setnames(DaysInHospital_Y3,"DaysInHospital","DaysInHospital_Y3")

#
# Combine by MemberID

# be sure that MemberID is an appropraite merge key
# unique(duplicated(DaysInHospital_Y2$MemberID))
# unique(duplicated(DaysInHospital_Y3$MemberID))

# merge
# setkeyv(DaysInHospital_Y2, c("MemberID"))
# setkeyv(DaysInHospital_Y3, c("MemberID"))
# outcomes <- merge(DaysInHospital_Y2, DaysInHospital_Y3, all = T, by="MemberID", allow.cartesian=TRUE)

#
# rbind
outcomes <- rbindlist(list(DaysInHospital_Y2,DaysInHospital_Y3))
outcomes <- rename(outcomes, c("ClaimsTruncated" = "ClaimsTruncated_DIH"))

#unload original datasets from memory
rm(list='DaysInHospital_Y2','DaysInHospital_Y3')


#
# Members Datatable
#

# Make binary sex category

Members[, male := as.numeric(NA)]
Members[Sex=="M", male := as.numeric(1)]
Members[Sex=="F", male := as.numeric(0)]
Members$Sex <- NULL

# Make age at first claim numeric, use middle value of each range
Members[, age_firstclaim := as.numeric(paste(substr(Members$AgeAtFirstClaim, start=0, stop=1),5,sep=""))]
Members[AgeAtFirstClaim == "", age_firstclaim := NA]

# drop variable
Members$AgeAtFirstClaim <- NULL


#
# 'Left Outer Join' by Year, MemberId
#
dt.list.yr.m <- list(Claims,counts, outcomes)

#Step 1, Set keys to Year and MemberID

l_ply(dt.list.yr.m, function(i) setkeyv(i,c('MemberID','Year')))

#Step 2, merge with 'all=TRUE' to ensure no lost data

dt.interim <- Reduce(function(...) merge(..., all.x = T, allow.cartesian=TRUE)  , dt.list.yr.m)

# Step 3, relieve memory

rm(list=c('Claims', 'counts'))

#
# 'Left Outer Join' with remaining datatables MemberID
#

dt.list.remaining <- list(dt.interim,Members) 
l_ply(dt.list.remaining, function(i) setkeyv(i,'MemberID'))
final <- Reduce(function(...) merge(..., all.x = T, allow.cartesian=TRUE)  , dt.list.remaining)
rm(list=c('dt.interim','Members'))



##
## Clean up the final dataset
##

names(final) <- sub(" ", "_", names(final))
names(final) <- sub(" ", "_", names(final))

final_wide <- reshape(final, direction  = "wide", idvar="MemberID", timevar="Year",sep="_")
final_wide$DaysInHospital_1  <-  NULL
final_wide$ClaimsTruncated_DIH_1  <-  NULL
final_wide$V1  <-  NULL
final_wide$V1_1  <-  NULL
final_wide$V1_2  <-  NULL
final_wide$V1_3  <-  NULL

#figure out which columns are in Year 3 (i.e. to exclude... except DaysInHospital), and exclude MemberID
grep("_3",names(final_wide))
final_wide_Y2 <- final_wide[, c(2:183,273), with=FALSE]
final_wide_Y3_Y2 <- final_wide[, c(1,92:275), with=FALSE]
final_wide_Y3_only <- final_wide[, c(1,184:275), with=FALSE]
final_wide_Y2_only <- final_wide[, c(1,92:183), with=FALSE]

# drop missing rows
final_wide_nona <- na.omit(final_wide)
final_wide_Y2_nona <- na.omit(final_wide_Y2)
final_wide_Y3_Y2_nona <- na.omit(final_wide_Y3_Y2)
final_wide_Y3_only_nona <- na.omit(final_wide_Y3_only)
final_wide_Y2_only_nona <- na.omit(final_wide_Y2_only)


names(final_wide_Y3_Y2_nona) <- sub("_2", "_1", names(final_wide_Y3_Y2_nona))
names(final_wide_Y3_Y2_nona) <- sub("_3", "_2", names(final_wide_Y3_Y2_nona))


# write final file
write.csv(final,'final.csv')
write.csv(final_wide,'final_wide.csv')
write.csv(final_wide_Y2,'final_wide_Y2.csv')
write.csv(final_wide_Y3,'final_wide_Y3.csv')
write.csv(final_wide_Y3_only,'final_wide_Y3_Y2.csv')
write.csv(final_wide_nona,'final_wide_nona.csv')
write.csv(final_wide_Y2_nona,'final_wide_Y2_nona.csv')
write.csv(final_wide_Y3_Y2_nona,'final_wide_Y3_Y2_nona.csv')
write.csv(final_wide_Y3_only_nona,'final_wide_Y3_only_nona.csv')
write.csv(final_wide_Y2_only_nona,'final_wide_Y2_only_nona.csv')

# size of objects in memory
#by object in bytes
sort( sapply(ls(),function(x){object.size(get(x))}))
#total in MB
print(object.size(x=lapply(ls(), get)), units="Mb")


