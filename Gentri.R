getwd()
dir()
CS90 <- read.csv("CS90_YG.csv")
CS00 <- read.csv("CS00_YG.csv")
ACS10 <- read.csv("ACS10_YG.csv")

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#================= Merging Files==============================================================

#Merge many files.Save your files to merge in one folder 
# multmerge = function(mypath){
#   filenames=list.files(path=mypath, full.names=TRUE)
#   datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
#   Reduce(function(x,y) {merge(x,y)}, datalist)
# 
# mymergeddata = multmerge("C://R//mergeme")

# First join 90s to 2000 all inclusive
data90_00 <- merge(CS90,CS00, by="Geo_FIPS", all=TRUE)
# First join 90s to 2000 just machting  FIPS
#data <- merge(CS00,CS90, by=3)
# Second join 2000 to 2010 all inclusive
data00_10 <- merge(data90_00,ACS10, by="Geo_FIPS", all=TRUE)
# Join 1990 to 2010 all inclusive
data90_10 <- merge(CS90,ACS10, by="Geo_FIPS", all=TRUE)
# Secon join 2000 to 2010 just machting  FIPS
#data <- merge(data,ACS10, by="Geo_FIPS") 
# Second join 2000 to 2010 just matching from 90-00 to 2010
#datafull3 <- merge(data,ACS10, by="Geo_FIPS", all=TRUE)
# Second join 2000 to 2010 just matching from 90-00 to 2010
#data3 <- merge(datafull,ACS10, by="Geo_FIPS") 
# Join just matching FIPS from 90-2010 and then to 2010
#dat <- merge(ACS10,CS90, by="Geo_FIPS")
#dat2 <- merge(dat,CS00, by="Geo_FIPS")

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#================= Comparing percentiles 1990 to 2000 ==============================================================
#After merging 90s to 2000 all inclusive
#Compare percentiles Median Household Income
data90_00$Inc90_00<- ifelse(data90_00$percentil90Income < data90_00$percentil00Income,data90_00$percentil00Income, NA)
#Compare percentiles Median Home Value
data90_00$HValue90_00<- ifelse(data90_00$percentil90HValue < data90_00$percentil00HValue,data90_00$percentil00HValue, NA)
#Compare percentiles Education Attaiment
data90_00$EAtt90_00<- ifelse(data90_00$percentil90EduAtt < data90_00$percentil00EduAtt, data90_00$percentil00EduAtt, NA)
#Compare percentiles Age 25 to 34
data90_00$Age2590_00<- ifelse(data90_00$percentil90Age25_34 < data90_00$percentil00Age25_34, data90_00$percentil00Age25_34, NA)


#================= Comparing percentiles 2000 to 2010 ==============================================================
#After merging 90s to 2000 all inclusive
#Compare percentiles Median Household Income

data00_10$Inc00_10<- ifelse(data00_10$percentil00Income < data00_10$percentil10Income,data00_10$percentil10Income, NA)
#Compare percentiles Median Home Value
data00_10$HValue00_10<- ifelse(data00_10$percentil00HValue < data00_10$percentil10HValue,data00_10$percentil10HValue, NA)
#Compare percentiles Education Attaiment
data00_10$EAtt00_10<- ifelse(data00_10$percentil00EduAtt < data00_10$percentil10EduAtt, data00_10$percentil10EduAtt, NA)
#Compare percentiles Age 25 to 34
data00_10$Age2500_10<- ifelse(data00_10$percentil00Age25_34 < data00_10$percentil10Age25_34, data00_10$percentil10Age25_34, NA)

#================= Comparing percentiles 90 to 2010 ==============================================================
#After merging 90s to 2000 all inclusive
#Compare percentiles Median Household Income
data90_10$Inc90_10<- ifelse(data90_10$percentil90Income < data90_10$percentil10Income,data90_10$percentil10Income, NA)
#Compare percentiles Median Home Value
data90_10$HValue90_10<- ifelse(data90_10$percentil90HValue < data90_10$percentil10HValue,data90_10$percentil10HValue, NA)
#Compare percentiles Education Attaiment
data90_10$EAtt90_10<- ifelse(data90_10$percentil90EduAtt < data90_10$percentil10EduAtt, data90_10$percentil10EduAtt, NA)
#Compare percentiles Age 25 to 34
data90_10$Age2590_10<- ifelse(data90_10$percentil90Age25_34 < data90_10$percentil10Age25_34, data90_10$percentil10Age25_34, NA)
#===================+++++++++++++++++++++++++++=============
#find index given the name of the column
match("Inc00_10",names(data00_10))
#Return index that contain "the name"
#grep("Inc90_00",colnames (data90_00))
#Return column that contain "the name"
#data90_00 [names(data90_00 ) == "Inc90_00"]  
#Select specific columns
data_90_00 <-data90_00 [,c(1:5, 52:55)]
data_00_10 <-data00_10 [,c(1:5, 88:91)]
data_90_10 <-data90_10 [,c(1:5, 62:65)]

write.csv(data_90_00, file="data_90_00.csv")
write.csv(data_00_10, file="data_00_10.csv")
write.csv(data_90_10, file="data_90_10.csv")