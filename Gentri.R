getwd()
dir()
CS90 <- read.csv("CS90_YG.csv")
CS00 <- read.csv("CS00_YG.csv")
ACS10 <- read.csv("ASC_10R.csv")

#================= 90s==============================================================
# 1.Percentiles for media household and house value 90s at 40th percentile
quantile(CS90$Median_HouseholdInc_90, prob = seq(0, 1, length = 11), type = 5)
  #Percentiles in a new colum
CS90 <- within(CS90, percentilIncome<- as.integer(cut(CS90$Median_HouseholdInc_90, 
                                                      quantile(CS90$Median_HouseholdInc_90, 
                                                               probs=seq(0, 1, length = 11), 
                                                               type =5),
                                                      include.lowest=TRUE)))
      #Quantiles in a new colum
      # tableOne <- within(CS90, quartile <- as.integer(cut(CS90$Median_HouseholdInc_90, 
      #                                                         quantile(CS90$Median_HouseholdInc_90, probs=0:4/4), 
      #                                                         include.lowest=TRUE)))
#2. Percentiles for median home value 90s at 40th percentile
quantile(CS90$MedianHomeValue_90, prob = seq(0,1, length = 11), type = 5)
    #Percentiles in a new colum. Use .bincode since "breaks" are not unique
CS90 <- within(CS90, percentilHValue<- as.integer(.bincode(CS90$MedianHomeValue_90, 
                                                      quantile(CS90$MedianHomeValue_90, 
                                                               probs=seq(0, 1, length = 11), 
                                                               type =5),
                                                      include.lowest=TRUE)))
#3. Percentiles for Education Attaiment 90s 
CS90$EAtt90 <- (CS90$Pop90_Hsmore/CS90$Pop90_25more) * 100
CS90$EAtt90 <- round(CS90$EAtt90, digits =1)
quantile(CS90$EAtt90, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
CS90 <- within(CS90, percentilEduAtt <- as.integer(cut(CS90$EAtt90, 
                                                       quantile(CS90$EAtt90, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))

#4.The tract had a population of at least 500 residents 
CS905 <-subset(CS90, Pop90 >= 500)
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
datafull <- merge(CS90,CS00, by="Geo_FIPS", all=TRUE)
# First join 90s to 2000 just machting  FIPS
data <- merge(CS00,CS90, by=3)
# Second join 2000 to 2010 all inclusive
datafull2 <- merge(datafull,ACS10, by="Geo_FIPS", all=TRUE)
# Secon join 2000 to 2010 just machting  FIPS
data <- merge(data,ACS10, by="Geo_FIPS") 
# Second join 2000 to 2010 just matching from 90-00 to 2010
datafull3 <- merge(data,ACS10, by="Geo_FIPS", all=TRUE)
# Second join 2000 to 2010 just matching from 90-00 to 2010
data3 <- merge(datafull,ACS10, by="Geo_FIPS") 
# Join just matching FIPS from 90-2010 and then to 2010
dat <- merge(ACS10,CS90, by="Geo_FIPS")
dat2 <- merge(dat,CS00, by="Geo_FIPS")

write.csv(, file="data_gentriNYC.csv")