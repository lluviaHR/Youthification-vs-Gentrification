getwd()
CS90 <- read.csv("CS_90R.csv")

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
#Percentiles in a new colum. 
CS90 <- within(CS90, percentilEduAtt <- as.integer(cut(CS90$EAtt90, 
                                                       quantile(CS90$EAtt90, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))
# 4 Percentiles for Age 25-34 90s 
quantile(CS90$Pop90_2534, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. 
CS90 <- within(CS90, percentilAge25_40 <- as.integer(cut(CS90$Pop90_2534, 
                                                       quantile(CS90$Pop90_2534, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))

#5.Location Quotient (LQ) for age group 25-34
t <- (CS90$Pop90_2534/CS90$Pop90*100)
m <- (sum(CS90$Pop90_2534)/sum(CS90$Pop90)*100)
CS90$LQ90 <-round(t/m, digits=2)

#6.The tract had a population of at least 500 residents 
CS90_YG <-subset(CS90, Pop90 >= 500)
write.csv(CS90_YG, file="CS90_YG.csv")
dir()