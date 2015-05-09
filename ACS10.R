getwd()
ACS10 <- read.csv("ASC_10R.csv")

#================= 10==============================================================
# 1.Percentiles for media household and house value 90s at 40th percentile
quantile(ACS10$Median_HouseholdInc_10, prob = seq(0, 1, length = 11), type = 5)
#Percentiles in a new colum
ACS10 <- within(ACS10, percentilIncome<- as.integer(cut(ACS10$Median_HouseholdInc_10, 
                                                      quantile(ACS10$Median_HouseholdInc_10, 
                                                               probs=seq(0, 1, length = 11), 
                                                               type =5),
                                                      include.lowest=TRUE)))
#Quantiles in a new colum
# tableOne <- within(ACS10, quartile <- as.integer(cut(ACS10$Median_HouseholdInc_90, 
#                                                         quantile(ACS10$Median_HouseholdInc_90, probs=0:4/4), 
#                                                         include.lowest=TRUE)))
#2. Percentiles for median home value 90s at 40th percentile
quantile(ACS10$MedianHomeValue_10, prob = seq(0,1, length = 11), type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
ACS10 <- within(ACS10, percentilHValue<- as.integer(.bincode(ACS10$MedianHomeValue_10, 
                                                           quantile(ACS10$MedianHomeValue_10, 
                                                                    probs=seq(0, 1, length = 11), 
                                                                    type =5),
                                                           include.lowest=TRUE)))
#3. Percentiles for Education Attaiment 90s 
ACS10$EAtt10 <- (ACS10$Pop10_Hsmore/ACS10$Pop10_25more) * 100
ACS10$EAtt10 <- round(ACS10$EAtt10, digits =1)
quantile(ACS10$EAtt10, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
ACS10 <- within(ACS10, percentilEduAtt <- as.integer(cut(ACS10$EAtt10, 
                                                       quantile(ACS10$EAtt10, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))
# 4 Percentiles for Age 25-34 90s 
quantile(ACS10$Pop10_2534, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. 
ACS10 <- within(ACS10, percentilAge25_40 <- as.integer(cut(ACS10$Pop10_2534, 
                                                         quantile(ACS10$Pop10_2534, 
                                                                  probs=seq(0, 1, length = 11), 
                                                                  na.rm= TRUE,
                                                                  type =5),
                                                         include.lowest=TRUE)))

#5.Location Quotient (LQ) for age group 25-34
t <- (ACS10$Pop10_2534/ACS10$Pop10*100)
m <- (sum(ACS10$Pop10_2534)/sum(ACS10$Pop10)*100)
ACS10$LQ10 <-round(t/m, digits=2)

#6.The tract had a population of at least 500 residents 
ACS10_YG <-subset(ACS10, Pop10 >= 500)

write.csv(ACS10_YG, file="ACS10_YG.csv")