getwd()
CS00 <- read.csv("CS_00R.csv")

#================= 00==============================================================
# 1.Percentiles for media household and house value 90s at 40th percentile
quantile(CS00$Median_HouseholdInc_00, prob = seq(0, 1, length = 11), type = 5)
#Percentiles in a new colum
CS00 <- within(CS00, percentilIncome<- as.integer(cut(CS00$Median_HouseholdInc_00, 
                                                      quantile(CS00$Median_HouseholdInc_00, 
                                                               probs=seq(0, 1, length = 11), 
                                                               type =5),
                                                      include.lowest=TRUE)))
#Quantiles in a new colum
# tableOne <- within(CS00, quartile <- as.integer(cut(CS00$Median_HouseholdInc_90, 
#                                                         quantile(CS00$Median_HouseholdInc_90, probs=0:4/4), 
#                                                         include.lowest=TRUE)))
#2. Percentiles for median home value 90s at 40th percentile
quantile(CS00$MedianHomeValue_00, prob = seq(0,1, length = 11), type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
CS00 <- within(CS00, percentilHValue<- as.integer(.bincode(CS00$MedianHomeValue_00, 
                                                           quantile(CS00$MedianHomeValue_00, 
                                                                    probs=seq(0, 1, length = 11), 
                                                                    type =5),
                                                           include.lowest=TRUE)))
#3. Percentiles for Education Attaiment 90s 
CS00$EAtt00 <- (CS00$Pop00_Hsmore/CS00$Pop00_25more) * 100
CS00$EAtt00 <- round(CS00$EAtt00, digits =1)
quantile(CS00$EAtt00, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
CS00 <- within(CS00, percentilEduAtt <- as.integer(cut(CS00$EAtt00, 
                                                       quantile(CS00$EAtt00, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))
#4.The tract had a population of at least 500 residents 
CS00_YG <-subset(CS00, Pop00 >= 500)

write.csv(CS00_YG, file="CS00_YG.csv")
dir()