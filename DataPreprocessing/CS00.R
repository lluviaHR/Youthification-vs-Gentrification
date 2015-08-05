getwd()
CS00 <- read.csv("CS_00R.csv")

#================= 00==============================================================
# 1.Percentiles for media household and house value 90s at 40th percentile
quantile(CS00$Median_HouseholdInc_00, prob = seq(0, 1, length = 11), type = 5)
#Percentiles in a new colum
CS00 <- within(CS00, percentil00Income<- as.integer(cut(CS00$Median_HouseholdInc_00, 
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
CS00 <- within(CS00, percentil00HValue<- as.integer(.bincode(CS00$MedianHomeValue_00, 
                                                           quantile(CS00$MedianHomeValue_00, 
                                                                    probs=seq(0, 1, length = 11), 
                                                                    type =5),
                                                           include.lowest=TRUE)))
#3. Percentiles for Education Attaiment 90s 
CS00$EAtt00 <- (CS00$Pop00_Hsmore/CS00$Pop00_25more) * 100
CS00$EAtt00 <- round(CS00$EAtt00, digits =1)
quantile(CS00$EAtt00, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. Use .bincode since "breaks" are not unique
CS00 <- within(CS00, percentil00EduAtt <- as.integer(cut(CS00$EAtt00, 
                                                       quantile(CS00$EAtt00, 
                                                                probs=seq(0, 1, length = 11), 
                                                                na.rm= TRUE,
                                                                type =5),
                                                       include.lowest=TRUE)))
# 4 Percentiles for Age 25-34 90s 
quantile(CS00$Pop90_2534, prob = seq(0,1, length = 11), na.rm= TRUE, type = 5)
#Percentiles in a new colum. 
CS00 <- within(CS00, percentil00Age25_34 <- as.integer(cut(CS00$Pop00_2534, 
                                                         quantile(CS00$Pop00_2534, 
                                                                  probs=seq(0, 1, length = 11), 
                                                                  na.rm= TRUE,
                                                                  type =5),
                                                         include.lowest=TRUE)))
#5.Location Quotient (LQ) for age group 25-34
t <- (CS00$Pop00_2534/CS00$Pop00*100)
m <- (sum(CS00$Pop00_2534)/sum(CS00$Pop00)*100)
CS00$LQ00 <-round(t/m, digits=2)

#6.The tract had a population of at least 500 residents  
#CS00_YG <-subset(CS00, Pop00 >= 500)

write.csv(CS00, file="CS00_YG.csv")
dir()
