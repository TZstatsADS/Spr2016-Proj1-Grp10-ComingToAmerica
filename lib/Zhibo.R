#@author:Zhibo Wan

library(data.table)
library(dplyr)

columns=c("ANC1P","ANC2P","WAGP","POWSP")

pusa=fread("..//data//ss13pusa.csv",select=columns)
pusb=fread("..//data//ss13pusb.csv",select=columns)

pus <- bind_rows(pusa, pusb)

rm(pusa, pusb)


levels(pus$POWSP) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                                             "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                                            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                                            "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                                            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
                                            "Wisconsin", "Wyoming", "Puerto Rico","Europe","Eastern Asia","Other asia","Canada","Mexico","Americas,Not Specified","Other US island Area")



#plot the wage, see a very skewed boxplot
boxplot(wagp,main="wage")

#List mean and median 
WAGE_ByState=pus %>% group_by(POWSP) %>% summarise(AVE_WAGP=mean(WAGP),MED_WAGP=median(WAGP))
#this line of script gives all NA results!!
WAGE_ANC1P=pus %>% group_by(ANC1P) %>% summarise(AVE_WAGP_ANC1P=na.omitmean(WAGP),MED_WAGP_ANC1P=median(WAGP))
#filter out the zero wage, will see 

Nonzero_Wage=filter(pus,WAGP,WAGP>0&&WAGP<100000) %>% select(WAGP)
#this script gives
Nonextrem_Wage=filter(pus,WAGP,WAGP>0,WAGP<100000) %>% select(WAGP)

#also see a very strong skewness
hist(Nonzero_Wage$WAGP,breaks=100,col='red')

#this is a more resonable hist-graph
hist(Nonzero_Wage$WAGP,breaks=25,col='red')
