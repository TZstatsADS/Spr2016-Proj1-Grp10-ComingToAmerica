library(dplyr)
library(readr)

#get your current working directory
getwd()

#Please set it to your own work directory, for window users, you must use"//" instead of "/"
setwd("D:\\ADSProject\\project1\\cycle1-10\\lib")

#using read.csv(), this could take a while 
ss13pusa=fread(file="../data/ss13pusa.csv",header=TRUE)

#for this line you should input the columns you want 
data=data.frame(ss13pusa$WAGP,ss13pusa$ANC1P,ss13pusa$ANC2P)

# this lines output data to csv file 
write.csv(data,file="../data/output.csv",row.names=FALSE)

