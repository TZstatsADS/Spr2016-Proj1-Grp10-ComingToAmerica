#@author:Zhibo Wan
#Data input
#############
library(data.table)
library(dplyr)
library(ggplot2)

columns=c("ANC1P","ANC2P","WAGP","POWSP","POBP")

pusa=fread("..//data//ss13pusa.csv",select=columns)
pusb=fread("..//data//ss13pusb.csv",select=columns)
country_name=fread("..//data//ANC1P_NAME_MAP.csv")
POBP_name=fread("..//data//POBP.csv")
 


pus <- bind_rows(pusa, pusb)
pus=merge(pus,country_name,by="ANC1P")
pus=merge(pus,POBP_name,by="POBP")

rm(pusa, pusb)

##################################################################

#plot the wage, see a very skewed boxplot

merge(country_name,WAGE_ANC1P,by='ANC1P') %>% select(CountryName,)


boxplot(pus$WAGP,main="wage")


Nonzero_Wage=filter(pus,WAGP,WAGP>0) %>% select(WAGP)
#this script gives
Nonextrem_Wage=filter(pus,WAGP,WAGP>0,WAGP<100000) %>% select(WAGP)

#also see a very strong skewness
#hist(Nonzero_Wage$WAGP,breaks=100,col='red')

#this is a more resonable hist-graph
hist(Nonextrem_Wage$WAGP,breaks=25,col='red')



#List mean and median 
WAGE_ByState=pus %>% group_by(POWSP) %>% summarise(AVE_WAGP=mean(WAGP),MED_WAGP=median(WAGP))
#this line of script gives all NA results!!
#WAGE_ANC1P=pus %>% group_by(ANC1P) %>% summarise(AVE_WAGP_ANC1P=na.omitmean(WAGP),MED_WAGP_ANC1P=median(WAGP))
#filter out the zero wage, will see 


###below code will output the right table
mean_table=setNames(aggregate(pus$WAGP~pus$ANC1P,pus,mean),c("ANC1P","WAGP_Mean"))
median_table=setNames(aggregate(pus$WAGP~pus$ANC1P,pus,median),c("ANC1P","WAGP_Median"))
WAGP_ByANC1P=merge(mean_table,median_table,by="ANC1P")
WAGP_ByCountryName=merge(WAGP_ByANC1P,country_name,by="ANC1P") %>% select(CountryName,WAGP_Mean,WAGP_Median)


# output the highest and lowest five income country 
Hi_Low_Five=bind_rows(arrange(WAGP_ByCountryName,WAGP_Mean) %>% head(5),arrange(WAGP_ByCountryName,WAGP_Mean) %>% tail(5)) %>% arrange(desc(WAGP_Mean)) %>% select(CountryName,WAGP_Mean)

#Hi_Low_Table_prepared for box plot
Hi_Low_ALL_Data=filter(pus,ANC1P==190|ANC1P==615|ANC1P==181|ANC1P==570|ANC1P==800|ANC1P==168|ANC1P==904|ANC1P==607|ANC1P==825|ANC1P==427)
#box_plot
Hi_Low_Box=ggplot(Hi_Low_ALL_Data,aes(x=CountryName,y=WAGP))+geom_boxplot(aes(fill=CountryName),size=0.3)+stat_summary(fun.y=mean,geom="point",shape=23,size=3)



#for non-zero wage
Hi_Low_NonZero=Hi_Low_ALL_Data[Hi_Low_ALL_Data$WAGP>0 & Hi_Low_ALL_Data,]
#Hi_Low_NonZero=filter(Hi_Low_ALL_Data,WAGP>0)
Hi_Low_Box_NonZero=Hi_Low_Box=ggplot(Hi_Low_NonZero,aes(x=CountryName,y=WAGP))+geom_boxplot(aes(fill=CountryName),size=0.3)+stat_summary(fun.y=median,geom="point",shape=23,size=3)







#map prepare #####################
mean_table_new=setNames(aggregate(pus$WAGP~pus$POBP,pus,mean),c("POBP","WAGP"))
median_table_new=setNames(aggregate(pus$WAGP~pus$POBP,pus,median),c("POBP","WAGP_Median"))
WAGP_POBP=merge(mean_table_new,median_table_new,by="POBP")
WAGP_ByCountryName_new=merge(WAGP_POBP,POBP_name,by="POBP") %>% select(POBP,Countryname,WAGP_Mean,WAGP_Median)
WAGP_ByCountryName_new1=filter(WAGP_ByCountryName_new,POBP>56)


#read map file
WorldMap= readShapePoly("..//data//worldmap//ne_110m_admin_0_countries.shp") 


ggplot(WM_df, aes(long,lat, group=group)) + 
    geom_polygon() + 
    labs(title="World map (longlat)") + 
    coord_equal() + 
    theme_opts
