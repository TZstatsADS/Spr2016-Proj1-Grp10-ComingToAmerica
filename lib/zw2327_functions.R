
prepare_data <- function(){
columns=c("ANC1P","WAGP","CountryName") 
pus=fread("..//data//zw2327.csv",select=columns)
return(pus)
}


wage_plot<-function(){
total=boxplot(pus$WAGP,main="wage")
Nonzero_Wage=filter(pus,WAGP,WAGP>0) %>% select(WAGP)
Nonextrem_Wage=filter(pus,WAGP,WAGP>0,WAGP<100000) %>% select(WAGP)

NoneXtrem_Wage_his=hist(Nonextrem_Wage$WAGP,breaks=25,col='red',main="wage without tail")
plot(total)
plot(NoneXtrem_Wage_his)
}



prepare_data_boxplot<-function(){
mean_table=setNames(aggregate(pus$WAGP~pus$ANC1P,pus,mean),c("ANC1P","WAGP_Mean"))
median_table=setNames(aggregate(pus$WAGP~pus$ANC1P,pus,median),c("ANC1P","WAGP_Median"))

WAGP_ByCountryName=merge(WAGP_ByANC1P,country_name,by="ANC1P") %>% select(CountryName,WAGP_Mean,WAGP_Median)

return(WAGP_ByCountryName)
}

prepare_data_boxplot_HiLow<-function(){
Hi_Low_Five=bind_rows(arrange(WAGP_ByCountryName,WAGP_Mean) %>% head(5),arrange(WAGP_ByCountryName,WAGP_Mean) %>% tail(5)) %>% arrange(desc(WAGP_Mean)) %>% select(CountryName,WAGP_Mean)
Hi_Low_ALL_Data=filter(pus,ANC1P==190|ANC1P==615|ANC1P==181|ANC1P==570|ANC1P==800|ANC1P==168|ANC1P==904|ANC1P==607|ANC1P==825|ANC1P==427)
#box_plot
#Hi_Low_Box=ggplot(Hi_Low_ALL_Data,aes(x=CountryName,y=WAGP))+geom_boxplot(aes(fill=CountryName),size=0.3)+stat_summary(fun.y=mean,geom="point",shape=23,size=3)

return(Hi_Low_ALL_Data)
}

box_plot<-function(){
Hi_Low_NonZero=Hi_Low_ALL_Data[Hi_Low_ALL_Data$WAGP>0 ]
Hi_Low_Box_NonZero=ggplot(Hi_Low_NonZero,aes(x=CountryName,y=WAGP))+geom_boxplot(aes(fill=CountryName),size=0.3)+stat_summary(fun.y=mean,geom="point",shape=23,size=3)
plot(Hi_Low_Box_NonZero)
return(Hi_Low_Box_NonZero)
}

prepare_data_map<-function(){
mean_table_new=setNames(aggregate(pus$WAGP~pus$POBP,pus,mean),c("POBP","WAGP"))
median_table_new=setNames(aggregate(pus$WAGP~pus$POBP,pus,median),c("POBP","WAGP_Median"))
WAGP_POBP=merge(mean_table_new,median_table_new,by="POBP")
WAGP_ByCountryName_new=merge(WAGP_POBP,POBP_name,by="POBP") %>% select(POBP,Countryname,WAGP_Mean,WAGP_Median)
WAGP_ByCountryName_new1=filter(WAGP_ByCountryName_new,POBP>56)
return(WAGP_ByCountryName_new1);
}


map_plot<-funtion()
{
ggplot(WM_df, aes(long,lat, group=group)) + 
    geom_polygon() + 
    labs(title="World map (longlat)") + 
    coord_equal() + 
    theme_opts

}




