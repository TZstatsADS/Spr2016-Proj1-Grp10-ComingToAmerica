#this is better version of lookup function


Region_Lookup=function(code){
	region_code=c(1,2,3,4,9)
	region=c("Northeast","MidWest","South","West","Puerto Rico")
	result=region[match(code,region_code)]
	return(result)}
	
Region_Lookup(2)