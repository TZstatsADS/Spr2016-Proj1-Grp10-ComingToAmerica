#use this function like region_hash["1"]

library(hash)
region_hash<-hash()
region_code=c(1,2,3,4,9)
region=c("Northeast","MidWest","South","West","Puerto Rico")
region_hash<-hash(region_code,region)







