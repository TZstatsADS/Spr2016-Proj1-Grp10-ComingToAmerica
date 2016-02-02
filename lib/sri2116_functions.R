prepare_data <- function(){
  states <- read.csv("data/statenames.csv")
  subdata <- read.csv("data/schinria.csv")
  subdata <- subdata[2:6]
  states <- rename(states, c("code"="ST")) 
  alldata <- merge(subdata,states,by="ST") #merging state name file with ACS data
  
  #Filtering data only to those who are non-native to US & those whose native country doesn't have English as primary or official language
  notnative <- subset(alldata, NATIVITY==2)
  rm(alldata)
  rm(subdata) 
    
  #Reverse coding the ENG variable so that as score increases, it reflects greater ability to speak English
  notnative$ENG <- as.integer(notnative$ENG)
  notnative$rENG <- 5-notnative$ENG
notnative$notenglish <- ifelse((notnative$POBP==100|notnative$POBP==102|notnative$POBP==103|notnative$POBP==104|notnative$POBP==105|notnative$POBP==106|notnative$POBP==108|notnative$POBP==109|notnative$POBP==110|notnative$POBP==116|notnative$POBP==117|notnative$POBP==118|notnative$POBP==120|notnative$POBP==126|notnative$POBP==127|notnative$POBP==128|notnative$POBP==129|notnative$POBP==130|notnative$POBP==132|notnative$POBP==134|notnative$POBP==136|notnative$POBP==137|notnative$POBP==140|notnative$POBP==147|notnative$POBP==148|notnative$POBP==149|notnative$POBP==150|notnative$POBP==151|notnative$POBP==152|notnative$POBP==154|notnative$POBP==156|notnative$POBP==157|notnative$POBP==158|notnative$POBP==159|notnative$POBP==160|notnative$POBP==161|notnative$POBP==162|notnative$POBP==163|notnative$POBP==164|notnative$POBP==165|notnative$POBP==168|notnative$POBP==200|notnative$POBP==202|notnative$POBP==203|notnative$POBP==205|notnative$POBP==206|notnative$POBP==207|notnative$POBP==208|notnative$POBP==210|notnative$POBP==211|notnative$POBP==212|notnative$POBP==213|notnative$POBP==214|notnative$POBP==215|notnative$POBP==216|notnative$POBP==217|notnative$POBP==218|notnative$POBP==222|notnative$POBP==223|notnative$POBP==224|notnative$POBP==226|notnative$POBP==229|notnative$POBP==231|notnative$POBP==233|notnative$POBP==235|notnative$POBP==238|notnative$POBP==239|notnative$POBP==240|notnative$POBP==242|notnative$POBP==243|notnative$POBP==245|notnative$POBP==246|notnative$POBP==247|notnative$POBP==248|notnative$POBP==249|notnative$POBP==253|notnative$POBP==303|notnative$POBP==311|notnative$POBP==312|notnative$POBP==313|notnative$POBP==314|notnative$POBP==315|notnative$POBP==316|notnative$POBP==327|notnative$POBP==329|notnative$POBP==332|notnative$POBP==339|notnative$POBP==343|notnative$POBP==360|notnative$POBP==361|notnative$POBP==362|notnative$POBP==363|notnative$POBP==364|notnative$POBP==365|notnative$POBP==369|notnative$POBP==370|notnative$POBP==372|notnative$POBP==373|notnative$POBP==374|notnative$POBP==400|notnative$POBP==407|notnative$POBP==408|notnative$POBP==412|notnative$POBP==414|notnative$POBP==416|notnative$POBP==417|notnative$POBP==420|notnative$POBP==423|notnative$POBP==429|notnative$POBP==430|notnative$POBP==436|notnative$POBP==444|notnative$POBP==447|notnative$POBP==448|notnative$POBP==449|notnative$POBP==451|notnative$POBP==453|notnative$POBP==454|notnative$POBP==459|notnative$POBP==460|notnative$POBP==508|notnative$POBP==511|notnative$POBP==512|notnative$POBP==523|notnative$POBP==527),1,0)
notnative.noteng <- filter(notnative, notenglish == 1) # POBP: birthplace does not have primary or official language of English
  notnative.noteng <- na.omit(notnative.noteng)
  return(notnative.noteng)
}

states <- function(population){
  notnative.noteng <- population
  ##In What States Do Immigrants from Non-English Speaking Countries Live?

  ggplot(notnative.noteng) + geom_bar(aes(x=name), color= "black", fill="indianred") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8))) + xlab("U.S. State") + ylab("Number of Respondents") + ggtitle("In What States Do Immigrants from Non-English Speaking Countries Live?")
  #png("figs/sri2116_states.png")
  #ggplot(notnative.noteng) + geom_bar(aes(x=name), color= "black", fill="indianred") + coord_flip() + theme(axis.text.y=element_text(size=rel(0.8))) + xlab("U.S. State") + ylab("Number of Respondents") + ggtitle("In What States Do Immigrants from Non-English Speaking Countries Live?")
  #dev.off()
  # MIGHT NEED TO CALL DEV ON
}

popular_languages <- function(population){
  states <- read.csv("data/statenames.csv")
  subdata <- read.csv("data/schinria.csv")
  states <- rename(states, c("code"="ST")) 
  alldata <- merge(subdata,states,by="ST") #merging state name file with ACS data
  
  #Filtering data only to those who are non-native to US & those whose native country doesn't have English as primary or official language
  notnative <- subset(alldata, NATIVITY==2)
  rm(alldata)
  rm(subdata)

notnative$notenglish <- ifelse((notnative$POBP==100|notnative$POBP==102|notnative$POBP==103|notnative$POBP==104|notnative$POBP==105|notnative$POBP==106|notnative$POBP==108|notnative$POBP==109|notnative$POBP==110|notnative$POBP==116|notnative$POBP==117|notnative$POBP==118|notnative$POBP==120|notnative$POBP==126|notnative$POBP==127|notnative$POBP==128|notnative$POBP==129|notnative$POBP==130|notnative$POBP==132|notnative$POBP==134|notnative$POBP==136|notnative$POBP==137|notnative$POBP==140|notnative$POBP==147|notnative$POBP==148|notnative$POBP==149|notnative$POBP==150|notnative$POBP==151|notnative$POBP==152|notnative$POBP==154|notnative$POBP==156|notnative$POBP==157|notnative$POBP==158|notnative$POBP==159|notnative$POBP==160|notnative$POBP==161|notnative$POBP==162|notnative$POBP==163|notnative$POBP==164|notnative$POBP==165|notnative$POBP==168|notnative$POBP==200|notnative$POBP==202|notnative$POBP==203|notnative$POBP==205|notnative$POBP==206|notnative$POBP==207|notnative$POBP==208|notnative$POBP==210|notnative$POBP==211|notnative$POBP==212|notnative$POBP==213|notnative$POBP==214|notnative$POBP==215|notnative$POBP==216|notnative$POBP==217|notnative$POBP==218|notnative$POBP==222|notnative$POBP==223|notnative$POBP==224|notnative$POBP==226|notnative$POBP==229|notnative$POBP==231|notnative$POBP==233|notnative$POBP==235|notnative$POBP==238|notnative$POBP==239|notnative$POBP==240|notnative$POBP==242|notnative$POBP==243|notnative$POBP==245|notnative$POBP==246|notnative$POBP==247|notnative$POBP==248|notnative$POBP==249|notnative$POBP==253|notnative$POBP==303|notnative$POBP==311|notnative$POBP==312|notnative$POBP==313|notnative$POBP==314|notnative$POBP==315|notnative$POBP==316|notnative$POBP==327|notnative$POBP==329|notnative$POBP==332|notnative$POBP==339|notnative$POBP==343|notnative$POBP==360|notnative$POBP==361|notnative$POBP==362|notnative$POBP==363|notnative$POBP==364|notnative$POBP==365|notnative$POBP==369|notnative$POBP==370|notnative$POBP==372|notnative$POBP==373|notnative$POBP==374|notnative$POBP==400|notnative$POBP==407|notnative$POBP==408|notnative$POBP==412|notnative$POBP==414|notnative$POBP==416|notnative$POBP==417|notnative$POBP==420|notnative$POBP==423|notnative$POBP==429|notnative$POBP==430|notnative$POBP==436|notnative$POBP==444|notnative$POBP==447|notnative$POBP==448|notnative$POBP==449|notnative$POBP==451|notnative$POBP==453|notnative$POBP==454|notnative$POBP==459|notnative$POBP==460|notnative$POBP==508|notnative$POBP==511|notnative$POBP==512|notnative$POBP==523|notnative$POBP==527),1,0)
notnative.noteng <- filter(notnative, notenglish == 1) # POBP: birthplace does not have primary or official language of English

notnative.noteng[notnative.noteng==""] <- NA
notnative.noteng <- na.omit(notnative.noteng)

notnative.noteng$LANP <- as.factor(notnative.noteng$LANP)
levels(notnative.noteng$LANP)[1]<-NA
levels(notnative.noteng$LANP)[110]<-NA


View(levels(notnative.noteng$LANP))
levels(notnative.noteng$LANP) <- c("Jamaican Creole","Krio","German","Yiddish","Dutch","Afrikaans","Swedish","Danish","Norwegian","Italian","French","Patois","French Creole","Spanish","Portuguese","Romanian","Irish Gaelic","Greek","Albanian","Russian","Ukrainian","Czech","Polish","Slovak","Bulgarian","Macedonian","Serbo-Croatian","Croatian","Serbian","Lithuanian","Latvian","Armenian","Persian","Pashto","Kurdish","India N.E.C.","Hindi","Bengali","Panjabi","Marathi","Gujarati","Urdu","Nepali","Sindhi","Pakistan N.E.C.","Sinhalese","Finnish","Hungarian","Uighur","Turkish","Mongolian","Telugu","Kannada","Malayalam","Tamil","Chinese","Cantonese","Mandarin","Formosan","Burmese","Thai","Mien","Hmong","Japanese","Korean","Laotian","Mon-Khmer, Cambodian","Vietnamese","Indonesian","Malay","Tagalog","Bisayan","Sebuano","Ilocano","Micronesian","Chamorro","Trukese","Samoan","Tongan","Hawaiian","Arabic","Hebrew","Syriac","Amharic","Cushite","Swahili","Bantu","Mande","Fulani","Kru, Ibo, Yoruba","African","Other Algonquian languages","Ojibwa","Dakota","Keres","Cherokee","Zuni","Other Indo-European languages","Other Asian languages","Other Pacific Island languages","Other specified African languages","Aleut-Eskimo languages","South/Central American Indian languages","Other Specified North American Indian languages","Other languages")
LanguageCorpus <- paste(unlist(notnative.noteng$LANP), collapse =" ")
wordcloud(LanguageCorpus, scale=c(6,.5), max.words = 1000, random.order = FALSE, random.color = TRUE, colors = "red", ordered.colors = TRUE)
}
