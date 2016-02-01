#title: overview of non-native immigrants

library(data.table)
library(dplyr)
library(plotrix)
library(ggplot2)
library(RColorBrewer)

cols <- c("ENG", "NATIVITY", "POBP", "SEX", "YOEP","DECADE", "AGEP","WGTP")
pusa <- fread("../data/ss13pusa.csv", select = cols)
pusb <- fread("../data/ss13pusb.csv", select = cols)
pus <- bind_rows(pusa, pusb)
rm(pusa, pusb)
gc()

##1 Ability to speak English
# Create filter
country <- fread("../data/NonEnglish.csv")
#country <- c(001:001:150)
nonnative0 <- filter(pus, NATIVITY == 2 & POBP %in% country$V1)

# Count number of different English levels
cot <- tally(group_by(nonnative0,ENG), sort = FALSE)
cot2 <- cot$n

#plot pie chart
slices <- c(cot2[1], cot2[2], cot2[3], cot2[4]) 
lbls <- c("Very well", "Well", "Not well", "Not at all")

#layout(1,height = 1)
cols = brewer.pal(4, "Set3")
percentage <- round(100*slices/sum(slices), 1)
text <- c("Very well: ", "Well: ", "Not well: ", "Not at all: ")
perlabels <- paste(text, percentage, "%", sep="")
pie3D(slices, radius = 1.2, shade = 0.8, theta = 0.8, start = -0.1, border = "white",
      col = cols, main = "Pie Chart of Ability to Speak English", 
      labels = perlabels, labelrad = 1.3, labelcex = 1.2)
#dev.copy(png, "../figs/Eng.png")
#dev.off()

# Create object list
nonnativelist <- list()
nonnativelist[[1]] <- nonnative0
nonnativelist[[2]] <- filter(nonnative0, ENG %in% c(1, 2))
nonnativelist[[3]] <- filter(nonnative0, ENG %in% c(3, 4))

groupname <- c("English Ability: Not Specified", "English Ability: Well", 
               "English Ability: Not Well")

##2 Count vs. year of entry
for (i in 0:2)
{
nonnative <- nonnativelist[[i+1]]
cotyoe <- tally(group_by(nonnative,YOEP), sort = FALSE)
year <- cotyoe$YOEP
cotyoe2 <- cotyoe$n
plot(year, cotyoe2, type = "o", col = "darksalmon", main = "Number of Immigrants in Each Year", 
     xlab = "Year", ylab = "Number of Immigrants", xaxt="n")
points(year, cotyoe2, type = "o", pch = 19, col = "darksalmon")
axis(1, at = c(1920, 1940, 1960, 1980, 2000, 2013))
mtext(groupname[i+1])
}

##3 SEX
# note: inside a "for loop", we have to use print(ggplot()) in order to output figures 
for (i in 0:2)
{  
nonnative <- nonnativelist[[i+1]]  
nonnative$SEX <- factor(nonnative$SEX)
levels(nonnative$SEX) <- c("Male", "Female")

# DECADE (decade of entry)
nonnative$DECADE <- factor(nonnative$DECADE)
levels(nonnative$DECADE) <- c("~1950's", "1950's", "1960's", "1970's", "1980's", 
                              "1990's", "2000's~")
cotdec <- tally(group_by(nonnative,DECADE), sort = FALSE)

# Sex ratio by decades
nonnative.sum = nonnative %>%

# Get the counts
group_by(DECADE, SEX) %>%
summarise(count = n()) %>%

# Get labels and position of labels
group_by(DECADE) %>%
mutate(Percent = paste0(sprintf("%.1f", count / sum(count) * 100), "%")) %>%
mutate(pos = cumsum(count) - 0.5 * count)
print(ggplot(nonnative.sum, aes(x = DECADE, y = count)) + 
  geom_bar(aes(fill = SEX), stat = "identity") + scale_fill_brewer(palette = "Set2") +
  geom_text(aes(y = pos, label = Percent)) +
  ggtitle(bquote(atop("Sex Ratio by Decade", atop(.(groupname[i+1]), "")))) + 
            labs(x = "Decade", y = "Count") + theme_bw(base_family= 'Helvetica'))
}

##4 Age group at immigrant decade
for (i in 0:2)
{
nonnative <- nonnativelist[[i+1]] 
nonnative$AGEE <- nonnative$AGEP - (2013-nonnative$YOEP) # age at entry, why +2
nonnative$AGEG <- rep(0, nrow(nonnative)) # what does rep mean?
nonnative$AGEG <- ifelse(nonnative$AGEE >= 0 & nonnative$AGEE < 10, 0, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 10 & nonnative$AGEE < 20, 1, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 20 & nonnative$AGEE < 30, 2, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 30 & nonnative$AGEE < 40, 3, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 40 & nonnative$AGEE < 50, 4, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 50 & nonnative$AGEE < 60, 5, nonnative$AGEG)
nonnative$AGEG <- ifelse(nonnative$AGEE >= 60, 6, nonnative$AGEG)
nonnative$AGEG <- factor(nonnative$AGEG)
levels(nonnative$AGEG) <- c("0's", "10's", "20's", "30's", "40's", "50's", "60's~")
print(ggplot(nonnative, aes(x=DECADE)) + 
  geom_bar(aes(fill=AGEG), position="fill") +
  ggtitle(bquote(atop("Age Group at Immigrant Year", atop(.(groupname[i+1]), "")))))
}