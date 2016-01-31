#title: overview of non-native immigrants

library(data.table)
library(dplyr)
library(plotrix)
library(ggplot2)
library(RColorBrewer)

cols <- c("ENG", "NATIVITY", "POBP", "ANC1P", "SEX", "YOEP","DECADE", "AGEP")
pusa <- fread("W4249/project1/data/ss13pusa.csv", select = cols)
pusb <- fread("W4249/project1/data/ss13pusb.csv", select = cols)
pus <- bind_rows(pusa, pusb)
rm(pusa, pusb)
gc()

##1 Ability to speak English
# Create filter
country1 <- c(001:001:150)
country2 <- c(001:001:150)
nonnative <- filter(pus, NATIVITY == 2 & POBP %in% country1 & ANC1P %in% country2)

# Count number of different English levels
cot <- tally(group_by(nonnative,ENG), sort = FALSE)
cot2 <- cot$n

#plot pie chart
slices <- c(cot2[1], cot2[2], cot2[3], cot2[4]) 
lbls <- c("Very well", "Well", "Not well", "Not at all")

# grab the radial positions of the labels
pie3D(slices, radius = 1.2, labels = lbls, shade = 0.8, theta = 0.8, start = -0.1,
      col = topo.colors(4),
      labelrad = 1.4, main = "Pie chart of ability to speak English")

##2 SEX
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
ggplot(nonnative.sum, aes(x = DECADE, y = count)) + 
  geom_bar(aes(fill = SEX), stat = "identity") + 
  geom_text(aes(y = pos, label = Percent)) +
  ggtitle("Sex Ratio by Decade")

##3 Age group at immigrant year
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
ggplot(nonnative, aes(x=DECADE)) + 
  geom_bar(aes(fill=AGEG), position="fill") +
  ggtitle("Age Group at Immigrant Year")
