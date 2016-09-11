#### Analysis of Sructured Discussions

## Statement 64, 65 & 67 Were exclude from the analysis -- so we have 78 statements

#source("code/0-package.R")
rm(data.agreement)
data.agreement  <- read.csv("data/dataagreement.csv", row.names=1)
datalabel.agreement <- read.csv("data/datalabelagreementlabel.csv", row.names=1)
communitycenter <- read.csv("data/data.sp.csv", row.names=1)

#summary(data)
#str(data)
#names(data)

#str(data.agreement)
## need to convert to data frame
data.agreement <- as.data.frame(data.agreement)

data.agreement$introduction.fgtype <- as.factor(data.agreement$introduction.fgtype)
#levels(data.agreement$introduction.fgtype)

data.agreement$introduction.fgtype1 <- as.factor(data.agreement$introduction.fgtype1)
#levels(data.agreement$introduction.fgtype1)

data.agreement$introduction.partner <- as.factor(data.agreement$introduction.partner)
data.agreement$introduction.commcenter <- as.factor(data.agreement$introduction.commcenter)

## Checking levels of the factor
#data.agreement$protection.documentation <- as.factor(data.agreement$protection.documentation)   
#levels(data.agreement$protection.documentation)

### Now we recode all variables through a loop - let's also compute value to get our numeric value to the Z-score
for (i in 6:83 ) {
             data.agreement[, i] <- revalue(data.agreement[, i], c( "Strongly_Disagree"= "Strongly Disagree",
                                                                    "No_Agreement"= "No Agreement / Undecided",
                                                                    "Undecided"= "No Agreement / Undecided",
                                                                    "Strongly_Agree" = "Strongly Agree"))
             data.agreement[, i] <- factor(data.agreement[, i], levels=c( "Strongly Disagree", "Disagree", "No Agreement / Undecided", "Agree", "Strongly Agree" ) ) 
            }

#data.agreement1 <- data.agreement
#data.agreement <- data.agreement1

#data.agreement <- as.data.frame(data.agreement)

## Reinputing the label
#str(data.agreement)
#datalabel.agreement[6 ,10]
#names(data.agreement)[6]
# names(datalabel.agreement)
#i<-7
#for (i in 6:83) { attributes(data.agreement)$variable.labels[i] <- datalabel.agreement[i , 10] }
#for (i in 6:83) { names(data.agreement)[i] <- datalabel.agreement[i, 10] }

newname <- datalabel.agreement[, 10]
names(data.agreement) <- newname




### Check # of entries

## Quick check on variable
#table(data.agreement[, 3],useNA = "ifany")
#table(data.agreement[, 4],useNA = "ifany")
#table(data.agreement[, 2],useNA = "ifany")
#table(data.agreement[, 1],useNA = "ifany")



library("likert")
## See bug: https://github.com/jbryer/likert/issues/16
library("reshape")
detach("package:reshape", unload=TRUE)
library("reshape2")


# "introduction.partner", "introduction.commcenter", "introduction.fgtype1", "introduction.fgtype"
#plot(likert(data.agreement[,c(6:18)]))  + ggtitle("Likert analysis")


plot1 <- plot(likert(data.agreement[,c(6:83)]))  + ggtitle("Likert analysis")
plot1.protection <- plot(likert(data.agreement[,c(6:49)]))  + ggtitle("Likert analysis - Protection statement")
plot1.nfi <- plot(likert(data.agreement[,c(50:68)]))  + ggtitle("Likert analysis- NFI statement")
plot1.shelter <- plot(likert(data.agreement[,c(69:79)]))  + ggtitle("Likert analysis- Shelter statement")
plot1.livelihood <- plot(likert(data.agreement[,c(80:83)]))  + ggtitle("Likert analysis- Livelihood statement")

plot2 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement[, 3])) + ggtitle("Likert analysis by Population group")
plot2.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement[, 3]))  + ggtitle("Likert analysis by Population group - Protection statement")
plot2.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement[, 3]))  + ggtitle("Likert analysis by Population group- NFI statement")
plot2.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement[, 3]))  + ggtitle("Likert analysis by Population group- Shelter statement")
plot2.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement[, 3]))  + ggtitle("Likert analysis by Population group- Livelihood statement")

plot3 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement[, 4])) + ggtitle("Likert analysis by Age & Gender ")
plot3.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement[, 4]))  + ggtitle("Likert analysis by Age & Gender - Protection statement")
plot3.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement[, 4]))  + ggtitle("Likert analysis by Age & Gender- NFI statement")
plot3.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement[, 4]))  + ggtitle("Likert analysis by Age & Gender - Shelter statement")
plot3.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement[, 4]))  + ggtitle("Likert analysis by Age & Gender - Livelihood statement")

plot4 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement[, 2])) + ggtitle("Likert analysis by Community Center")
plot4.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement[, 2]))  + ggtitle("Likert analysis by Community Center - Protection statement")
plot4.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement[, 2]))  + ggtitle("Likert analysis by Community Center- NFI statement")
plot4.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement[, 2]))  + ggtitle("Likert analysis by Community Center- Shelter statement")
plot4.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement[, 2]))  + ggtitle("Likert analysis by Community Center- Livelihood statement")

plot5 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement[, 1])) + ggtitle("Likert analysis by Partner")
plot5.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement[, 1]))  + ggtitle("Likert analysis by Partner - Protection statement")
plot5.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement[, 1]))  + ggtitle("Likert analysis by Partner- NFI statement")
plot5.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement[, 1]))  + ggtitle("Likert analysis by Partner- Shelter statement")
plot5.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement[, 1]))  + ggtitle("Likert analysis by Partner- Livelihood statement")


## We can also produce Zscore for each partners to see the influence of the partners on the result of the discussions
## the standard score is the signed number of standard deviations by which an observation or datum differs from the mean. 
### A positive standard score indicates a datum above the mean, while a negative standard score indicates a datum below the mean

## We will then produce maps to see the results

#data.agreement1 <- data.agreement
#data.agreement <- data.agreement1

### Now convert rating into numeric level in order to generate Z-score
levels(data.agreement[, 6])
for (i in 6:83 ) {
  data.agreement[, i+(84-6)] <- data.agreement[, i]
  data.agreement[, i+(84-6)] <- revalue(data.agreement[, i+(84-6)], c( "Strongly Disagree"= "-2", "Disagree"= "-1", "No Agreement / Undecided"= "0", "Agree" = "1", "Strongly Agree" = "2"))
}

for (i in 84:161 ) {
  data.agreement[, i] <- as.numeric(data.agreement[, i])
}

## We will compute Z-score for each center for each of the statement and in general as well as for based on the 3 variable (host/displaced / and age/gender)
#zscore <- as.data.frame(data.agreement[, 84]) 
#zscore$z1 <- (data.agreement[, 84] - mean(data.agreement[, 84], na.rm=TRUE))/sd(data.agreement[, 84], na.rm=TRUE)
#zscore$z2 <-scale(data.agreement[, 84], center = TRUE, scale = TRUE)
#zscore$z3 <-scale(data.agreement[, 84], center = TRUE, scale = FALSE)

#z-score calculation
for (i in 84:161 ) {
  data.agreement[, i + 78 ] <- data.agreement[, i ]
  data.agreement[, i + 78 ] <- scale(data.agreement[, i], center = TRUE, scale = TRUE)
}

## Selecting only dataframe with Zscore and applying the right label
data.agreement.score <- data.agreement[ , c(1:5,162:239)]
names(data.agreement.score) <- newname




### Now we need to aggregate the z-score - we compute the mean for each community centers
# data.agreement.score.centers <-aggregate( data.agreement.score[ ,6] ~ data.agreement.score[ ,2], FUN=mean, na.rm=TRUE, data=data.agreement.score)

data.agreement.score.centers <- as.data.frame(unique(data.agreement.score[ ,2]))
names(data.agreement.score.centers)[1] <- "center"

for (i in 6:83 ) {
  #i <- 6
  temp <- aggregate( data.agreement.score[ ,i] ~ data.agreement.score[ ,2], FUN=mean, na.rm=TRUE, data=data.agreement.score)
  names(temp)[1] <- "center" 
  data.agreement.score.centers <- join(x=data.agreement.score.centers, y = temp , by="center", type = "left")
  rm(temp)
}



#data.agreement.score.centers1 <- data.agreement.score.centers

## Re-apply names
varname <- datalabel.agreement[c(2,6:83), 7]
labelname <- datalabel.agreement[c(2,6:83), 10]
names(data.agreement.score.centers) <- varname
attributes(data.agreement.score.centers)$variable.labels <- labelname

## Now Join with Coordinates
names(communitycenter)
names(data.agreement.score.centers)
names(data.agreement.score.centers)[1] <- "center"
data.agreement.score.centers <- join(x=data.agreement.score.centers, y = communitycenter , by="center", type = "left")

#names(data.agreement.score.centers)

## Remove records without coordinates
#data.agreement.score.centers1 <- data.agreement.score.centers
data.agreement.score.centers <- data.agreement.score.centers[!rowSums(is.na(data.agreement.score.centers[80])), ]
attributes(data.agreement.score.centers)$variable.labels <- labelname


