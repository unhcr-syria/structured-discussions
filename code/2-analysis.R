#### Analysis of Sructured Discussions

source("code/0-package.R")

data.agreement  <- read.csv("data/dataagreement.csv", row.names=1)
datalabel.agreement <- read.csv("data/datalabelagreementlabel.csv", row.names=1)

#summary(data)
#str(data)
#names(data)

## Quick check on variable
#table(data.agreement$introduction.fgtype,useNA = "ifany")
#table(data.agreement$introduction.fgtype1,useNA = "ifany")
#table(data.agreement$introduction.partner,useNA = "ifany")
#table(data.agreement$introduction.commcenter,useNA = "ifany")



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

### Now we recode all variables through a loop
for (i in 6:83 ) {
             data.agreement[, i] <- revalue(data.agreement[, i], c( "Strongly_Disagree"= "Strongly Disagree",
                                                                    "No_Agreement"= "No Agreement / Undecided",
                                                                    "Undecided"= "No Agreement / Undecided",
                                                                    "Strongly_Agree" = "Strongly Agree"))
             data.agreement[, i] <- factor(data.agreement[, i], levels=c( "Strongly Disagree", "Disagree", "No Agreement / Undecided", "Agree", "Strongly Agree" ) ) 
                            # factor(data.agreement[, i], 
                            #                         levels=c( "Strongly_Disagree", "Disagree", "No_Agreement", "Undecided", "Agree", "Strongly_Agree" ),
                            #                         labels=c( "Strongly Disagree", "Disagree", "No Agreement", "Undecided", "Agree", "Strongly Agree") )
            }

## Reinputing the label
# names(datalabel.agreement)
for (i in 6:83 ) { attributes(data.agreement)$variable.labels[i] <- datalabel.agreement[i ,10] }

data.agreement1 <- data.agreement
data.agreement <- data.agreement1

## Reinputing the label
#datalabel.agreement.label[6 ,10]
#names(data.agreement)[6]
for (i in 6:83 ) {  names(data.agreement)[i] <- datalabel.agreement[i ,10] }

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

plot2 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement$introduction.fgtype1)) + ggtitle("Likert analysis by Population group")
plot2.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement$introduction.fgtype1))  + ggtitle("Likert analysis by Population group - Protection statement")
plot2.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement$introduction.fgtype1))  + ggtitle("Likert analysis by Population group- NFI statement")
plot2.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement$introduction.fgtype1))  + ggtitle("Likert analysis by Population group- Shelter statement")
plot2.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement$introduction.fgtype1))  + ggtitle("Likert analysis by Population group- Livelihood statement")

plot3 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement$introduction.fgtype)) + ggtitle("Likert analysis by Age & Gender ")
plot3.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement$introduction.fgtype))  + ggtitle("Likert analysis by Age & Gender - Protection statement")
plot3.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement$introduction.fgtype))  + ggtitle("Likert analysis by Age & Gender- NFI statement")
plot3.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement$introduction.fgtype))  + ggtitle("Likert analysis by Age & Gender - Shelter statement")
plot3.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement$introduction.fgtype))  + ggtitle("Likert analysis by Age & Gender - Livelihood statement")

plot4 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement$introduction.commcenter)) + ggtitle("Likert analysis by Community Center")
plot4.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement$introduction.commcenter))  + ggtitle("Likert analysis by Community Center - Protection statement")
plot4.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement$introduction.commcenter))  + ggtitle("Likert analysis by Community Center- NFI statement")
plot4.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement$introduction.commcenter))  + ggtitle("Likert analysis by Community Center- Shelter statement")
plot4.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement$introduction.commcenter))  + ggtitle("Likert analysis by Community Center- Livelihood statement")

plot5 <- plot(likert(data.agreement[,c(6:83)], grouping = data.agreement$introduction.partner)) + ggtitle("Likert analysis by Partner")
plot5.protection <- plot(likert(data.agreement[,c(6:49)], grouping = data.agreement$introduction.partner))  + ggtitle("Likert analysis by Partner - Protection statement")
plot5.nfi <- plot(likert(data.agreement[,c(50:68)], grouping = data.agreement$introduction.partner))  + ggtitle("Likert analysis by Partner- NFI statement")
plot5.shelter <- plot(likert(data.agreement[,c(69:79)], grouping = data.agreement$introduction.partner))  + ggtitle("Likert analysis by Partner- Shelter statement")
plot5.livelihood <- plot(likert(data.agreement[,c(80:83)], grouping = data.agreement$introduction.partner))  + ggtitle("Likert analysis by Partner- Livelihood statement")



## in PDF

pdf("out/likert1.pdf", width=10, height=40)
plot1
dev.off()

pdf("out/likert2.pdf", width=10, height=60)
plot2
dev.off()

pdf("out/likert3.pdf", width=10, height=100)
plot3
dev.off()

pdf("out/likert4.pdf", width=10, height=240)
plot4
dev.off()

pdf("out/likert5.pdf", width=10, height=150)
plot5
dev.off()


pdf("out/likert-protection.pdf", width=10, height=60) 
plot1.protection
dev.off()
pdf("out/likert-PopType-protection.pdf", width=10, height=60)
plot2.protection 
dev.off()
pdf("out/likert-AgeGender-protection.pdf", width=10, height=60)
plot3.protection 
dev.off()
pdf("out/likert-CommCenter-protection.pdf", width=10, height=60)
plot4.protection
dev.off()
pdf("out/likert-partner-protection.pdf", width=10, height=60)
plot5.protection 
dev.off()



pdf("out/likert-nfi.pdf", width=10, height=60) 
plot1.nfi 
dev.off()
pdf("out/likert-PopType-nfi.pdf", width=10, height=60)
plot2.nfi 
dev.off()
pdf("out/likert-AgeGender-nfi.pdf", width=10, height=60)
plot3.nfi
dev.off()
pdf("out/likert-CommCenter-nfi.pdf", width=10, height=60) 
plot4.nfi 
dev.off()
pdf("out/likert-partner-nfi.pdf", width=10, height=60)
plot5.nfi 
dev.off()


pdf("out/likert-shelter.pdf", width=10, height=120)
plot1.shelter 
dev.off()
pdf("out/likert-PopType-shelter.pdf", width=10, height=120)
plot2.shelter 
dev.off()
pdf("out/likert-AgeGender-shelter.pdf", width=10, height=120) 
plot3.shelter
dev.off()
pdf("out/likert-CommCenter-shelter.pdf", width=10, height=120)
plot4.shelter 
dev.off()
pdf("out/likert-partner-shelter .pdf", width=10, height=120)
plot5.shelter 
dev.off()


pdf("out/likert-livelihood.pdf", width=10, height=120)
plot1.livelihood
dev.off()
pdf("out/likert-PopType-livelihood.pdf", width=10, height=120)
plot2.livelihood
dev.off()
pdf("out/likert-AgeGender-livelihood.pdf", width=10, height=120) 
plot3.livelihood
dev.off()
pdf("out/likert-CommCenter-livelihood.pdf", width=10, height=120)
plot4.livelihood
dev.off()
pdf("out/likert-partner-livelihood.pdf", width=10, height=120)
plot5.livelihood
dev.off()


## likert analysis
png("out/likert1.png", width=10, height=40, units="in", res=300)
plot1
dev.off()
png("out/likert2.png", width=10, height=60, units="in", res=300)
plot2
dev.off()
png("out/likert3.png", width=10, height=100, units="in", res=300)
plot3
dev.off()
png("out/likert4.png", width=10, height=200, units="in", res=300)
plot4
dev.off()
png("out/likert5.png", width=10, height=150, units="in", res=300)
plot5
dev.off()