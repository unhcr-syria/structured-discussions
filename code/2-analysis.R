#### Analysis of Sructured Discussions


#summary(data)
#str(data)
#names(data)

## Quick check on variable
table(data$introduction.fgtype,useNA = "ifany")
table(data$introduction.fgtype1,useNA = "ifany")
table(data$introduction.partner,useNA = "ifany")
table(data$introduction.commcenter,useNA = "ifany")



str(data.agreement)
## need to convert to data frame
data.agreement <- as.data.frame(data.agreement)

## Checking levels of the factor
#data.agreement$protection.documentation <- as.factor(data.agreement$protection.documentation)   
#levels(data.agreement$protection.documentation)

### Now we recode all variables through a loop
for (i in 5:84 ) {
  data.agreement[, i] <- factor(data.agreement[, i], levels=c( "Strongly_Disagree",  "Disagree","No_Agreement",  "Undecided", "Agree", "Strongly_Agree" ),
                      labels=c(  "Strongly Disagree",  "Disagree","No Agreement", "Undecided", "Agree", "Strongly Agree"))
}

for (i in 5:84 ) {
  data.agreement[, i] <- factor(data.agreement[, i], levels=c( "Strongly_Disagree",  "Disagree","No_Agreement",  "Undecided", "Agree", "Strongly_Agree" ),
                                labels=c(  "Strongly Disagree",  "Disagree","No Agreement", "Undecided", "Agree", "Strongly Agree"))
}



## Reinputing the label
for (i in 1:84 ) {
  attributes(data.agreement)$variable.labels[i] <- datalabel.agreement.label[i ,10]
}



data.agreement$introduction.fgtype <- as.factor(data.agreement$introduction.fgtype)
levels(data.agreement$introduction.fgtype)

data.agreement$introduction.fgtype1 <- as.factor(data.agreement$introduction.fgtype1)

levels(data.agreement$introduction.fgtype1)

data.agreement$introduction.partner <- as.factor(data.agreement$introduction.partner)
data.agreement$introduction.commcenter <- as.factor(data.agreement$introduction.commcenter)

## Reinputing the label
for (i in 5:84 ) {
  names(data.agreement)[i] <- datalabel.agreement.label[i ,10]
}



## likert analysis
png("out/likert1.png", width=10, height=40, units="in", res=300)
plot(likert(data.agreement[,c(5:84)]))  + ggtitle("Likert analysis")
dev.off()

## See bug: https://github.com/jbryer/likert/issues/16
detach("package:reshape", unload=TRUE)
library("reshape2")

# "introduction.partner", "introduction.commcenter", "introduction.fgtype1", "introduction.fgtype"

png("out/likert2.png", width=10, height=60, units="in", res=300)
plot(likert(data.agreement[,c(5:84)], grouping = data.agreement$introduction.fgtype1)) + ggtitle("Likert analysis")
dev.off()

png("out/likert3.png", width=10, height=100, units="in", res=300)
plot(likert(data.agreement[,c(5:84)], grouping = data.agreement$introduction.fgtype)) + ggtitle("Likert analysis")
dev.off()
