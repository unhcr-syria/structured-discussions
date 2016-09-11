
source("code/0-package.R")


## Load data

# Structured community discussions forms:
#  -	Structured_community_discussion_debriefing_form (first version of the form deployed with only 5 records) id 636
#  -	Structured_community_discussion_numbered (second version after the first update with 47 records) id 747
#  -	Structured_community_discussion_numbered_v1 (third version after second update with 104 records as of yesterday) id 774
#  -	Structured_community_discussions_updated: this form is used only by the community centers run by SARC with a revised questionnaire where some questions were dropped.



kobo_datasetsunhcr <- kobo_datasets (user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/")
#kobo_datasetsocha <- kobo_datasets (user = usernamepasswordocha , api = "https://kc.humanitarianresponse.info/api/v1/")
#kobo_datasetsocha2 <- kobo_datasets (user = usernamepasswordocha2 , api = "https://kc.humanitarianresponse.info/api/v1/")

########################################################################
###### Downloading info
##kobo_data_downloader(formid, user = NULL, api = "kobo", check = TRUE)
########################################################################


## Downloading using
formid <- 636
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_636, "data/data_636.csv")

#formid <- 665
#kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
#write.csv(data_665, "data/data_665.csv")



formid <- 747
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_747, "data/data_747.csv")

formid <- 774
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_774, "data/data_774.csv")


data <- rbind (data_774, data_747)


write.csv(data, "data/data.csv")

data  <- read.csv("data/data.csv", row.names=1)

## Results cleaned manually
data2  <- read.csv("data/data2.csv")


source("code/1-get-labels-from-form.R")

## Conversion table to get the correct label

selectedvar <- datalabel.agreement$namenew

#names(data)
#data.agreement <-subset(data, select=c("introduction.numbers" , "introduction.partner", "introduction.commcenter", "introduction.fgtype1", "introduction.fgtype", datalabel.agreement) )
data.agreement2 <-subset(data2, select=c(selectedvar) )

write.csv(data.agreement2, "data/dataagreement.csv")
write.csv(datalabel.agreement, "data/datalabelagreementlabel.csv")

