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

formid <- 665
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_665, "data/data_665.csv")



formid <- 747
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_747, "data/data_747.csv")

formid <- 774
kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
write.csv(data_774, "data/data_774.csv")


data <- rbind (data_774, data_747)



########################################################################
################# Trying ot get the form nicely with labels 
## https://gist.github.com/mrdwab/28c13a0537044aeb5dc0
########################################################################

#source("code/1-kobo_form_downloader.R")
#kobo_form_downloader (formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/")


## Load survey structure in XLS form

form_tmp <- "data/Structured_community_discussion_numbered_v1.xls"

survey <- read_excel(form_tmp, sheet = "survey")                         


## Avoid columns without names
survey <- survey[ ,c("type",   "name" ,  "label::English", "label::Arabic" ,"hint::Arabic",               
                     "hint::English", "relevant",  "required", "constraint",   "constraint_message::Arabic", 
                     "constraint_message::English", "default",  "appearance", "calculation",  "read_only"  ,                
                     "repeat_count")]

## need to delete empty rows from the form
survey <- survey[!is.na(survey$type), ] 

survey_temp <- survey %>%
  filter(!type %in% c("begin group", "end group", "note")) %>%
  separate(type, into = c("q_type", "q_group"), sep = " ", fill = "right")

names(survey_temp)


survey_temp1 <- survey_temp[ ,c("q_type", "q_group" ,  "name" ,   "label::English")]
names(survey_temp1)[4] <- "label"
survey_temp1 <- as.data.frame(survey_temp1)

## get variable name from data
datalabel <- as.data.frame( names(data))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")


## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(data) <- datalabel[, 2]


## Extract the variable name as defined in the form
datalabel$length <- str_length(datalabel$nameor)
datalabel$find <- regexpr("/",datalabel$nameor)
datalabel$nameor2 <- substr(datalabel$nameor,datalabel$find+1, 200)
datalabel$find2 <- regexpr("/",datalabel$nameor2)
datalabel$name <- substr(datalabel$nameor2,datalabel$find2 +1, 200)
datalabel <- join(x=datalabel, y=survey_temp1, by="name", type = "left")


#datalabel$q_group <- as.factor(datalabel$q_group)
#datalabel$namenew <- as.character(datalabel$namenew)

## Exact variable that match likert type on agreement
datalabel.agreement <- datalabel[ datalabel$q_group=="Agreement", ]
datalabel.agreement <- na.omit(datalabel.agreement[1:10])
datalabel.agreement <- datalabel.agreement$namenew

## Conversion table to get the correct label
datalabel.agreement.label <-datalabel[ datalabel$namenew %in% c("introduction.partner", "introduction.commcenter",
                                                             "introduction.fgtype1", "introduction.fgtype", datalabel.agreement), ]


#names(data)
data.agreement <-subset(data, select=c("introduction.partner", "introduction.commcenter", "introduction.fgtype1", "introduction.fgtype", datalabel.agreement) )

## Inputting the label


##################################
## for the record -- different test to do the samae than above

#datalabel1 <- datalabel %>% str_split("/")
#datalabel1 <- sqldf("select * from datalabel, survey_temp1  where name LIKE nameor")

## pmatch(), and agrep(), grep(), grepl() are three functions
#datalabel1 <- pmatch(datalabel[14, ], survey_temp1$name, nomatch = NA_integer_, duplicates.ok = TRUE)
#datalabel2 <- agrep(datalabel[14, ], survey_temp1$name, ignore.case=T, value=T, max.distance = 20, useBytes = FALSE)
#datalabel3 <- grep(datalabel[14, ], survey_temp1$name, ignore.case=T, value=T, useBytes = FALSE)
#datalabel31 <- grep(datalabel[14, ], survey_temp1$name, value=TRUE)
#datalabel32 <- grep(survey_temp1$name, datalabel[14, ], value=TRUE)
#datalabel3 <- grep("($|[^a-zA-Z])|([^a-z]+|^)datalabel[14, ]",  survey_temp1$name, ignore.case = TRUE, value = TRUE)
#datalabel4 <- grepl(datalabel[14, ], survey_temp1$name)


## testing with string function

#str(survey_temp1)
#str(datalabel)
#datalabel5  <-  str_match(datalabel[14, 1], survey_temp1$name)
#datalabel51 <-  str_match(survey_temp1[14, 3], datalabel$nameor)

