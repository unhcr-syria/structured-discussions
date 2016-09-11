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
survey <- survey[ ,c("type",   "name" ,  "label::English"#,
                     #"label::Arabic" ,"hint::Arabic",               
                     # "hint::English", "relevant",  "required", "constraint",   "constraint_message::Arabic", 
                     # "constraint_message::English", "default",  "appearance", "calculation",  "read_only"  ,                
                     # "repeat_count"
)]

## need to delete empty rows from the form
survey <- as.data.frame(survey[!is.na(survey$type), ])
str(survey)


survey_temp <- survey %>%
  filter(!type %in% c("begin group", "end group", "note")) %>%
  separate(type, into = c("q_type", "q_group"), sep = " ", fill = "right")

#names(survey_temp)
names(survey_temp)[4] <- "label"
survey_temp1 <- as.data.frame(survey_temp)

## get variable name from data
rm(datalabel)
datalabel <- as.data.frame( names(data2))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")
## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#names(data) <- datalabel[, 2]

## Extract the variable name as defined in the form
datalabel$length <- str_length(datalabel$namenew)
#str(datalabel)
## Find the next dot to parse the label 
datalabel$find <- regexpr(".", datalabel$namenew, fixed = TRUE, useBytes = TRUE)
#summary(datalabel$find)
datalabel$nameor2 <- substr(datalabel$namenew,datalabel$find+1, 200)
datalabel$find2 <- regexpr(".",datalabel$nameor2, fixed = TRUE, useBytes = TRUE)
datalabel$name <- substr(datalabel$nameor2,datalabel$find2 +1, 200)
datalabel <- join(x=datalabel, y=survey_temp, by="name", type = "left")


#datalabel$q_group <- as.factor(datalabel$q_group)
#datalabel$namenew <- as.character(datalabel$namenew)

#names(datalabel)
## Exact variable that match likert type on agreement
#datalabel.agreement <- datalabel[ datalabel$q_group=="Agreement", ]

levels(as.factor(datalabel$q_type))

datalabel.agreement <- datalabel[ datalabel$q_type %in% c("integer", "select_one"), ]

#datalabel.agreement <- na.omit(datalabel.agreement[1:10])
#datalabel.agreement <- datalabel.agreement$namenew
