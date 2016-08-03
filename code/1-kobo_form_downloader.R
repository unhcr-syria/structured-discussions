
#install.packages(httr)
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("readxl")
#install.packages(koboloadeR)




library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(koboloadeR)
library(stringdist) ## used to fuzzy match column

get_me <- function (user, URL) {
  if (is.null(user)) GET(URL, progress())
  else {
    u <- koboloadeR:::pwd_parse(user)
    GET(URL, authenticate(u$username, u$password), progress())
  }
}

kobo_form_downloader <- function(formid, user = NULL, api = "https://kobocat.unhcr.org/api/v1/") {
  
  
  ## Function to create factored version of the dataset
  ## In the real application, it would check to see whether a "choices" sheet exists or not before proceeding
  Labs <- function(data_set, survey_set, choices_set) {
    for (i in 1:nrow(survey_set)) {
      A <- choices_set[[survey_set$q_group[i]]]
      data_set[[survey_set$name[i]]] <- factor(
        data_set[[survey_set$name[i]]], A, names(A))
    }
    data_set
  }
  
  ## URL to get the form
  URL1 <- sprintf(fmt = "%sforms/%s/form.xls", koboloadeR:::host(api), formid)
  FORM <- get_me(user, URL1)
  
  ## Changed to xlsx as my forms are always desinged in xl and saved as xlsx
  form_tmp <- tempfile(fileext = ".xlsx")
  cat("\nDownloading form\n")
  
  ## This requires that the download really is an Excel file
  writeBin(content(FORM, as = "raw"), form_tmp)                             
  
  #form_tmp <- "D:/R-project/kobo-analysis/data/file6c646abbaef.xlsx"
  
   ## Otherwise, this line won't work
  cat("\nGetting questions\n")
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
  
  names(survey)
  survey <- survey[ ,c("q_type", "q_group" ,  "name" ,   "label::English")]
  
  ## Now loading choices
  cat("\nGetting choices\n")
  choices_temp <- read_excel(form_tmp, sheet = "choices")
 # names(choices)
  ## need to delete empty rows from the form
 # choices <- na.omit(choices) 
  choices_temp <- choices[!is.na(choices_temp$name), ] 
  
  choices <- lapply(split(choices_temp, choices_temp$list_name), function(y) setNames(y$name, y$label))

  #choices3 <- ldply (choices2, data.frame)
  
 
  
  ## Now getting data
  cat("\nGetting data\n")
  #api <- "https://kobocat.unhcr.org/api/v1/"
  
  URL2 <- sprintf(fmt = "%sdata/%s.csv", koboloadeR:::host(api), formid)
 # DATA <- get_me(user, URL2)
  data_tmp <- read_csv(content(get_me(user, URL2)))
  
  #data_tmp <- read.csv("D:/R-project/kobo-analysis/data/data_250.csv", comment.char="#")
  #data_tmp1 <- read.csv("D:/R-project/kobo-analysis/data/data_250.csv", comment.char="#")
  names(data_tmp)[seq_len(nrow(survey))] <- survey3$name
  

  names.data <- as.data.frame(as.character(names(data_tmp))) 
  names(names.data) <- "label"
  names.data$id <- row.names(names.data)


  
  ## changing the default variable names for coordinates
  names.data$name[names.data$name=="_GPS_coordinates_longitude"] <- "longitude"
  names.data$name[names.data$name=="_GPS_coordinates_latitude"] <- "latitude"
  names.data$name[names.data$name=="_GPS_coordinates_altitude"] <- "altitude"
  names.data$name[names.data$name=="_GPS_coordinates_precision"] <- "precision"
  
  names.data$label <- iconv(names.data$label)
  names.data$label <- as.character(names.data$label)
  
  survey$name <- iconv(survey$name)
  survey$name <- as.character(survey$name)
  
  # Fuzzy matching of names from the data & Names from the form
  # Creates a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(names.data$label, survey3$name, partial = TRUE, ignore.case = TRUE)
  
  # We now take the pairs with the minimum distance
  min.name<-apply(dist.name, 1, min)
  
  match.s1.s2 <- NULL  
  for(i in 1:nrow(dist.name))
  {
    s2.i<-match(min.name[i],dist.name[i,])
    s1.i<-i
    match.s1.s2<-rbind(data.frame(s2.i=s2.i,
                                  s1.i=s1.i,
                                  s2name=survey3[s1.i, ]$name,
                                  s1name=names.data[s2.i, ]$label, 
                                  adist=min.name[i] ),
                       match.s1.s2)
  }

  
  is.recursive(names.data)
  str(names.data)
  ## trying to match using fuzzy string match
  #source("code/partialmatch.R")

  # names.data2 <- partialMatch(names.data$labelkobo, survey3$name)
  
  
  # just like with 'match' you can control the output of no-matches:
  names.data2 <- amatch('fu',x,maxDist=1,nomatch=0)
  
  f<-function(x) merge(x,survey3[agrep(x$labelkobo[1],survey3$name,ignore.case=T,value=T,max.distance = 0.1, useBytes = FALSE),],all=TRUE)
  names.data2 <-do.call(rbind,by(names.data,names.data$labelkobo,f))
  
  f<-function(x,b) {
    matches<-b[agrep(x[1,1],b[,1]),]
    if (nrow(matches)>0) merge(x,matches,all=TRUE)
    # Ugly... but how else to create a data.frame full of NAs?
    else merge(x,b[NA,][1,],all.x=TRUE)
  }
  d<-do.call(rbind,by(names.data,names.data$labelkobo,f,survey3))
  left.over<-!(b$bID %in% d$bID)
  rbind(d,do.call(rbind,by(b[left.over,],'bID',f,a))[names(d)])
  
  
  
  agrep(names.data[1],survey3[3],ignore.case=T,value=T,max.distance = 0.1, useBytes = FALSE)
  names.data2 <- dt[lapply(car.vins, agrep, x=vin.vins, max.distance=c(cost=2, all=2), value=TRUE),
                    list(NumTimesFound=.N), vin.names]
  
  names.data2 <- join(x=names.data, y=survey3, by="name",type="left",match="first")
  
  
  
  ## We can start relabelling all questions already
  n <- length(names.data)
  for (i in 1:n ) {
    attributes(data)$variable.labels[i] <- names.data[i,"label"]
    
  }
  names(data) <- names.data[,"name"]
  names(data)
  
  
  write.csv(data_tmp, "data/data_tmp.csv")
 # print(data_tmp)
  
  data_fac <- Labs(data_tmp, survey, choices)
  write.csv(data_fac, "data/data_fac.csv")
  
  list(data = data_tmp, data_lab = data_fac, survey = survey, choices = choices)
}
