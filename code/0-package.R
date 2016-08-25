
############################################
## Library used to get data from kobo

#source("http://news.mrdwab.com/install_github.R")

#library(devtools)
#install_github("mrdwab/koboloadeR")



### Tutorial to use te
## kobo_datasets 	
#Lists the datasets available for a given user. Returns a data.table with the basic metadata about the available datasets.

##kobo_submission_count 	
#Lists the number of submissions for a particular data collection project. A single integer. This function is mostly for use within the kobo_data_downloader function.


##kobo_data_downloader 	
#Downloads a specified dataset via the KoBo API. Returns a data.table of the entire dataset requested.

#host("https://kobocat.unhcr.org/api/v1/")


## ned to install DT
#install.packages('DT') 
#kobo_apps("data_viewer")


## Store my username and password in another file -- not shared on Github --
# Format is "username:password" 

library("koboloadeR")
source("code/usernamepassword.R")

library("sqldf")
library("stringr")
library("tidyr")
library("dplyr")
library("data.table")

############################################
## Library used for analysis

library(ggplot2)
library(RColorBrewer)
library(directlabels)
library(ggthemes)
###########Likert Analyisis
## http://jason.bryer.org/likert/
#library(devtools)
#install_github('jbryer/likert')
library(likert)
library(reshape)

library(readxl)
library(plyr)

#display.brewer.all()
# Choose a qualitative color palette with blue and red
#display.brewer.pal(2, 'Set1')
# Warning tells us we need to request 3+ color levels;
# Just save the first two levels: first blue, then red
#cbQualColors = brewer.pal(3, 'Set1')[c(2, 1)]
#cbQualColors # saved as character strings of hex values
# Use a sequential color scheme for the 7 MONTHS values;
# first few are too light, so request more colors and only use later ones
#display.brewer.pal(9, 'YlGn')
#cbSeqColors = brewer.pal(9, 'YlGn')[3:9]