library(dplyr)
library(data.table)
library(lubridate)
library(sqldf)
library(googleAuthR)
library(googleID)
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))


#setwd("~/Documents/Analytics/Insights V1/App_v14/")

#load("Data.RData")
#source("Import_rev_data.R")
#source("Import_data.R")
