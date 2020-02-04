library(stringr)
library(gsheet)
library(dplyr)
library(plyr)
library(RMySQL)

#### RETREIVE METADATA
cred <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))
conn = dbConnect(MySQL(), user=cred[1, "username"], password=cred[1, "password"], dbname=cred[1, "dbname"], host=cred[1, "host"])
rs<-dbSendQuery(conn, "SELECT * FROM bci_experiments_overview")
M<- fetch(rs, n=-1);dbClearResult(dbListResults(conn)[[1]])

# RETREIVE GAME DATA
cred <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))
conn = dbConnect(MySQL(), user=cred[1, "username"], password=cred[1, "password"], dbname=cred[1, "dbname"], host=cred[1, "host"])
current_exp = 1
query <- paste("SELECT * FROM ", M[current_exp,"Exp.ID"])
rs<-dbSendQuery(conn, query)
D<- fetch(rs, n=-1);dbClearResult(dbListResults(conn)[[1]])

# DISCONNECT FROM SERVER (MAX 16 connections allowed)
dbDisconnect(conn)

# format the DateTime data
D$DateTime <- as.POSIXct(D$DateTime, format = "%Y-%m-%d %H:%M:%OS")
format(D$DateTime, "%Y-%m-%d %H:%M:%OS4")

#### SURVEY DATA
survey.conditiondata <- gsheet2tbl(M[current_exp,"Exp.SurveyData1"])
survey.experimentdata <- gsheet2tbl(M[current_exp,"Exp.SurveyData2"])

D <- D %>% left_join(survey.conditiondata, by=c("PID","TestCondition"))
D <- D %>% left_join(survey.experimentdata, by=c("PID"))

#### MANUAL DATA
# HOWTO: Make an R file 
path_to_data = paste("manual-data/", M[current_exp,"Exp.ID"],".r", sep="")
source(path_to_data)

# Convert timestamps in survey.
D$Surv.Cond.Timestamp <- as.POSIXct(D$Surv.Cond.Timestamp, format = "%d/%m/%Y %H:%M:%S")
D$Surv.Exp.Timestamp <- as.POSIXct(D$Surv.Exp.Timestamp, format = "%d/%m/%Y %H:%M:%S")


