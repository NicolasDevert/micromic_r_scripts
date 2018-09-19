library(httr)    # for GET(...)
library(rjson)   # for fromJSON(...)
library(miniUI)
library(shiny)
set_config( config( ssl_verifypeer = 0L ) )
set_config( config( ssl_verifyhost = 0L ) )
#CURLOPT_SSL_VERIFYPEER => 0, CURLOPT_SSL_VERIFYHOST => 0,
setwd("c:/Users/ndevert/Documents/INRA/repo/R/micromic/")

rmarkdown::render('rmarkDown_test.Rmd', output_file = paste('rmarkDown_test.', Sys.Date(), '.html', sep=''))

#getdata<-GET(url=query, add_headers(Authorization="token !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))

#fromJSON(content(putData,type="text"))

#txt <-"Nicolas is here"

#query <- "https://147.100.179.81/micromic/maintenancelog/"
#putData <- POST(url=query, add_headers(Authorization="token !!!!!!!!!!!!!!!!!!!!!!!!!!!"), body=list(comment=txt))
query <- "https://147.100.179.81/micromic/dailyloglists/"
postData <- POST(url=query, add_headers(Authorization="token 2bf797f3b9bcd224c2aa7fff58d1efe7f03a6d4c"), body=list(name=paste(Sys.time()), upload=upload_file(paste('rmarkDown_test.',Sys.Date(),'.html', sep=''))))
fromJSON(content(postData,type="text"))
#getwd()
#setwd("C:\\Users\\ndevert\\Documents\\INRA\\repo\\R\\Micromic_Campbell_Datalogger")



write.csv(x = 45, file = "sheduleTest.csv")



