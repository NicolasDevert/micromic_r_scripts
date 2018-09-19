
library(dplyr)
library(data.table)
library(stringr)

Sys.setenv(TZ='UTC')
setwd("C:/repo/R/Micromic_Campbell_Datalogger/data/")
dateIwant <- as.POSIXct(Sys.Date()-49)                                                    #la date qui correspond au photo que je veux analyser
strftime(dateIwant,format = "%Y%m%d")                                         #converti la date en string au bon format
searchString<-strftime(dateIwant,format = "%Y%m%d")                         #crée l'objet serachString 
fileList<-list.files(pattern = searchString,recursive = 1)                    #fileList contient toutes les photo qui ont "searchString" 
                                                                              #dans le titre et se trouve dans le working directory du 
                                                                              #programme à plus ou moins un dossier (recursive, à confirmer)


flirPicture <- as.data.frame(matrix(unlist(strsplit(fileList,"_")),ncol = 3,byrow = TRUE))    #converti d'une liste de liste en dataframe


flirPicture <- tbl_df(flirPicture) %>%
  mutate(V3=substr(V3, 1,6)) %>%
  mutate(V4=paste0(V2,V3)) %>%
  mutate(V4=as.POSIXct(V4, format="%Y%m%d%H%M%S",tz = "UTC")) %>%
  as.data.table()


data <- fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_Temp_RH.DAT", skip=4, na.strings = "NAN")
data$V1 <- as.POSIXct(data[,V1],format="%Y-%m-%d %H:%M:%S")
col_headings <- c(colnames(fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_Temp_RH.DAT", nrows=1)))
names(data) <- col_headings
selectTimeInterval <- function(x, Start, End){
  y <- subset(x, as.numeric(x$TIMESTAMP)>=as.numeric(as.POSIXct(Start)) &
                as.numeric(x$TIMESTAMP)<=as.numeric(as.POSIXct(End)))
  return(y)
}

campbellData <- selectTimeInterval(x = data, Start = dateIwant+3600, End = dateIwant+90000)     # 1 jour plus
campbellData$TIMESTAMP <- campbellData$TIMESTAMP-1*60*60                                      # solution contre le décalage de 1heure entre campbell et flir
  
  
campbellData <- select(campbellData,TIMESTAMP,AirTC_Avg,RH_Avg)

setkey(campbellData, "TIMESTAMP")
setkey(flirPicture, "V4")

minutesOfTimestamp <- str_sub(flirPicture$V4,-2,-1)
minutesOfTimestamp <- mean(as.numeric(minutesOfTimestamp))       #a combien de minutes se r?pete le TIMESTAMP 

if (minutesOfTimestamp>=30) {
  rollValue <- -30
}                                              #si ?a se repete entre 0 et 29 minutes, je fais correspondre flirPicture a la valeur inferieur de campbell
                                               #si ?a se repete entre 30 et 59 minutes, je fais correspondre flirPicture a la valeur superieur de campbell
if (minutesOfTimestamp<30) {
  rollValue <- 30
}

dataroll <- campbellData[flirPicture, roll = rollValue]
dataroll$flirFile <- paste0(dataroll$V1,"_",dataroll$V2,"_",dataroll$V3,".tiff")
dataroll <- select(dataroll,flirFile,TIMESTAMP,AirTC_Avg,RH_Avg)                     #cr?e une colone flirFile


