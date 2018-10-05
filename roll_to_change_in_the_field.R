

setkey(campbellData, "TIMESTAMP")
setkey(flirPicture, "V4")


      #a combien de minutes se répete le TIMESTAMP 


numberOfRow <- 0
dataroll <- data.frame()

for (i in 1:nrow(flirPicture)) {
  
  numberOfRow <- numberOfRow +1
  minutesOfTimestamp <- str_sub(flirPicture[numberOfRow,V4],-2,-1)
  if (minutesOfTimestamp>=30) {
    rollValue <- -30
  }                                              #si ça se repete entre 0 et 29 minutes, je fais correspondre flirPicture a la valeur inferieur de campbell
  #si ça se repete entre 30 et 59 minutes, je fais correspondre flirPicture a la valeur superieur de campbell
  if (minutesOfTimestamp<30) {
    rollValue <- 30
  }
  dataroll <- rbind(dataroll,campbellData[flirPicture[numberOfRow,], roll = rollValue])


}
  


dataroll <- campbellData[flirPicture, roll = rollValue]
dataroll$flirFile <- paste0(dataroll$V1,"_",dataroll$V2,"_",dataroll$V3,".tiff")
dataroll <- select(dataroll,flirFile,TIMESTAMP,AirTC_Avg,RH_Avg)                     #crée une colone flirFile


