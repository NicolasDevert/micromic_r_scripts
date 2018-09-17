library(exifr)
library(exiftoolr)
library(rjson)
library(stringr)
library(Thermimage)
library(tiff)
library(fields)
library(exiftoolr)
library(rjson)
library(tiff)
library(Thermimage)
library(dplyr)

hourOfMesurement <- 9
hourOfMesurement <- hourOfMesurement*60
minutesOfMesurement <- 00
timeOfMesurement <- hourOfMesurement + minutesOfMesurement

metadata <- exif_read(dataroll[hourOfMesurement,flirFile])
imageDescription <- metadata$ImageDescription
  
jsonFile <- fromJSON(metadata$ImageDescription)
unlistjson <- unlist(jsonFile)
dfunlistjson <- as.data.frame(unlistjson)
  

rawPicture <- readTIFF(dataroll[timeOfMesurement,flirFile],convert = FALSE, as.is = TRUE)
  
tempPicture <- raw2temp(raw = rawPicture, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])

img=as.cimg(tempPicture)

rawPictureDF = reshape2::melt(rawPicture)


locatorGround1 <- list()
locatorGround1$x <- list(468.9377, 226.5566, 290.4097, 470.2408)
locatorGround1$y <- list(248.95848, 10.48674, 14.39612, 165.55853)

rawPictureDF$polygonGround1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround1$x,locatorGround1$y)

subpolygonGround1 <- filter(rawPictureDF,rawPictureDF$polygonGround1 == 1)

tempGround1 <- mean(subpolygonGround1$value)
tempGround1 <- raw2temp(raw = tempGround1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempGround1Value <- paste0("tempGround1 = ",tempGround1)


locatorRow1 <- list()
locatorRow1$x <- list(474.1502, 153.5816, 213.5253, 471.5439)
locatorRow1$y <- list(397.514642, 6.577372, 9.183620, 272.414716)

rawPictureDF$polygonRow1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow1$x,locatorRow1$y)

subpolygonRow1 <- filter(rawPictureDF,rawPictureDF$polygonRow1 == 1)

tempRow1 <- mean(subpolygonRow1$value)
tempRow1 <- raw2temp(raw = tempRow1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow1Value <- paste0("tempRow1 = ",tempRow1)



locatorGround2 <- list()
locatorGround2$x <- list(474.1502, 128.8223, 145.7629, 472.8471)
locatorGround2$y <- list(479.61147, 10.48674, 9.18362, 420.97088)

rawPictureDF$polygonGround2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround2$x,locatorGround2$y)

subpolygonGround2 <- filter(rawPictureDF,rawPictureDF$polygonGround2 == 1)

tempGround2 <- mean(subpolygonGround2$value)
tempGround2 <- raw2temp(raw = tempGround2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempGround2Value <- paste0("tempGround2 = ",tempGround2)



locatorRow2 <- list()
locatorRow2$x <- list(291.7128, 337.3221, 122.3066, 93.6379)
locatorRow2$y <- list(340.17718, 323.23656, 17.00237, 18.30549)

rawPictureDF$polygonRow2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow2$x,locatorRow2$y)

subpolygonRow2 <- filter(rawPictureDF,rawPictureDF$polygonRow2 == 1)

tempRow2 <- mean(subpolygonRow2$value)
tempRow2 <- raw2temp(raw = tempRow2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow2Value <- paste0("tempRow2 = ",tempRow2)



locatorRow3 <- list()
locatorRow3$x <- list(316.47214, 12.84420, 48.02855, 356.86899)
locatorRow3$y <- list(535.645811, 7.880496, 7.880496, 497.855208)

rawPictureDF$polygonRow3=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow3$x,locatorRow3$y)

subpolygonRow3 <- filter(rawPictureDF,rawPictureDF$polygonRow3 == 1)

tempRow3 <- mean(subpolygonRow3$value)
tempRow3 <- raw2temp(raw = tempRow3, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow3Value <- paste0("tempRow3 = ",tempRow3)



locatorCanopy <- list()
locatorCanopy$x <- list(6.328575, 298.228404, 10.237948)
locatorCanopy$y <- list(9.18362, 630.77388, 632.07700)

rawPictureDF$polygonCanopy=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorCanopy$x,locatorCanopy$y)

subpolygonCanopy <- filter(rawPictureDF,rawPictureDF$polygonCanopy == 1)

tempCanopy <- mean(subpolygonCanopy$value)
tempCanopy <- raw2temp(raw = tempCanopy, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempCanopyValue <- paste0("tempCanopy = ",tempCanopy)

print(tempGround1Value)
print(tempRow1Value)
print(tempGround2Value)
print(tempRow2Value)
print(tempRow3Value)
print(tempCanopyValue)
meanTempRow <- (tempRow1 + tempRow2 + tempRow3 + tempCanopy)/4
meanTempGround <- (tempGround1 + tempGround2)/2
differencial_ground_to_row <- meanTempGround - meanTempRow
differencial_ground_to_row_value <- paste0("differencial_ground_to_row = ",differencial_ground_to_row)
print(differencial_ground_to_row_value)
differencial_row1_to_canopy <- tempRow1 - tempCanopy
differencial_row1_to_canopy_value <- paste0("differencial_row1_to_canopy = ",differencial_row1_to_canopy)
print(differencial_row1_to_canopy_value)

plotTherm(rotate270.matrix(tempPicture),h = dfunlistjson[7,1], w = dfunlistjson[8,1], minrangeset = min(tempPicture),thermal.palette=rainbowpal, maxrangeset = max(tempPicture))



