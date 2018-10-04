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

metadata <- exif_read("C:/Users/ndevert/Documents/INRA/repo/R/Micromic_Campbell_Datalogger/data/27/06/FLIRSystems_20180727_060002.tiff")
imageDescription <- metadata$ImageDescription
  
jsonFile <- fromJSON(metadata$ImageDescription)
unlistjson <- unlist(jsonFile)
dfunlistjson <- as.data.frame(unlistjson)
  
getwd()
rawPicture <- readTIFF("C:/Users/ndevert/Documents/INRA/repo/R/Micromic_Campbell_Datalogger/data/27/06/FLIRSystems_20180727_060002.tiff",convert = FALSE, as.is = TRUE)
  
tempPicture <- raw2temp(raw = rawPicture[1:480, 1:640], E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])

img=as.cimg(tempPicture)
plot(img)

rawPictureDF = reshape2::melt(rawPicture)


locatorGround1 <- list()
locatorGround1$x <- list(470.8020, 446.4457, 470.8020, 429.3061, 431.1103, 382.3978, 348.1186, 328.2727, 316.5457, 344.5102, 365.2582, 351.7269,
                          363.4540, 376.9853, 419.3832, 416.6769, 437.4249, 432.9145, 470.8020)
locatorGround1$y <- list(382.833983, 350.358970, 329.611045, 287.213112, 265.563104, 169.942233,  87.852617,  56.279688,  9.371337, 7.567169,
                          43.650517,  49.965102,  68.908860,  70.713027, 154.606810, 178.963070, 197.906827, 214.144333, 263.758936)

rawPictureDF$polygonGround1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround1$x,locatorGround1$y)

subpolygonGround1 <- filter(rawPictureDF,rawPictureDF$polygonGround1 == 1)

tempGround1 <- mean(subpolygonGround1$value)
tempGround1 <- raw2temp(raw = tempGround1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempGround1Value <- paste0("tempGround1 = ",tempGround1)


locatorRow1 <- list()
locatorRow1$x <- list(469.8999, 446.4457, 471.7040, 450.9561, 420.2853, 431.1103, 405.8519, 398.6353, 403.1457, 389.6144, 364.3561, 380.5936,
                       357.1394, 384.2019, 393.2228, 416.6769, 424.7957, 455.4665, 471.7040)
locatorRow1$y <- list(612.8653, 611.0612, 541.6007, 470.3361, 455.9028, 421.6236, 400.8757, 368.4006, 360.2819, 304.3527, 295.3319, 270.9756,
                       222.2631, 222.2631, 261.9548, 319.6881, 361.1840, 388.2465, 436.9590)

rawPictureDF$polygonRow1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow1$x,locatorRow1$y)

subpolygonRow1 <- filter(rawPictureDF,rawPictureDF$polygonRow1 == 1)

tempRow1 <- mean(subpolygonRow1$value)
tempRow1 <- raw2temp(raw = tempRow1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow1Value <- paste0("tempRow1 = ",tempRow1)



locatorGround2 <- list()
locatorGround2$x <- list(429.3061, 426.5999, 430.2082, 413.0686, 391.4186, 395.0269, 395.0269, 379.6915, 356.2373, 340.9019, 346.3144, 367.0623,
                          368.8665, 377.8873, 401.3415, 391.4186, 398.6353, 398.6353, 431.1103, 457.2707, 447.3478, 431.1103, 439.2290, 449.1520)
locatorGround2$y <- list(613.7674, 584.9007, 550.6215, 530.7757, 475.7486, 455.9028, 443.2736, 441.4694, 384.6382, 372.0090, 346.7506, 352.1631,
                          368.4006, 411.7007, 427.9382, 435.1548, 455.0007, 466.7278, 478.4549, 537.0903, 579.4882, 588.5091, 603.8445, 621.8862)

rawPictureDF$polygonGround2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround2$x,locatorGround2$y)

subpolygonGround2 <- filter(rawPictureDF,rawPictureDF$polygonGround2 == 1)

tempGround2 <- mean(subpolygonGround2$value)
tempGround2 <- raw2temp(raw = tempGround2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempGround2Value <- paste0("tempGround2 = ",tempGround2)



locatorRow2 <- list()
locatorRow2$x <- list(457.26789, 447.47268, 466.08357, 462.16549, 429.84131, 413.18947, 426.90275, 405.35330, 408.29186, 370.09056, 380.86529,
                       366.17248, 313.27837, 290.74940, 286.83132, 294.66748, 260.38427, 267.24091, 209.44920, 215.32632, 174.18646, 142.84181,
                       118.35379, 121.29235,  93.86578, 90.92722, 114.43571, 178.10454, 223.16249, 230.01913, 277.03612, 298.58557, 288.79036,
                       326.01214, 299.56509, 298.58557, 329.93022, 374.98817, 407.31234, 416.12803, 459.22693, 431.80036, 434.73892, 473.91974,
                       472.94022)
locatorRow2$y <- list(633.064995, 615.433626, 588.986572, 573.314244, 576.252805, 549.805751, 523.358698, 511.604452, 497.891164, 468.505549,
                       431.283770, 405.816236, 377.410142, 371.533019, 346.065485, 339.208842, 294.150898, 263.785762, 217.748298, 200.116929,
                       123.714330, 115.878165,  65.922619,  51.229812,  31.639402,  10.089950, 8.130909, 103.144399, 154.079465, 176.608437,
                       240.277270, 269.662886, 274.560488, 328.434116, 345.085965, 363.696854, 368.594457, 387.205347, 426.386167, 453.812741,
                       473.403152, 505.727328, 520.420136, 534.133423, 630.126433)

rawPictureDF$polygonRow2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow2$x,locatorRow2$y)

subpolygonRow2 <- filter(rawPictureDF,rawPictureDF$polygonRow2 == 1)

tempRow2 <- mean(subpolygonRow2$value)
tempRow2 <- raw2temp(raw = tempRow2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow2Value <- paste0("tempRow2 = ",tempRow2)



locatorRow3 <- list()
locatorRow3$x <- list(282.2665, 278.6581, 288.5811, 275.0498, 251.5956, 251.5956, 277.7561, 275.0498, 234.4560, 220.0227, 229.0435, 215.5123,
                       220.0227, 191.1560, 193.8623, 166.7998, 177.6248, 170.4081, 147.8560, 140.6393, 124.4018, 141.5414, 144.2477, 153.2685,
                      178.5269, 186.6456, 179.4289, 196.5685, 204.6873, 223.6310, 233.5540, 242.5748, 259.7144, 258.8123, 267.8331, 266.9311,
                       285.8748, 282.2665, 315.6436, 303.0144, 304.8186)
locatorRow3$y <- list(634.515331, 614.669490, 602.040318, 561.446553, 549.719465, 540.698628, 524.461121, 508.223615, 483.867356, 427.036083,
                       410.798577, 365.694393, 349.456886, 289.919363, 268.269355, 238.500593, 216.850584, 188.885990, 138.369304,  89.656785,
                        33.727596,   6.665086,  44.552600,  77.929697, 130.250551, 153.704726, 174.452651, 188.885990, 240.304760, 282.702694,
                       335.023547, 368.400644, 394.561071, 440.567339, 467.629849, 493.790276, 511.831950, 542.502795, 589.411147, 616.473657,
                       636.319499)

rawPictureDF$polygonRow3=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow3$x,locatorRow3$y)

subpolygonRow3 <- filter(rawPictureDF,rawPictureDF$polygonRow3 == 1)

tempRow3 <- mean(subpolygonRow3$value)
tempRow3 <- raw2temp(raw = tempRow3, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
tempRow3Value <- paste0("tempRow3 = ",tempRow3)



locatorCanopy <- list()
locatorCanopy$x <- list(5.326783, 6.228867, 102.751821, 130.716415, 107.262240, 109.066407, 156.876842, 170.408098)
locatorCanopy$y <- list(634.515331, 5.763002, 9.371337, 396.365238, 420.721498, 465.825682, 476.650686, 636.319499)

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



