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
library(imager)


timeOfMesurement = 0

metadata <- exif_read(dataroll[1,flirFile])
imageDescription <- metadata$ImageDescription


jsonFile <- fromJSON(metadata$ImageDescription)
unlistjson <- unlist(jsonFile)
dfunlistjson <- as.data.frame(unlistjson)

for (i in 1:nrow(dataroll)) {
  timeOfMesurement=timeOfMesurement + 1
  rawPicture <- readTIFF(dataroll[timeOfMesurement,flirFile],convert = FALSE, as.is = TRUE)
  
  tempPicture <- raw2temp(raw = rawPicture, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[timeOfMesurement,AirTC_Avg], IRT = .88, RH = dataroll[timeOfMesurement,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  
  img=as.cimg(tempPicture)
  plot(img)
  
  rawPictureDF = reshape2::melt(rawPicture)
  
  locator(type = "l")
  
  locatorGround1 <- list()
  locatorGround1$x <- list(475.2012, 468.4424, 468.4424, 436.3378, 428.7341, 398.3192, 349.3176, 334.1101, 
                           294.4019, 233.5722, 326.5064, 432.9584, 474.3564)
  locatorGround1$y <- list(154.570128, 146.121561, 131.758995, 138.517850, 126.689855,  86.981586,  69.239594,  
                           55.721885, 5.875335, 9.254762, 101.344151, 221.313814, 269.470650)
  
  rawPictureDF$polygonGround1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround1$x,locatorGround1$y)
  
  subpolygonGround1 <- filter(rawPictureDF,rawPictureDF$polygonGround1 == 1)
  
  tempGround1 <- mean(subpolygonGround1$value)
  tempGround1 <- raw2temp(raw = tempGround1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempGround1"] <- tempGround1
  
  locatorRow1 <- list()
  locatorRow1$x <- list(476.7927, 444.8157, 452.3892, 406.1068, 412.8388, 346.3605, 361.5074, 343.8360,
                           302.6026, 295.8706, 247.0637, 263.8936, 252.9542, 219.2942, 200.7813, 203.3058,
                           171.3289, 166.2799, 206.6718, 303.4441, 390.9599, 401.0578, 471.7437)
  locatorRow1$y <- list(384.958755, 370.653287, 345.408344, 329.419880, 297.442952, 237.696587,
                           197.304678, 179.633217, 186.365202, 160.278761, 119.045354,  92.117415,
                           74.445954,  76.128951,  45.835019,  26.480563,  17.224083, 5.443110,
                           7.967604, 118.203856, 211.610145, 222.549621, 309.223925)
  
  rawPictureDF$polygonRow1=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow1$x,locatorRow1$y)
  
  subpolygonRow1 <- filter(rawPictureDF,rawPictureDF$polygonRow1 == 1)
  
  tempRow1 <- mean(subpolygonRow1$value)
  tempRow1 <- raw2temp(raw = tempRow1, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempRow1"] <- tempRow1
  
  
  locatorGround2 <- list()
  locatorGround2$x <- list(474.3564, 430.4238, 437.1827, 382.2670, 336.6447, 309.6093, 292.7121, 252.1590, 248.7796, 215.8302, 194.7088, 186.2602, 160.0696, 179.5013, 197.2433,
                           194.7088, 228.5030, 225.9685, 233.5722, 272.4356, 283.4187, 308.7644, 318.0579, 312.9887, 321.4373, 341.7138, 390.7155, 386.4913, 431.2687, 421.1304,
                           435.4929, 470.9769)
  locatorGround2$y <- list(475.61570, 441.82143, 421.54487, 373.38803, 292.28178, 255.10809, 230.60724, 174.00183, 151.19070, 128.37957, 103.03386, 98.80958, 54.87703, 54.87703,
                           79.37787,  89.51616, 92.89558, 118.24129, 130.91414, 161.32898, 191.74383, 198.50268, 217.08953, 244.12495, 258.48751, 262.71180, 304.10978, 334.52462,
                           358.18061, 380.14689, 400.42345, 424.07944)
  
  rawPictureDF$polygonGround2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorGround2$x,locatorGround2$y)
  
  subpolygonGround2 <- filter(rawPictureDF,rawPictureDF$polygonGround2 == 1)
  
  tempGround2 <- mean(subpolygonGround2$value)
  tempGround2 <- raw2temp(raw = tempGround2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempGround2"] <- tempGround2
  
  
  locatorRow2 <- list()
  locatorRow2$x <- list(332.42042, 288.48787, 275.81501, 253.84874, 270.74587, 243.71046, 210.76104, 206.53676, 221.74418, 198.93305, 193.01905, 182.88077, 151.62107, 145.70707,
                        155.00049, 127.12022, 133.87907, 125.43051, 80.65310, 88.25681, 117.82680, 138.10336, 138.10336, 149.93135, 163.44906, 184.57048, 184.57048, 196.39848,
                        216.67504, 222.58904, 259.76273, 257.22816, 268.21130, 287.64301, 291.86729, 286.79815, 307.91957)
  locatorRow2$y <- list(326.076055, 331.990052, 311.713489, 304.109778, 277.919218, 238.210950, 225.538098, 215.399817, 195.123254, 168.087837, 146.966417, 133.448709,
                        125.000141, 108.947862, 97.119867,  85.291872,  61.635883,  44.738747,  32.065895, 4.185622, 7.565049,  57.411599,  65.015310,  66.705023,
                        95.430154, 110.637576, 129.224425, 144.431847, 150.345845, 175.691548, 219.624100, 234.831523, 250.883801, 252.573515, 267.780937, 276.229505,
                        286.367786)
  
  rawPictureDF$polygonRow2=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow2$x,locatorRow2$y)
  
  subpolygonRow2 <- filter(rawPictureDF,rawPictureDF$polygonRow2 == 1)
  
  tempRow2 <- mean(subpolygonRow2$value)
  tempRow2 <- raw2temp(raw = tempRow2, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempRow2"] <- tempRow2
  
  
  locatorRow3 <- list()
  locatorRow3$x <- list(312.98871, 307.07471, 319.74757, 290.17758, 261.45245, 284.26358, 284.26358, 257.22816, 269.05616, 247.08988, 220.05447, 230.19275, 201.46762, 203.15733,
                        169.36306, 157.53506, 163.44906, 142.32764, 122.05108,  87.41195, 83.18767, 51.92797, 62.91110, 92.48109, 92.48109, 96.70538, 111.91280, 127.12022,
                        129.65479, 136.41364, 152.46592, 168.51820, 167.67335, 182.03591, 206.53676, 200.62276, 224.27875, 228.50303, 248.77960, 271.59073, 283.41872, 302.85043,
                        329.88585, 342.55870)
  locatorRow3$y <- list(540.66968, 525.46225, 507.72026, 480.68485, 479.83999, 461.25314, 448.58029, 443.51115, 428.30373, 388.59546, 380.99175, 359.87033, 331.14520, 310.86863,
                        279.60893, 247.50437, 228.91753, 205.26154, 193.43354, 123.31043, 103.03386,  45.58360, 42.20418, 93.74044, 100.49929, 114.86186, 121.62071, 157.94956,
                        176.53640, 195.12325, 203.57182, 223.84838, 233.98667, 244.96980, 277.91922, 292.28178, 305.79949, 328.61063, 374.23289, 390.28517, 418.16544, 461.25314,
                        491.66798, 521.23797)
  
  rawPictureDF$polygonRow3=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow3$x,locatorRow3$y)
  
  subpolygonRow3 <- filter(rawPictureDF,rawPictureDF$polygonRow3 == 1)
  
  tempRow3 <- mean(subpolygonRow3$value)
  tempRow3 <- raw2temp(raw = tempRow3, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempRow3"] <- tempRow3
  
  
  locatorRow4 <- list()
  locatorRow4$x <- list(263.987018, 221.744179, 229.347891, 208.226471, 204.002187, 188.794765, 177.811627, 186.260195, 161.759348, 162.604205, 143.172499, 148.241640,
                        125.430507, 112.757655, 113.602512, 92.481092,  89.101665,  90.791379,  83.187668,  73.049386,  63.755962,  53.617680, 43.479399,  34.185974,
                        34.185974,  23.202836,   3.771130,   6.305701,  27.427120,  40.099972,  55.307394,  73.894243,  81.497954, 104.309087, 105.153944, 140.637929,
                        162.604205, 205.691901, 242.020742, 280.039297, 299.471003)
  locatorRow4$y <- list(631.06935, 534.75568, 513.63426, 484.90913, 462.94285, 446.89057, 444.35600, 413.94116, 390.28517, 368.31889, 346.35262, 321.85177, 308.33406, 282.14350,
                        264.40151, 254.26323, 240.74552, 220.46896, 201.03725, 194.27840, 170.62241, 152.03556, 142.74213, 109.79272,  91.20587,  62.48074,  53.18731,  21.08276,
                        60.79103, 105.56844, 124.15528, 168.93269, 201.88211, 234.83152, 236.52124, 300.73035, 357.33576, 432.52801, 506.03055, 581.22280, 632.75907)
  
  rawPictureDF$polygonRow4=sp::point.in.polygon(rawPictureDF$Var1,rawPictureDF$Var2,locatorRow4$x,locatorRow4$y)
  
  subpolygonRow4 <- filter(rawPictureDF,rawPictureDF$polygonRow4 == 1)
  
  tempRow4 <- mean(subpolygonRow3$value)
  tempRow4<- raw2temp(raw = tempRow4, E = .97,OD = 15,RTemp = dfunlistjson[12,1]-273.15, ATemp = dataroll[timeOfMesurement,AirTC_Avg], IRWTemp = dataroll[883,AirTC_Avg], IRT = .88, RH = dataroll[883,RH_Avg], PR1 = dfunlistjson[16,1], PB = dfunlistjson[15,1], PF = dfunlistjson[17,1], PO = dfunlistjson[10,1], PR2 = dfunlistjson[14,1])
  dataroll[timeOfMesurement,"tempRow4"] <- tempRow4
  

  
}

