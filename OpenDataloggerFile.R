library(data.table)
library(dplyr)

data <- fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_Temp_RH.DAT", skip=4)
data$V1 <- as.POSIXct(data[,V1],format="%Y-%m-%d %H:%M:%S")
col_headings <- c(colnames(fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_Temp_RH.DAT", nrows=1)))
names(data) <- col_headings

