library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

data <- fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_DataStats.DAT", skip=4, na.strings = "NAN")
data$V1 <- as.POSIXct(data[,V1],format="%Y-%m-%d %H:%M:%S")
col_headings <- c(colnames(fread("C:/repo/R/Micromic_Campbell_Datalogger/data/MicroMet_DataStats.DAT", nrows=1)))
names(data) <- col_headings
selectTimeInterval <- function(x, Start, End){
  y <- subset(x, as.numeric(x$TIMESTAMP)>=as.numeric(as.POSIXct(Start)) &
                as.numeric(x$TIMESTAMP)<=as.numeric(as.POSIXct(End)))
  return(y)
}

subdata <- selectTimeInterval(x = data, Start = "2018-06-15", End = "2018-07-24")


plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","ENE","E", "ESE", 
                                "SSE", "S","SSW", 
                                "WSW","W", "WNW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

subdata$wind_dir_compass <- as.numeric(subdata[,wind_dir_compass])
subdata$result_wind_speed <- as.numeric(subdata[,result_wind_speed])

speedmin <- floor(min(subdata$result_wind_speed,na.rm = TRUE))
speedmax <- ceiling(max(subdata$result_wind_speed,na.rm = TRUE))
if (speedmax<8) {
  speedres <- 1
}
if (speedmax>=8) {
  speedres <- round((speedmax/6)/.5)*.5
}

if (min(subdata$wind_dir_compass,na.rm = TRUE)<0) {
  subdata$wind_dir_compass_mod <- (subdata$wind_dir_compass) %% 360
  
  plot.windrose(data =  na.omit(subdata), spd = "result_wind_speed", dir = "wind_dir_compass_mod", spdmin = speedmin, spdmax = speedmax, spdres = speedres)
}

if (min(subdata$wind_dir_compass,na.rm = TRUE)>0) {
  plot.windrose(data = na.omit(subdata), spd = "result_wind_speed", dir = "wind_dir_compass", spdmin = speedmin, spdmax = speedmax, spdres = speedres)
}












