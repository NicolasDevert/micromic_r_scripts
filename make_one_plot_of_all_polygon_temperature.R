library(ggplot2)
library(tidyr)

datarollToPlot <- dataroll %>%
select(TIMESTAMP, AirTC_Avg,tempGround1,tempGround2,tempRow1,tempRow2,tempRow3,tempCanopy) %>%
gather("tempArea","value", 2:8) %>%
group_by(TIMESTAMP = cut(TIMESTAMP ,breaks="15 min"), tempArea) %>%
summarize(value = mean(as.numeric(value))) 

ggplot(datarollToPlot,
       aes(
         as.POSIXct(TIMESTAMP, format="%Y-%m-%d %H:%M:%S"),
         value, 
         group = tempArea)) + 
  geom_line(aes(color = tempArea)) +
  geom_point(aes(color = tempArea)) +
  scale_x_datetime() +
  xlab("TIME") +
  ylab("deg C") +
  ggtitle("IR and air Temperature") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
