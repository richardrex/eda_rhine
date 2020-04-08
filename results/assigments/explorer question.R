###Explorer question 
#1
#The 0.5 quantile is the median. There is not difference

###2
runoff_day[,mean(value)]
runoff_day[,median(value)]
# median: 1220
# mean: 1372.793
# The mean is less reliable due to being more influenced by outliers.
###3
#There have big areas and in the appxl. same altitude
#So this is strage ,that they are close to each other
#This is because they belong to different countries Germany and Netherlands
###4
str(runoff_month)
library(ggplot2)
runoff_month <- runoff_month[value >= 0]  
plot1 <- ggplot(runoff_month ,
  aes(x = date, y = value)) +
  geom_area(fill="#67b3a2", alpha=0.5) +
  geom_line(color="#72b3a2") +
  ylab("dis") +
  facet_wrap(~ sname, scales = 'free')
plot1

