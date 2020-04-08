library(data.table)
library(ggplot2)
#################
runoff_stations <- fread('./data/raw/runoff_stations.csv')
head(runoff_stations)
#################
#1
runoff_stations[, sname := factor(abbreviate(station))]
runoff_stations[, id := factor(id)]
runoff_stations[, altitude := round(altitude, 0)]
runoff_saa <- runoff_stations[,.(sname, area, altitude)]
runoff_saa$size <- runoff_saa[,(2*(area/altitude))]
runoff_lls <- runoff_stations[,.(sname, lon, lat)]
runoff_lls$altitude <- runoff_saa[,altitude]


runoff_saa <- runoff_stations[,.(sname, area, altitude)]
runoff_saa$size <- runoff_saa[,(2*(area/altitude))]
#2
ggplot(data = runoff_saa)+
  geom_point(aes(x = area, y = altitude))
#3
p1 <- ggplot(runoff_saa ,aes(x = area, y = altitude))
p1 + geom_point(aes(color = size)) +
  geom_text(aes(label=sname,color=size), hjust=0, vjust=0) 

p2 <- ggplot(data = runoff_lls ,aes(x = lon, y = lat,label=sname))
p2 + geom_point(aes(color=altitude)) +
  scale_color_gradient( low = "darkgreen",high = "darkred") +
  geom_text(aes(label=sname,color=altitude), hjust=0, vjust=0)
  

#4
runoff_day <- readRDS('./data/runoff_day_raw.rds')

p3_1 <- ggplot(data = runoff_day, aes(x = sname, y = date, color=sname))
p3_1 + geom_boxplot()
######################$$$$$$$###################
###Explorer questions###

# 1a area: m^2
# 1b runoff: m^3/s

#2:
runoff_stations <- readRDS('./data/runoff_stations.rds')
runoff_day <- readRDS('./data/runoff_day.rds')

average_catchment <- mean(runoff_stations[,area])
average_catchment
### 74490.29

average_runoff <- mean(runoff_day[,value])
average_runoff
### 1372.793


#3:
head(runoff_day)

runoff_day[, list(avg=mean(value)), by=sname]

avg_runoff_stations <- runoff_day[, .(mean_runoff = mean(value)), by = sname]
avg_runoff_stations <- avg_runoff_stations[, mean_runoff := round(mean_runoff, 3)]
sname_plot <- ggplot(aver_runoff_stations, aes(x = sname, y = mean_runoff)) +
  geom_bar(stat = "identity", width = 0.4)

print(sname_plot + labs(title = "Average runoff per station", x = "Station", y = "Mean runoff (m³)"))
#4: if we speak about currnt sitiation , area and altitude havent got any relationships


