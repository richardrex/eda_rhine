####1
library(ggplot2)
library(data.table)
runoff_stats
runoff_melt <- melt(runoff_stats, id.vars = 'sname')
ggplot(runoff_melt, 
       aes(x = sname, y = value, shape = "cyl", color = "cyl", size = 0.5)) + 
  geom_point(aes(col = variable, shape = variable))

####2
#a
runoff_stats$skew <- (runoff_stats$mean_day - runoff_stats$median) / runoff_stats$sd_day
runoff_stats$cof <- runoff_stats$sd_day / runoff_stats$mean_day
runoff_stats
####2
#b
runoff_stats_new <- data.table(skew = (runoff_stats$mean_day - runoff_stats$median) / runoff_stats$sd_day, cv = runoff_stats$sd_day / runoff_stats$mean_day)
runoff_stats_new
####4

p1 <- ggplot(runoff_day, aes(x = sname, y = value))
p1 + geom_boxplot() 
###the bigger station altitude we have ,more outliers it makes
###i thinks that is because he have different amount of precipitations 
###in higher places we have bigger aount of snow

###5
head(runoff_stations)
readRDS(runoff_summary, './data/runoff_summary.rds')
readRDS(runoff_stats, './data/runoff_stats.rds')
readRDS(runoff_day, './data/runoff_day.rds')
readRDS(runoff_month, './data/runoff_month.rds')
readRDS(runoff_summer, './data/runoff_summer.rds')
readRDS(runoff_winter, './data/runoff_winter.rds')
readRDS(runoff_year, './data/runoff_year.rds')




to_plot <- runoff_stations[, area_class := factor('small')]
to_plot <- runoff_stations[area >= 10000 & area < 130000, area_class := factor('medium')]
to_plot <- runoff_stations[area >= 130000, area_class := factor('large')]

to_plot <- runoff_day[, .(mean_day = round(mean(value), 0),
                               sd_day = round(sd(value), 0),
                               min_day = round(min(value), 0),
                               max_day = round(max(value), 0)), by = sname]
head(runoff_stats, 4)


tab1 <- to_plot[,.(mean_day)]
tab1

runoff_stations$mean_days <- tab1

runoff_stations$mean_days


runoff_stations$mean_days <- runoff_stats_tidy$value[1:17]
runoff_stations
ggplot(runoff_stations, aes(x = mean_days, y = area)) + 
  geom_point(aes( size=alt_class, col = area_class)) +
  xlim(c(0, 2000)) + 
  ylim(c(0, 150000)) + 
  labs(subtitle="runoff / area", 
       y="area", 
       x="mean runoff per day", 
       title="My plot")

###3i will solve it tommorow  no idea how to do



ggplot(runoff_month, aes(x = factor(month), y = value,)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~ sname, scales = 'free',) + 
  theme_bw()







