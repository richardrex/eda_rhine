library(data.table)
library(ggplot2)

runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_summary_key <- readRDS('data/runoff_summary_key.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summer_key <- readRDS('data/runoff_summer_key.rds')
runoff_winter_key <- readRDS('data/runoff_winter_key.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())

year_thres <- 2000

runoff_winter_key[year < year_thres, period := factor('pre_2000')]
runoff_winter_key[year >= year_thres, period := factor('aft_2000')]
runoff_summer_key[year < year_thres, period := factor('pre_2000')]
runoff_summer_key[year >= year_thres, period := factor('aft_2000')]

to_plot <- rbind(cbind(runoff_winter_key, season = factor('winter')), 
                 cbind(runoff_summer_key, season = factor('summer'))) 

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(to_plot[year >= 1983], aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

dt <- runoff_summary[, .(sname, area, category)]
to_plot <- runoff_stats[dt, on = 'sname']

ggplot(to_plot, aes(x = mean_day, y = area, col = category)) +
  geom_point(cex = 3) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(to_plot, aes(x = mean_day, y = area)) +
  geom_point(aes(col = category), cex = 3) +
  geom_smooth(method = 'lm', formula = y ~ x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot() +
  geom_point(data = to_plot, aes(x = mean_day, y = area, col = category), cex = 3) +
  geom_smooth(data = to_plot[c(1:7)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(8:11)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(data = to_plot[c(12:17)], aes(x = mean_day, y = area), 
              method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  scale_color_manual(values = colset_4[c(2, 3, 4)]) +
  xlab(label = "Area (km3)") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer_key, aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_winter_key, aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  facet_wrap(~sname, scales = 'free') +
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)

ggplot(runoff_winter[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

ggplot(runoff_summer[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

year_thres <- 1980
to_plot <- rbind(cbind(runoff_winter, season = factor('winter')), 
                 cbind(runoff_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]
to_plot[year < year_thres, period := factor('1950-1980')]
to_plot[year >= year_thres, period := factor('1981-2016')]

to_plot <- to_plot[year >= 1950]

ggplot(to_plot, aes(season, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
