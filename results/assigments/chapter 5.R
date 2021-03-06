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

###1
year_thres <- 1950

runoff_year_key[year < year_thres, period := factor('-2000')]
runoff_year_key[year >= year_thres, period := factor('2000-')]


ggplot(runoff_year_key, aes(year, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

runoff_month_key[year < year_thres, period := factor('-2000')]
runoff_month_key[year >= year_thres, period := factor('2000-')]

ggplot(runoff_month_key, aes(month, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff") +
  theme_bw()
###We can stations BASR and KOEL have runoff increased after 2000.
###BTW for stations DOMA we can se runoff decreased after 2000.


###2 
runoff_day <- readRDS('./data/runoff_day.rds')
runoff_day[, quantile := cut(value,
                             breaks = quantile(value, probs = seq(0, 1, by = 1/10)),
                             labels = 1:10, right = FALSE)]

low_day <- runoff_day[, quantile == 1]
high_day <- runoff_day[, quantile ==9]

runoff_day[quantile == 1, by = sname, .N]
runoff_day[quantile == 9, by = sname, .N]

runoff_month <- readRDS('./data/runoff_month.rds')
runoff_month[, quantile := cut(value,
                               breaks = quantile(value, probs = seq(0, 1, by = 1/10)),
                               labels = 1:10, right = FALSE)]
low_month <- runoff_month[, quantile == 1]
high_month <- runoff_month[, quantile == 9]

runoff_month[quantile == 1, by = sname, .N]
runoff_month[quantile == 9, by = sname, .N]

runoff_month[quantile == 1, by = month, .N]
runoff_month[quantile == 9, by = month, .N]
###3

runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)


ggplot(runoff_winter[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
ggplot(runoff_summer[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
#removing the upwards trends which show up at the ends of the lines of the original graph. 
ggplot(runoff_winter[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()

ggplot(runoff_summer[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
# changing limit ,but graff looks common


ggplot(runoff_summer[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer') +
  xlab(label = "Year") +
  ylab(label = "Runoff") +
  theme_bw()
ggplot(runoff_winter[year > 1950 & year < 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter') +
  xlab(label = "Year") +
  ylab(label = "Runoff ") +
  theme_bw()
# The linear plots show more straight-forward lines .This indicates that analysis might be simplified and with more unclear variables


