library(data.table)
library(ggplot2)


runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_stats <- readRDS('data/runoff_stats.rds')
runoff_month <- readRDS('data/runoff_month.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')
runoff_year <- readRDS('data/runoff_year.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())

dt <- runoff_summary[, .(sname, area)]
for_cor <- runoff_stats[dt, on = 'sname']
cor(for_cor$mean_day, for_cor$area)


for_cor_mat <- for_cor[, c('mean_day', 'area')]
cor(for_cor_mat)

runoff_month_mat <- dcast(runoff_month, date~sname)
runoff_month_cor <- cor(runoff_month_mat[, -1], use = "pairwise.complete.obs")
to_plot <- melt(runoff_month_cor)

ggplot(data = to_plot, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(col = 'black') +
  scale_fill_gradient2(low = colset_4[4], 
                       high = colset_4[1], 
                       mid = colset_4[3],
                       midpoint = 0.5,
                       limits = c(-0.1, 1)) +
  geom_text(aes(label = round(value, 1))) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab(label = "") +
  ylab(label = "")

runoff_summary[, category := 'downstream']
runoff_summary$category[3:10] <- ('mid')
runoff_summary$category[1:2] <- ('upstream')
runoff_summary[, category := factor(category, levels = c('upstream', 'mid', 'downstream'))]

runoff_month_mean <- runoff_month[, .(value = mean(value)), .(month, sname)]
to_plot <- runoff_month[runoff_summary[, .(sname, category)], on = 'sname']

ggplot(to_plot, aes(x = factor(month), y = value, fill = category, group = month)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free') +
  scale_fill_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

key_stations <- c('DOMA', 'BASR', 'KOEL')

runoff_summary_key <- runoff_summary[sname %in% key_stations]
runoff_month_key <- runoff_month[sname %in% key_stations]
runoff_winter_key <- runoff_winter[sname %in% key_stations]
runoff_summer_key <- runoff_summer[sname %in% key_stations]
runoff_year_key <- runoff_year[sname %in% key_stations]

ggplot(runoff_year_key[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)")

saveRDS(runoff_summary, './data/runoff_summary.rds')
saveRDS(runoff_summary_key, './data/runoff_summary_key.rds')
saveRDS(runoff_month_key, './data/runoff_month_key.rds')
saveRDS(runoff_winter_key, './data/runoff_winter_key.rds')
saveRDS(runoff_summer_key, './data/runoff_summer_key.rds')
saveRDS(runoff_year_key, './data/runoff_year_key.rds')