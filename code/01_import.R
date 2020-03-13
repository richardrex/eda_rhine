library(data.table)
station_meta <- data.table(read.csv('./data/raw/station_meta_raw.csv'))

#Data Import----
raw_path <- './data/raw/day/'
fnames <- list.files(raw_path)
n_station <- length(fnames)
id_length <- 7
station_meta[, sname := factor(abbreviate(station))]
station_meta[, id := factor(id)]

rhine_day_raw <- fread(paste0(raw_path, fnames[1]))
station_id <- substr(fnames[1], 1, id_length)
rhine_day_raw <- cbind(id = factor(station_id), rhine_day_raw)
id_sname <- station_meta[, .(id, sname)]
rhine_day_raw <- id_sname[rhine_day_raw, on = 'id']

for(file_count in 2:n_station){
  dummy <- fread(paste0(raw_path, fnames[file_count]))
  station_id <- substr(fnames[file_count], 1, id_length)
  dummy <- cbind(id = factor(station_id), dummy)
  id_sname <- station_meta[, .(id, sname)]
  dummy <- id_sname[dummy, on = 'id', ]
  rhine_day_raw <- rbind(rhine_day_raw, dummy)
}
saveRDS(rhine_day_raw, './data/rhine_day_raw.rds')

#Data Preparation----
rhine_day_raw[, 'hh:mm' := NULL]
colnames(rhine_day_raw)[3:4] <- c('date', 'value')
rhine_day_raw[, date := as.Date(date)]

saveRDS(rhine_day_raw, './data/rhine_day.rds')
saveRDS(station_meta, './data/station_meta_raw.rds')
