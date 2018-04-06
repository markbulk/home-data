source('/home/markbulkeley/r_scripts/homeServerUtils.R')

dt.daily <- dq("select dt, kwh from electricity.usage_daily")
dt.30 <- dq("select interval_beginning, kwh from electricity.usage_30min")[interval_beginning != '0000-00-00 00:00:00']
dt.30[, dt := as.character(as.Date(interval_beginning))]
dt.dates <- data.table(dt = as.character(seq(from = as.Date(min(dt.daily$dt)), to = as.Date(max(dt.daily$dt)), by = 'day')))

dt.all <- merge(merge(dt.daily[, .(dt, daily_kwh = kwh)], dt.30[, .(sum30 = sum(kwh)), by = dt], all.x = TRUE, by = 'dt'), dt.dates, by = 'dt', all.y = TRUE)
dt.all[is.na(daily_kwh), daily_kwh := 0]
dt.all[is.na(sum30), sum30 := 0]

# Plot 1: Timeseries of data
dt.plot <- copy(dt.all[dt >= min(dt.all[sum30 > 0]$dt)])
ggplot() + 
    geom_line(data = dt.plot, mapping = aes(x = as.Date(dt), y = daily_kwh), col = 'blue', lwd = 2) + 
    geom_line(data = dt.plot, mapping = aes(x = as.Date(dt), y = sum30), col = 'dark gray', lwd = 2)
# Plot 2: Scatterplot of data
ggplot(data = dt.plot) +
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bulkeley() +
    geom_point(mapping = aes(x = daily_kwh, y = sum30), size = 2, col = 'steelblue') +
    geom_abline(slope = 1, lty = 'dashed') +
    scale_x_continuous(name = 'Reported Daily Load (kWh)') +
    scale_y_continuous(name = 'Sum of Reported 30 Minute Load (kWh)')
ggplot(data = dt.plot[sum30 > 30 & daily_kwh < 100], mapping = aes(x = daily_kwh, y = sum30)) +
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bulkeley() +
    geom_point(size = 2, col = 'steelblue') +
    geom_smooth(method='lm',formula=y~x) +
    geom_abline(slope = 1, lty = 'dashed') +
    scale_x_continuous(name = 'Reported Daily Load (kWh)') +
    scale_y_continuous(name = 'Sum of Reported 30 Minute Load (kWh)') +
    coord_cartesian(xlim = c(30, 70), ylim = c(30, 70)) +
    ggtitle('Daily versus Sum of 30 Minute Load')
# Plot 3: Cumulative, non-outlyer days
dt.plot <- copy(dt.all[dt >= min(dt.all[sum30 > 0]$dt)])[sum30 > 30 & daily_kwh < 100][order(dt)]
dt.plot[, `:=` (Daily = cumsum(daily_kwh), `30 Minute` = cumsum(sum30))]
dt.plot[, delta := Daily - `30 Minute`]
dt.plot <- dt.plot[, .(dt, Daily, `30 Minute`, delta)][, .(variable = names(.SD), value = unlist(.SD, use.names = F)), by = c('dt')]
ggplot() +
    geom_hline(yintercept = 0) + theme_bulkeley() +
    geom_line(data = dt.plot[variable %in% c('Daily', '30 Minute')], mapping = aes(x = as.Date(dt), y = value/1e3, col = variable), lwd = 2) +
    geom_line(data = dt.plot[variable == 'delta'], mapping = aes(x = as.Date(dt), y = value)) +
    scale_x_date(name = 'Date of Reporting') +
    scale_y_continuous(name = 'Cumulative Sum of Load - Color (MWh)\nDifference Between Series - Black (kWh)') +
    scale_color_discrete(name = 'Type') +
    theme(legend.position = c(0.2, 0.8))
# outage in Feb 18

# Plot 4: Correlation of 30-minute load with 30-minute weather data
dt.weather <- dq("select observation_time, temp from home.weather where observation_time >= '2017-12-01 00:00:00'")
dt.weather[, dt := as.Date(observation_time)]
dt.weather[, hr := lubridate::hour(observation_time)]
dt.weather[, min := lubridate::minute(observation_time)]
dt.weather[, min30 := ifelse(min < 30, 0, 30)]
dt.weather[, interval_beginning := paste0(dt, ' ', formatC(hr, width = 2, format = "d", flag = "0"), ":", formatC(min30, width = 2, format = "d", flag = "0"), ":00")]
dt.weatherSum <- dt.weather[, .(temp = mean(temp)), by = interval_beginning]

dt.plot <- merge(dt.30, dt.weatherSum, by = 'interval_beginning')
dt.plot[, mth := floor_date(as.Date(dt), unit = 'month')]
ggplot(data = dt.plot, mapping = aes(x = temp, y = kwh, col = mth)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method='loess',formula=y~x)    

# Plot 5: Shape of days
dt.plot <- dt.30[dt %in% dt.30[, .(.N), by = dt][N == 48]$dt]
dt.plot[, time := as.ITime(substr(interval_beginning, 12, 19))]
ggplot(data = dt.plot, mapping = aes(x = time, y = kwh, group = dt, col = dt)) +
    geom_line(alpha = 0.6) +
    scale_color_discrete(guide = FALSE)
