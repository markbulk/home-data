# This script should develop a status email about the home monitoring of three pieces:
# 
# 1. Sensor data missing
# 2. Weather data (from WU) missing
# 3. SmartThings data missing
# 4. Electricity data missing for more than 60 days (and potentially going to lose daily load data)

source('/home/markbulkeley/r_scripts/homeServerUtils.R')

daily_check <- function(chr.date = as.character(Sys.Date() - 1), num.sensorThreshold = 650, num.weather = 270, num.elec = 45, num.smart = 125) {
    chr.message <- ""
    # Sensor data
    chr.query <- glue("SELECT sen.sensor_name, count(*) `ct`
                        FROM  home.sensors sen 
                        LEFT JOIN home.observations obs on obs.sensor = sen.sensor_id AND obs.time between '{chr.date} 00:00:00' AND '{chr.date} 23:59:59'
                        GROUP BY sen.sensor_name")
    dt.sensor <- dq(chr.query)
    if(nrow(dt.sensor[ct < num.sensorThreshold]) > 0) {
        chr.message <- paste0("<p>There were issues with the Raspberry Pi-based sensors.  The following had less than an acceptable count of observations:</p><br>",
                            knitr::kable(dt.sensor[ct < num.sensorThreshold]), '<br>')
    }
    # Weather data
    chr.query <- glue("SELECT DATE(time) `dt`, count(*) `ct`
                        FROM home.weather
                        WHERE time BETWEEN '{as.character(as.Date(chr.date) - 7)} 00:00:00' AND '{chr.date} 23:59:59'
                        GROUP BY `dt`")
    dt.weather <- dq(chr.query)
    dt.joiner <- data.table(dt = as.character(seq(from = (as.Date(chr.date) - 7), to = as.Date(chr.date), by = 'day')))
    dt.weather <- merge(dt.weather, dt.joiner, by = 'dt', all.y = TRUE)
    dt.weather[is.na(ct), ct := 0]
    if(nrow(dt.weather[ct < num.weather]) > 0) {
        chr.message <- paste0(chr.message, "<p>There was less weather data than should be there for the following</p><br>",
                              knitr::kable(dt.weather[ct < num.weather]), '<br>')
    }
    # SmartThings
    chr.query <- "SELECT \"deviceName\", \"value\" FROM \"temperature\" WHERE \"deviceName\"='Garage Sensor'"
    dt.smart <- influxQuery(chr.query)
    dt.smart[, dt := as.POSIXlt(paste0(substr(time, 1, 19), 'GMT'), tryFormats = c("%Y-%m-%dT%H:%M:%S%Z"))]
    dt.smart[, dt := as.Date(time)]
    dt.smartSum <- dt.smart[, .(.N), by = dt]
    if(dt.smartSum[dt == chr.date]$N < num.smart) {
        chr.message <- paste0(chr.message, glue("<p>There were fewer data points from the SmartThings system than acceptable.  
                                                Only {dt.smartSum[dt == chr.date]$N}, which is less than the {num.smart} allowed.</p>"))
    }
    # Electricity - 30 minute
    chr.query <- glue("SELECT DATE(interval_beginning) `dt`, count(*) `ct`
                        FROM  electricity.usage_30min
                        WHERE interval_beginning BETWEEN '{as.character(as.Date(chr.date) - 70)} 00:00:00' AND '{chr.date} 23:59:59'
                      GROUP BY `dt`")
    dt.elec30 <- dq(chr.query)
    dt.joiner <- data.table(dt = as.character(seq(from = (as.Date(chr.date) - 70), to = as.Date(chr.date), by = 'day')))
    dt.elec30 <- merge(dt.elec30, dt.joiner, by = 'dt', all.y = TRUE)
    dt.elec30[is.na(ct), ct := 0]
    if(max(dt.elec30[ct == 0]$dt) < as.character(as.Date(chr.date) - num.elec)) {
        chr.message <- paste0(chr.message, glue("<p>Missing 30 minute electricity data for more than the last {num.elec} days.
                                                The most recent non-zero date is {max(dt.elec30[ct == 48]$dt)}.</p><br>"))
    }
    # Electricity - Daily Load
    chr.query <- glue("SELECT dt, count(*) `ct`
                        FROM  electricity.usage_daily
                        WHERE dt BETWEEN '{as.character(as.Date(chr.date) - 70)}' AND '{chr.date}'
                      GROUP BY dt")
    dt.elecDaily <- dq(chr.query)
    dt.joiner <- data.table(dt = as.character(seq(from = (as.Date(chr.date) - 70), to = as.Date(chr.date), by = 'day')))
    dt.elecDaily <- merge(dt.elecDaily, dt.joiner, by = 'dt', all.y = TRUE)
    dt.elecDaily[is.na(ct), ct := 0]
    if(max(dt.elecDaily[ct == 0]$dt) < as.character(as.Date(chr.date) - num.elec)) {
        chr.message <- paste0(chr.message, glue("<p>Missing 30 minute electricity data for more than the last {num.elec} days.
                                                The most recent non-zero date is {max(dt.elecDaily[ct == 1]$dt)}.</p><br>"))
    }
    return(chr.message)
}

chr.date <- as.character(Sys.Date() - 1)
chr.msg <- daily_check(chr.date)
sendStoneLeighAlert(to = 'markbulk@gmail.com, andrew.bulkeley@gmail.com', 
                    subj = glue("StoneLeigh Server check for {chr.date}"), 
                    body = chr.msg, bln.html = TRUE)
