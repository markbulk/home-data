library(data.table) # install.packages('data.table', '/usr/local/lib/R/site-library')
library(lubridate)  # install.packages('lubridate', '/usr/local/lib/R/site-library')
library(stringr)    # install.packages('stringr', '/usr/local/lib/R/site-library')

# create table `electricity`.`usage_30min` (
#     interval_beginning datetime,
#     kwh decimal(5,1),
#     PRIMARY KEY(interval_beginning)
# );
# create table `electricity`.`usage_daily` (
#     dt date,
#     kwh decimal(4,0),
#     temp_max decimal(3,0),
#     temp_min decimal(3,0),
#     PRIMARY KEY(dt)
# );

# This function doesn't work because I can't figure out the cURL issues.  I'm going to let it go for now.
get30MinuteData <- function() {
    opts = curlOptions(header = TRUE, userpwd = "markbulk:jlmab0872", netrc = TRUE)
    html.downloadPage <- getURL("https://mya.dominionenergy.com/Usage/DailyIntervalData", verbose = TRUE, .opts = opts)
    kwhStart <- as.numeric(regexpr("csv(KWH)", html.downloadPage))
    linkStart <- as.numeric(regexpr("", html.downloadPage))
    # https://mya.dominionenergy.com/usage/ViewDailyIntervalData?d=JhIqSNcAHLQwIPvLHwbFYE%2fT8kIk%2beP0GysdLCrBm4wcu419lUPs1AU57xh9JQn4MfNfAi4HgoaCwKy4N0Fq%2fl9jQB2K4NnxC6wMFYo7%2flMq%2fCU4bevpXacYFMaLV2iiQlsf3Yj8YphJvqo%2ffnG%2bDAZpZPknYmrMfchE5XKI%2bwbKqqibwoqnb98x%2f7utgmSV3qbfr6SmglI%3d
    
    
    # curl 'https://mya.dominionenergy.com/usage/ViewDailyIntervalData?d=JhIqSNcAHLQwIPvLHwbFYE%2fT8kIk%2beP0GysdLCrBm4wcu419lUPs1AU57xh9JQn4MfNfAi4HgoaCwKy4N0Fq%2fl9jQB2K4NnxC6wMFYo7%2flMq%2fCU4bevpXacYFMaLV2iiQlsf3Yj8YphJvqo%2ffnG%2bDAZpZPknYmrMfchE5XKI%2bwbKqqibwoqnb98x%2f7utgmSV3qbfr6SmglI%3d' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8,ko;q=0.6' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.112 Safari/537.36' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Referer: https://mya.dominionenergy.com/Usage/DailyIntervalData' -H 'Cookie: dailyUsage=dailyAvgUsage=/yZt24vU8m4=; SMSESSION=+u992tLHyyWrPGruzm20gYaA6vTBKahPaJQZQBfEC9HywdgALOFI8S5vJzhtLvyuYuabPjXExICxTu6EMlnmMCmIHPhotcyfePHHfhKFTV7kj3VxFFEFvoFGergdBmE1hBOznENxATpaGcTsH3AFFGhFSf0P8GbWHjE8aaaDE8sSTONHvMsyORR23jtw+Vvbj8sWVaHfhyWASG7SRhdnWcnKFgtxx2ZHgn7nXiy6BWLucIMeqjCLOHu4NiG2jc9XuM4McL9UFZfPg6KqASvKBr4Cs30gL+MdD3O2QTcbv56kNZrppXXPsAykZ3heUOvb0Mu3DiPfdKM+fl8UZ3Uw7HDBSWXzIOQlP6w9iYt5vHebUgT6V4TG2WrCUa1T0dZZ/T8PVqfxxpm4/Kxz0t/DJY6Z4QQXnwS2RRBrY7xeNJ4q3Jr+gA/9WDK1CpVi1dsyTdk5CBy6flNFboc2R4eFKW577FeaABU3wV8s6HcSh1882GnT1+oy17ERSmvCc0+bqY0QmGn7yiEBdh5+C3hb6Ana/xnEB+tncwxv5EcM9w7fPC2GmAXmsMFKBxiuCCcuVXt1Hty/7qQTF+R7u6tAGnW4Rh0jHDB9r8MqkTdicH2wKERJwVXkBhdpDkk0g3wj1cyCZU+kwvoddZLl8n1wxbkeVgi8SJWhZU4X0GjeFlX1xmfrRGzBI1lu76a7dI/SipkiWXibdoHQqoZgVD6HCHmkgtMp9UeNtBGXPK6haQfYK3CzWdcm0fC1w21LvVfzSO52uZ2llLvqQ4nYTf9BXgr+kwAmjh31rf5yMGrYEmzsqF0c7jZ67m/my8nZgZYeTc1VBLot6kcItozeM/njqvGKrZj4yCUvDT4GS47Q4rtgwJPbYwNDj9xLuWdd+2g5m/fzpyYfsAVFadQikYANIksV1p1sRE1FmIodfUxpFLB7JKy65MplV74hoPkY4BTXffQQlkxgPUZQxDcu1I5mthT8zjV4S/m9s5N/NE1CsDL9YO/BhdjPiXc49xSZKRYxXP83HhSUTIrIWJMdWxTTk2qlGCKSwuAO0uYP8N0AggmPZu7QGTUqBVAhEa+t6FyQkR3KGtQrnq3rMzd/suChB+ZijV1vNQrZs5ZT2WBuoGVKbjcRig/5Q3ojgwlcH+LyqlpfcJN7K+dF8h8FfdXyqFl1vBAhvrIF; ui=an=cgTnw9mJ1sKCMJiWT7Pa5w==&br=aX5eS46Pf1s=&uu=4RzAQmSpKFaC2gmd+NvowLiZxmbZ43ZD&ci=rElJffBFz9kWWfDNKOhyug==&anm=HhHVKJp2XcAL9bits3qkfQ==; cnc=cn=nXdfHONBtLU=&an=cgTnw9mJ1sKCMJiWT7Pa5w==; val=an=cgTnw9mJ1sKCMJiWT7Pa5w==&v=ga9gcawnXa8=; __RequestVerificationToken=Odo0DOZaPzDVgVhjRaaqkuRs07knhwcMyzZvKEnr-V2kvqAQA9zvfJVUPnb0ZH5-6wvclS-5Uh2nVGcuxWppFf7op7k1; WT_FPC=id=2e82db87c52ee05d3bf1512314048768:lv=1512314059659:ss=1512314048768' -H 'Connection: keep-alive' --compressed
    
    
    # https://www.dominionenergy.com/sign-in
    chr.signIn <- "https://www.dominionenergy.com/sign-in"
    curl = getCurlHandle(cookiefile = '', verbose = TRUE)
    html.loginPage <- getURL(chr.signIn, verbose = TRUE)
    postForm(uri = chr.signIn, curl = curl,
             .params = list("smauthreason" = "0", "user" = "markbulk", "password" = "jlmab0872", "target" = "https://mydom.dominionenergy.com", "app-list" = "7668fb83-8e20-44fd-8dd9-2c5c75fe5a43"))
    html.downloadPage <- getURLContent(url = "https://mya.dominionenergy.com/Usage/DailyIntervalData", curl = curl, verbose = TRUE)
}

parse30MinuteData <- function(location = '/tmp/history-kwh.csv') {
    if(file.exists(location)) {
        raw <- data.table(read.csv2(file = location, header = FALSE, sep = ",", as.is = TRUE))
        if(length(names(raw)) != 51) stop("Something about the file structure changed -- you need to re-do the parsing routine.")
        setnames(raw, "V3", 'dt')
        raw[, dt := as.character(as.Date(dt, format = '%m/%d/%Y'))]
        long <- reshape(data = raw[, names(raw)[3:51], with = FALSE], varying = names(raw)[4:51],
                        v.names = 'kwh', direction = 'long', idvar = 'dt')
        if(trunc(nrow(long)/48) != nrow(long)/48) stop("Something is wrong with the reshaped file.")
        setnames(long, 'time', 't2')
        # long[, t2 := `time`]
        long[, hr := (as.numeric(t2)-1)*30/60]
        long[, interval_beginning := paste0(dt, " ", str_pad(as.character(trunc(hr)), width = 2, pad = '0'), ":", 
                                            str_pad(as.character(60 * (hr - trunc(hr))), width = 2, pad = '0'), ":00")]
        if(nrow(long[interval_beginning == '0000-00-00 00:00:00']) > 0) {
            warning("Found bad data = there was a row where the interval was '0000-00-00 00:00:00', please look into further")
        }
        return(long[interval_beginning != '0000-00-00 00:00:00', .(interval_beginning, kwh)])
    } else {
        stop("30 minute KWH file does not exist!")
    }
}

parseDailyElectricityUsage <- function(location = '/tmp/Dominion\ Energy\ __\ Detailed\ Energy\ Usage.htm') {
    if(file.exists(location)) {
        fileCon = file(location)
        raw <- readLines(con = fileCon, n = -1L)
        close(fileCon)
        raw <- gsub("[\\]", "", raw)
        row <- which(as.numeric(regexpr('id="hfMaxTemp30" name="MaxTemp30Days" type="hidden"', raw)) != -1)
        if(length(row) > 1) stop("Problem.  Only expect to find one row with data")
        w1 <- strsplit(raw[row], '<input id=')[[1]]
        w2 <- w1[which(nchar(w1) > 250)]
        dt1 <- data.table(raw = w2)
        dt1[, name := substr(raw, as.numeric(regexpr('hf', raw)) +2, as.numeric(regexpr('[369]0', raw)) - 1)]
        dt1[, values := substr(raw, as.numeric(regexpr('\\[\\[', raw)) + 1, as.numeric(regexpr('\\]\\]', raw)))]
        dt2 <- rbindlist(lapply(1:nrow(dt1), function(rw){
                w3 <- data.table(raw = strsplit(dt1[rw]$values, '\\],\\[')[[1]])
                w3[, raw := gsub("\\[", "", gsub("\\]", "", raw))]
                w3[, `:=` (dt = '1512190800000', val = 50)]
                w3[, val := stringr::str_split_fixed(raw, ',', n = 2)[,2] ]
                w3[, longDt := stringr::str_split_fixed(raw, ',', n = 2)[,1] ]
                w3[, dt := as.character(as.Date('2017-09-05') + (as.numeric(longDt) - 1504584000000)/(24 * 3600 * 1000))]  # uses first date in my series as a reference date
                w3[, type := dt1[rw]$name]
                return(w3[, .(dt, type, val)])
            }))
        return(dt2[, .(kwh = sum(ifelse(type == 'Usage', as.numeric(val), 0)),
                       temp_max = sum(ifelse(type == 'MinTemp', as.numeric(val), 0)),
                       temp_min = sum(ifelse(type == 'MaxTemp', as.numeric(val), 0))), by = dt][order(dt)])
    }
}

insert30MinuteData <- function(input = data.table()) {
    stopifnot(nrow(input) > 0)
    return(dbSqlFromDT(lst.writeConn, input, write.db = 'electricity', write.table = 'usage_30min'))
}

insertDailyElectricityData <- function(input = data.table()) {
    stopifnot(nrow(input) > 0)
    return(dbSqlFromDT(lst.conn = lst.writeConn, DT = input, write.db = 'electricity', write.table = 'usage_daily'))
}
