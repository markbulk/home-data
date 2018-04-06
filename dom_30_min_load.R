source('/home/markbulkeley/r_scripts/homeServerUtils.R')
source('/home/markbulkeley/r_scripts/dominion.R')

DT <- parse30MinuteData()
complete <- insert30MinuteData(input = DT)


# DT <- parse30MinuteData(location = '/tmp/history-kwh_2017-12-29.csv')
# DT <- parse30MinuteData(location = '/home/markbulkeley/history-kwh_2018-03-31.csv')