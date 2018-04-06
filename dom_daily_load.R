# This is intended to be run on the server directly, via command line R

source('/home/markbulkeley/r_scripts/homeServerUtils.R')
source('/home/markbulkeley/r_scripts/dominion.R')

DT <- parseDailyElectricityUsage()
complete <- insertDailyElectricityData(input = DT)


# DT <- parseDailyElectricityUsage(location = '/tmp/Dominion\ Energy\ __\ Detailed\ Energy\ Usage\ 2017-12-29.htm')
# DT <- parseDailyElectricityUsage(location = '/home/markbulkeley/Dominion\ Energy\ __\ Detailed\ Energy\ Usage\ 2018-03-31.htm')
# complete <- insertDailyElectricityData(input = DT)


hackintosh:~ markbulk$ scp Downloads/Dominion\ Energy\ __\ Detailed\ Energy\ Usage\ 2018-03-31.htm markbulkeley@192.168.1.60:/home/markbulkeley/.