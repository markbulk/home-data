library(RMySQL)  # install.packages('RMySQL', '/usr/local/lib/R/site-library')
library(glue)
library(curl)
library(rjson)
library(gmailr)
library(data.table)
library(DT)
library(lubridate)
library(knitr); options(knitr.table.format = "html")

# CREATE USER 'reader'@'*' IDENTIFIED BY 'stone819LEIGH22066reader';
# GRANT SELECT ON *.* TO 'reader'@'*'
lst.cred <- list(rootUser = 'root', rootPassword = 'photopro', readUser = 'reader', readPassword = 'stone819LEIGH22066reader', writeUser = 'writer', writePassword = 'write4096Stone819Leigh')
lst.readConn <- dbConnect(drv = MySQL(), user = lst.cred[["readUser"]], password = lst.cred[['readPassword']], host = '192.168.1.60', port = 3306)
lst.writeConn <- dbConnect(drv = MySQL(), user = lst.cred[["writeUser"]], password = lst.cred[['writePassword']], host = '192.168.1.60', port = 3306)
# If you run on the server directly, use the below
# lst.readConn <- dbConnect(drv = MySQL(), user = lst.cred[["readUser"]], password = lst.cred[['readPassword']], host = 'localhost', port = 3306)
# lst.writeConn <- dbConnect(drv = MySQL(), user = lst.cred[["writeUser"]], password = lst.cred[['writePassword']], host = 'localhost', port = 3306)

# simple function to shorten the writing of complex queries to R from a data.table
dbSqlFromDT <- function(lst.conn = lst.writeConn, DT = data.table(), write.db = NULL, write.table = NULL, server = "192.168.1.60", port = 3306) {
    stopifnot(nrow(DT) > 0)
    stopifnot(all(!is.null(c(write.db, write.table))))
    # check that the table exists
    chr.checkQuery <- paste0("SELECT * 
                            FROM information_schema.tables
                            WHERE table_schema = '", write.db, "'
                            AND table_name = '", write.table, "'
                            LIMIT 1")
    rsltExists <- data.table(dbGetQuery(conn = lst.conn, statement = chr.checkQuery))
    if(nrow(rsltExists) == 0) stop("Table doesn't exist, dummy!")
    # check columns are matching
    insertNames <- names(DT)
    chr.columnNamesQuery <- paste0("SHOW COLUMNS FROM ", write.db, ".", write.table)
    tableNames <- data.table(dbGetQuery(conn = lst.conn, statement = chr.columnNamesQuery))
    # if columns match
    if(all(sort(insertNames) == sort(tableNames$Field))) {
        lst.localWriteConnection <- dbConnect(drv = MySQL(), user = lst.cred[["writeUser"]], 
                                              password = lst.cred[["writePassword"]], host = 'localhost',
                                              dbname = write.db)
        rslt <- dbWriteTable(conn = lst.localWriteConnection, name = write.table, value = DT[, insertNames, with = FALSE], append = TRUE, row.names = FALSE)
        return(rslt)
    } else {
        stop("Column names don't all match.  There must be a problem.")
    }
}

# Returns a data.table from a StoneLeigh MySQL database
dq <- function(chr.query = NULL, lst.conn = lst.readConn) {
    stopifnot(!is.null(chr.query))
    return(data.table(dbGetQuery(conn = lst.conn, statement = chr.query)))
}

## Database creation in home
# CREATE TABLE `home`.`sensors` (
#     `sensor_id` tinyint(4) NOT NULL AUTO_INCREMENT,
#     `sensor_name` varchar(50) NOT NULL,
#     `sensor_mac` char(12) NOT NULL,
#     `sensor_ip` varchar(15) NOT NULL,
#     `sensor_port` mediumint(9) DEFAULT NULL,
#     `floor` enum('First','Second','Ground') DEFAULT NULL,
#     PRIMARY KEY (`sensor_id`)
# );
# 
# CREATE TABLE `home`.`observations` (
#     `id` int(11) NOT NULL AUTO_INCREMENT,
#     `sensor` tinyint(4) NOT NULL,
#     `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
#     `temp` decimal(5,2) NOT NULL,
#     `humidity` decimal(4,2) NOT NULL,
#     `pressure` decimal(6,2) NOT NULL,
#     PRIMARY KEY (`id`),
#     KEY `sensor` (`sensor`),
#     CONSTRAINT `observations_ibfk_1` FOREIGN KEY (`sensor`) REFERENCES `sensors` (`sensor_id`)
# );
# 
# CREATE TABLE `home`.`weather` (
#     `id` int(11) NOT NULL AUTO_INCREMENT,
#     `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
#     `observation_time` datetime NOT NULL,
#     `temp` decimal(5,2) NOT NULL,
#     `humidity` tinyint(4) NOT NULL,
#     `pressure` smallint(6) NOT NULL,
#     `wind_degrees` smallint(6) NOT NULL,
#     `wind_mph` decimal(5,2) NOT NULL,
#     `wind_gust_mph` decimal(5,2) NOT NULL,
#     `solarradiation` decimal(6,2) NOT NULL,
#     `precip_1hr_in` decimal(4,2) NOT NULL,
#     PRIMARY KEY (`id`)
# );
# 
# CREATE TABLE `home`.`nests` (
#     `nest_id` tinyint(4) NOT NULL AUTO_INCREMENT,
#     `nest_name` varchar(11) NOT NULL,
#     PRIMARY KEY (`nest_id`)
# );
# 
# CREATE TABLE `home`.`nest_polls` (
#     `id` int(11) NOT NULL AUTO_INCREMENT,
#     `time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
#     `nest` tinyint(4) NOT NULL,
#     `fan` tinyint(1) NOT NULL,
#     `temp` smallint(6) NOT NULL,
#     `humidity` tinyint(4) NOT NULL,
#     `target_low` tinyint(4) NOT NULL,
#     `target_high` tinyint(4) NOT NULL,
#     `eco_heat` tinyint(4) NOT NULL,
#     `eco_cool` tinyint(4) NOT NULL,
#     PRIMARY KEY (`id`),
#     KEY `nest` (`nest`),
#     CONSTRAINT `nest_polls_ibfk_1` FOREIGN KEY (`nest`) REFERENCES `nests` (`nest_id`)
# );

# chr.query <- "SELECT \"deviceName\", \"value\" FROM \"temperature\" WHERE \"deviceName\"='Garage Sensor' LIMIT 10"
influxQuery <- function(chr.query = NULL) {
    stopifnot(!is.null(chr.query))
    rslt <- curl_fetch_memory(url = paste0('http://192.168.1.60:8086/query?', URLencode("db=SmartThings"), '&', URLencode(paste0("q=", chr.query))))
    lst.json <- fromJSON(rawToChar(rslt$content))
    if("error" %in% names(lst.json$results[[1]])) {
        warning("Query failed")
        return(FALSE)
    } else if(!("series" %in% names(lst.json$results[[1]]))) {
        warning("No query results")
        return(FALSE)
    } else {
        DT <- rbindlist(lst.json[["results"]][[1]][["series"]][[1]][['values']])
        # DT <- data.table(as.data.frame(lst.json[["results"]][[1]][["series"]][[1]][['values']]))
        setnames(DT, lst.json[["results"]][[1]][["series"]][[1]]$columns)
        return(DT)        
    }
}

# This function sends email from the bulkeley@gmail.com email address and relies on their being a file called .httr-oauth
# found in the users director.  Note that there are issues when running as SuperUser, despite root having this file, it will
# still as for authentication, which cannot be done in a shell.
# 
# Input: are all obvious email parts, including the to, subj and body of the message.  
#     bln.html allows the user to choose whether the output will be converted to html or not.
# 
# Output: an email successfully sent via gmail!
# 
# Written by Mark on 2018-03-26 (and tested successfully the next morning)
# 
## stoneleighserver-email project; "StoneLeighServer Alerts"  Consent shown to user
# testing commands
# to = 'markbulk@gmail.com'
# subj = "Testing"
# body = paste0("This is a test body on ", Sys.Date())
sendStoneLeighAlert <- function(to = 'markbulk@gmail.com', subj = "The subject is missing", body = paste0("Body missing on ", Sys.Date()), bln.html = FALSE) {
    # gmail password: oetfmqzioxpomqme
    use_secret_file('/home/markbulkeley/stoneleighserver-email.json')
    msg = mime() %>%
        from("bulkeley@gmail.com") %>%
        to(to) %>%
        subject(subj)
    if(bln.html) {
        msg = msg %>% html_body(body)
    } else {
        msg = msg %>% text_body(body)
    }
    send_message(msg)
}


theme_bulkeley <- function() {
    theme(
        axis.text =    element_text(size = rel(2)), 
        axis.text.x =  element_text(size = rel(1)),    ## axis.text.x =  element_text(size = rel(1), angle = -90, hjust = 0), 
        axis.text.y =  element_text(size = rel(1)), 
        axis.title.x = element_text(size = rel(2)), 
        axis.title.y = element_text(size = rel(2)), 
        
        legend.text =  element_text(size = rel(2)), 
        legend.title = element_text(size = rel(2)), 
        
        strip.background = element_rect(fill = "grey80"), 
        strip.text.x = element_text(size = rel(2)), 
        strip.text.y = element_text(size = rel(2)), 
        
        plot.background = element_rect(colour = "white"),
        
        plot.title = element_text(lineheight=.8, face="bold", size = rel(2))
    )
}