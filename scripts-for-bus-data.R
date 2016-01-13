library(chron)


interval <- read.delim("intervaldata2014-2015.tsv")
i1 <- subset(interval, (route_id == '354' | route_id == "331"))
i2 <- subset(i1, (to == '67' | to== '68' | to == '1' | to == '12' | to =='6'))
i3 <- subset(i2, to != from)
aroute$Date <- sapply(strsplit(as.character(aroute$when), " "), "[", 1)
aroute$Time <- sapply(strsplit(as.character(aroute$when), " "), "[", 2)
aroute$Time <- chron(times = aroute$Time)
aroute <- subset(aroute, select = -when)
aroute67 <- subset(aroute, to == 67)

sched <- read.csv("DM-ScheduleData.csv")
stadiumS <- subset(sched, sched$Stop == 'Stadium' & Route == 'A1')



stadiumS_times <- c("7:25:00", "7:49:00", "8:20:00", "8:55:00" , "9:47:00", "10:29:00", "11:07:00", "11:57:00", "12:40:00", "13:18:00", "14:06:00", "14:52:00", "15:32:00", "16:08:00", "16:53:00", "17:31:00", "18:15:00", "18:55:00", "19:31:00", "20:07:00", "20:50:00", "21:25:00", "22:00:00", "22:35:00", "23:10:00", "23:45:00", "00:15:00")
stadiumS_times <- chron(times = stadiumS_times)
stadiumStop_data <- data.frame(stadiumS_times)
namevect <- c("sum_of_times", "num_times", "avg_time", "min", "max")
stadiumStop_data[, namevect] <- 0
stadiumStop_data$sum_of_times <- chron(times = stadiumStop_data$sum_of_times)
stadiumStop_data$avg_time <- chron(times = stadiumStop_data$avg_time)
stadiumStop_data$min <- chron(times = stadiumStop_data$min)
stadiumStop_data$max <- chron(times = stadiumStop_data$max)

ssd <- get_avg_stop_time(aroute67, stadiumStop_data)

ssd$avg_time <- ssd$sum_of_times / ssd$num_times
get_avg_stop_time <- function(routeData, stopData) { 
    for(i in 1:nrow(routeData)) { 
        row <- routeData[i,]
        # do stuff with row
        closestTimeRow <- 0
        ifelse(row$Time > "07:00:00" & row$Time <= "7:37:00", closestTimeRow <- 1,
               ifelse(row$Time > "07:37:00" & row$Time <= "8:04:30", closestTimeRow <- 2,
                      ifelse(row$Time > "08:04:30" & row$Time <= "8:37:30", closestTimeRow <- 3,
                             ifelse(row$Time > "08:37:30" & row$Time <= "9:21:00", closestTimeRow <- 4,
                                    ifelse(row$Time > "09:21:00" & row$Time <= "10:08:00", closestTimeRow <- 5,
                                           ifelse(row$Time > "10:08:00" & row$Time <= "10:48:00", closestTimeRow <- 6,
                                                  ifelse(row$Time > "10:48:00" & row$Time <= "11:32:00", closestTimeRow <- 7,
                                                         ifelse(row$Time > "11:32:00" & row$Time <= "12:18:30", closestTimeRow <- 8,
                                                                ifelse(row$Time > "12:18:30" & row$Time <= "12:59:00", closestTimeRow <- 9,
                                                                       ifelse(row$Time > "12:59:00" & row$Time <= "13:42:00", closestTimeRow <- 10,
                                                                              ifelse(row$Time > "13:42:00" & row$Time <= "14:29:00", closestTimeRow <- 11,
                                                                                     ifelse(row$Time > "14:29:00" & row$Time <= "15:12:00", closestTimeRow <- 12,
                                                                                            ifelse(row$Time > "15:12:00" & row$Time <= "15:50:00", closestTimeRow <- 13,
                                                                                                   ifelse(row$Time > "15:50:00" & row$Time <= "16:30:30", closestTimeRow <- 14,
                                                                                                          ifelse(row$Time > "16:30:30" & row$Time <= "17:12:00", closestTimeRow <- 15,
                                                                                                                 ifelse(row$Time > "17:12:00" & row$Time <= "17:53:00", closestTimeRow <- 16,
                                                                                                                        ifelse(row$Time > "17:53:00" & row$Time <= "18:35:00", closestTimeRow <- 17,
                                                                                                                               ifelse(row$Time > "18:35:00" & row$Time <= "19:13:00", closestTimeRow <- 18,
                                                                                                                                      ifelse(row$Time > "19:13:00" & row$Time <= "19:49:00", closestTimeRow <- 19,
                                                                                                                                             ifelse(row$Time > "19:49:00" & row$Time <= "20:28:30", closestTimeRow <- 20,
                                                                                                                                                    ifelse(row$Time > "20:28:30" & row$Time <= "21:07:30", closestTimeRow <- 21,
                                                                                                                                                           ifelse(row$Time > "21:07:30" & row$Time <= "21:42:30", closestTimeRow <- 22,
                                                                                                                                                                  ifelse(row$Time > "21:42:30" & row$Time <= "22:17:30", closestTimeRow <- 23,
                                                                                                                                                                         ifelse(row$Time > "22:17:30" & row$Time <= "22:52:30", closestTimeRow <- 24,
                                                                                                                                                                                ifelse(row$Time > "22:52:30" & row$Time <= "23:27:30", closestTimeRow <- 25,
                                                                                                                                                                                       ifelse(row$Time > "23:27:30" & row$Time <= "23:59:59", closestTimeRow <- 26,
                                                                                                                                                                                              closestTimeRow <- 27))))))))))))))))))))))))))
        
        stopRow <- stopData[closestTimeRow,]
        if (stopRow$num_times == 0) {
            stopRow$min = row$Time
            stopRow$max = row$Time
        }
        if (row$Time < stopRow$min) {stopRow$min = row$Time}
        if (row$Time > stopRow$max) {stopRow$max = row$Time}
        stopRow$sum_of_times <- stopRow$sum_of_times + row$Time
        stopRow$num_times <- stopRow$num_times + 1
        stopData[closestTimeRow,] <- stopRow
    }
    return(stopData)
}
