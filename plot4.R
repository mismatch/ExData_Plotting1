ensureDataDownloaded <- function(url) {
    localPath <- "power_consumption.zip"
    download.file(url, destfile = localPath, method = "curl")
    unzip(localPath)
    file.remove(localPath)
}

readData <- function() {
    data <- read.csv2("household_power_consumption.txt", skip = 66637, 
                      header = F, na.strings = "?", dec = ".", 
                      col.names = c("Date", "Time", "AP", "RP", "Voltage", "Intensity", "Sub1", "Sub2", "Sub3"),
                      colClasses = c("character", "character", rep("numeric", 7)))
    actualData <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007", ]
    rm(data)
    actualData
}

plotAllToFile <- function(data, fileName) {
    png(paste(fileName, ".png", sep = ""), width = 480, height = 480)
    par(mfrow = c(2, 2))
    
    plot1(data)
    plot2(data)
    plot3(data)
    plot4(data)
    
    dev.off()
}

plot1 <- function(data) {
    ap <- data$AP[!is.na(data$AP)]
    fullTimeStr <- paste(data$Date, data$Time)
    fullTime <- strptime(fullTimeStr, format="%d/%m/%Y %H:%M:%S")
    
    plot(fullTime, ap, type = "l", xlab = "", ylab = "Global Active Power")
}

plot2 <- function(data) {
    volt <- data$Voltage[!is.na(data$Voltage)]
    fullTimeStr <- paste(data$Date, data$Time)
    fullTime <- strptime(fullTimeStr, format="%d/%m/%Y %H:%M:%S")
    
    plot(fullTime, volt, type = "l", xlab = "datetime", ylab = "Voltage")
}

plot3 <- function(data) {
    sub1 <- data$Sub1[!is.na(data$Sub1)]
    sub2 <- data$Sub2[!is.na(data$Sub2)]
    sub3 <- data$Sub3[!is.na(data$Sub3)]
    
    fullTimeStr <- paste(data$Date, data$Time)
    fullTime <- strptime(fullTimeStr, format="%d/%m/%Y %H:%M:%S")
    
    plot(fullTime, sub1, type = "n", ylab = "Energy sub metering", xlab = "")
    lines(fullTime, y = sub1)
    lines(fullTime, y = sub2, col = "red")
    lines(fullTime, y = sub3, col = "blue")
    legend("topright", col = c("black", "red", "blue"), lty = c(1, 1, 1), bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

plot4 <- function(data) {
    rp <- data$RP[!is.na(data$RP)]
    fullTimeStr <- paste(data$Date, data$Time)
    fullTime <- strptime(fullTimeStr, format="%d/%m/%Y %H:%M:%S")
    
    plot(fullTime, rp, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
}


if(!file.exists("household_power_consumption.txt")) {
    ensureDataDownloaded("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
}
plotAllToFile(readData(), "plot4")
