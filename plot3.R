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

plotSubMeterings <- function(data, fileName) {
    sub1 <- data$Sub1[!is.na(data$Sub1)]
    sub2 <- data$Sub2[!is.na(data$Sub2)]
    sub3 <- data$Sub3[!is.na(data$Sub3)]
    
    fullTimeStr <- paste(data$Date, data$Time)
    fullTime <- strptime(fullTimeStr, format="%d/%m/%Y %H:%M:%S")
    
    png(paste(fileName, ".png", sep = ""), width = 480, height = 480)
    
    plot(fullTime, sub1, type = "n", ylab = "Energy sub metering", xlab = "")
    lines(fullTime, y = sub1)
    lines(fullTime, y = sub2, col = "red")
    lines(fullTime, y = sub3, col = "blue")
    legend("topright", col = c("black", "red", "blue"), lty = c(1, 1, 1),
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.off()
}


if(!file.exists("household_power_consumption.txt")) {
    ensureDataDownloaded("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
}

plotSubMeterings(readData(), "plot3")