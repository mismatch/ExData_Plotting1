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

plotHistogram <- function(activePowerData, fileName) {
    data <- activePowerData[!is.na(activePowerData)]
    hist(data, main = "Global Active Power", col = "red", 
         xlab = "Global Active Power (kilowatts)")
    dev.copy(png, file = paste(fileName, ".png", sep = ""))
    dev.off()
}


if(!file.exists("household_power_consumption.txt")) {
    ensureDataDownloaded("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
}

data <- readData()
plotHistogram(data$AP, "plot1")
