## The following R code takes in a filename as an input and generates the plots required by the Coursera Assignment.
## By Arun Rameshbabu a3909040@opayq.com
plot1 <- function(filename){
  data <- read.csv2("./household_power_consumption.txt", na.strings = "?", colClasses = "character") ## Read in the data taking into account the special NA character.
  
  ## Now transform each element from the character stuff (basically de-factorizing)
  temp <- paste(data$Date, data$Time)
  temp <- strptime(temp, "%Y-%m-%d %H:%M:%S")
  data$Global_active_power <- as.numeric(data$Global_active_power)
  data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
  data$Voltage <- as.numeric(data$Voltage)
  data$Global_intensity <- as.numeric(data$Global_intensity)
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  
  ## Now extract only the subset of the data where the plot is required
  plotData <- data[(data$Time >= "2007-02-01 00:00:00" & data$Time <= "2007-02-02 23:59:59"),]
  
  ## Plot functions
  ## Create the screen device for the fourth plot
  png(filename = "Plot4.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE)
  par(mfrow = c(2,2))
  hist(plotData$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
  plot(plotData$Time, plotData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
  plot(plotData$Time, plotData$Sub_metering_1, ylim = yrange, type = "l", xlab = "", ylab = "")
  par(new=T)
  plot(plotData$Time, plotData$Sub_metering_2, ylim = yrange, type = "l", xlab = "", ylab = "", col = "red")
  par(new=T)
  plot(plotData$Time, plotData$Sub_metering_3, ylim = yrange, type = "l", xlab = "", ylab = "Energy sub metering", col = "blue")
  plot(plotData$Time, plotData$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
  
  ## Saved plots will be uploaded. Assignment complete.
}