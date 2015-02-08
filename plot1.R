plot1 <- function(){
  
  ## function to plot data from electric consumption
  
  ## check if file exists
  if (!file.exists("./data/household_power_consumption.txt")){
    
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata_data_household_power_consumption.zip"
    download.file(fileUrl, destfile = "./data/exdata_data_household_power_consumption.zip")
    ## need to issue command to unzip but don't know that
  }
  
  ## read in the text file, this is the whole file
  fileName <- "./data/household_power_consumption.txt"
  data <- read.table(fileName, sep=";", header=TRUE)
  
  ## extract only the dates needed
  ## this using a sql query to filter as described in class forums
  ##dataDates <- read.csv.sql("household_power_consumption.txt",header=TRUE,sep=";",
  ##                     sql="Select * from file where Date = '1/1/2007' OR Date = '2/1/2007'")
  
  dataDates <- data[data$Date %in% c("1/2/2007", "2/2/2007"),]
  
  ## convert to data frame (not sure if this is necessary)
  dataDatesDF <- as.data.frame(dataDates)
  
  ## convert date/time to POSIX
  dateStrp <- strptime(paste(dataDatesDF$Date,dataDatesDF$Time),
                       "%d/%m/%Y %H:%M:%S")
  
  ## paste the new data/time object into the end of the existing data
  dataDatesDF_append <- cbind(dataDatesDF, dateStrp)

  ## plot histogram of active power data
  
  hist(as.numeric(dataDatesDF_append$Global_active_power)/1000.,
                   main="Global Active Power",
                   xlab="Global Active Power (kilowatts)",
                   col = "Red",
                   xlim = c(0,6))

  ## save plot to png file
  dev.copy(png,filename="plot1.png")
  dev.off()
}