plot4 <- function(){
  
  ## function to plot data from electric consumption
  
  ## check if file exists
  if (!file.exists("./data/household_power_consumption.txt")){
    
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata_data_household_power_consumption.zip"
    download.file(fileUrl, destfile = "./data/exdata_data_household_power_consumption.zip")
    ## need to issue command to unzip but don't know that
  }
  
  ## read in the text file, this is the whole file
  fileName <- "./data/household_power_consumption.txt"
  data <- read.table(fileName, sep=";", header=TRUE, stringsAsFactor = FALSE)
  
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
  
  # set up panel of four plots
  par(mfcol = c(2,2))

  ## upper left plot
  ## now plot this data, with no data symbols
  plot(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append$Global_active_power)/1000.,
       type="n",
       xlab = "",
       ylab ="Global Active Power (kilowatts)")
  
  ## now add the lines, note convert power to kW
  lines(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append$Global_active_power)/1000.)
    
  ## upper left plot
  
  ## columns to plot
  plotCol <- c(7,8,9)
  
  ## now plot this data, with no data symbols
  plot(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append[,plotCol[1]]),
       type="n",
       xlab = "",
       ylab ="Energy sub metering")
  
  ## now add the lines
  lineColor = c("black", "red", "blue")
  for (i in plotCol){  lines(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append[,i]),
                             col = lineColor[i-6])}
  
  ## add legend, probably more clever way to do this inside a loop
  legend("topright", lty = c(1,1,1), 
         col = lineColor, 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         bty="n",
         cex=0.75
  )
  
  plot(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append$Voltage),
       type = "n",
       xlab = "datetime",
       ylab ="Voltage",
       ylim = c(234, 246)
  )

# now add the lines, note convert power to kW
  lines(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append$Voltage))


  plot(dataDatesDF_append$dateStrp,as.numeric(dataDatesDF_append$Global_reactive_power),
     type = "n",
     xlab = "",
     ylab ="Global_reactive_power",
     ylim = c(0, 0.5)
)

# now add the lines, note convert power to kW
  lines(dataDatesDF_append$dateStrp,dataDatesDF_append$Global_reactive_power)
  
## save plot to png file
dev.copy(png,filename="plot4.png")
dev.off()

}