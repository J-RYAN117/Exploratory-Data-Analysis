getData <- function(t1="2007-02-01",t2="2007-02-02"){
  dataset<-read.table(file="household_power_consumption.txt", 
                      header=TRUE, 
                      sep=";", 
                      stringsAsFactors = FALSE, 
                      na.strings = "?")
  
  dataset$Time<-strptime(paste0(dataset$Date, " ", dataset$Time),"%d/%m/%Y %H:%M:%S")
  dataset$Date<-as.Date(dataset$Date,"%d/%m/%Y")
  dataset<-subset(dataset,Date==as.Date(t1) | Date==as.Date(t2))
  dataset
}

plot2<-function(dataset=data.frame()){
  if(is.null(dataset) | nrow(dataset)==0){
    print("Reading data from txt...")
    dataset<-getData()
  }
  plot.new()
  png(filename = "plot2.png",width = 480,height = 480,units = "px")
  with(dataset,plot(x = Time,
                    y = Global_active_power,
                    type = "s", 
                    xlab = "",
                    ylab="Global Active Power (kilowatts)",
                    pch=".")
  )
  dev.off()
}