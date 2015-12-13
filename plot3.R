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

plot3<-function(dataset=data.frame()){
  if(is.null(dataset) | nrow(dataset)==0){
    print("Reading data from txt...")
    dataset<-getData()
  }
  plot.new()
  png(filename = "plot3.png",width = 480,height = 480,units = "px") 
  with(dataset,plot(x = Time,
                    y = Sub_metering_1,
                    type = "s", 
                    xlab = "",
                    ylab="Energy sub-metering",
                    pch="."))
  with(dataset,points(x = Time,
                      y = Sub_metering_2,
                      type = "s", 
                      col="red",
                      pch="."))
  with(dataset,points(x = Time,
                      y = Sub_metering_3,
                      type = "s", 
                      col="blue",
                      pch="."))
  legend("topright",pch="-",col = c("black","red","blue"),legend = names(dataset)[7:9])
  dev.off()
}