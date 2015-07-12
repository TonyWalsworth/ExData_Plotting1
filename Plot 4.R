# code for Exploratory data analysis course project 1 - Plot 4.
# It is assumed that the required data file is present in the current working directory.
##
# grabData() reads a selected subset of household_power_consumption.txt, (lines 66639 to 69518)
# Each line is a single string containing 9 variable values delimited by ";" 
# Thus each line must be split (using strsplit() at the ";") to provide seperate elements
# After splitting, the resulting 9 element vector is added to the empty character vector
# After all the lines have been split the resulting data set, named "a" contains 2879 Obs of 9 variables
# Finally, column names are added (using grabLabels() defined below), and the data set is returned.
#
##
grabData<-function(){
    a<-character()         ## create an empty character vector
    readcon<- file('household_power_consumption.txt') ## create a connection to the data file
    open(readcon)                                     ## open the connection
    data<-read.table(readcon, skip=66637, nrow=2879)  ## read file contents, 2879 lines from 66638
    data<-as.character(data[[1]])                     ## ensure it's all as character
    for(i in 1:length(data)){                         ## for each line in the data... 
        b<-strsplit(data[i],";")                      ## split it into elements using ";" as a delimiter
        a<-rbind(a,b[[1]])                            ## bind the resulting elements as a row to "a"
    }
    close(readcon)                                    ## close the file connection
    colnames(a)<-grabLabels()                         ## get the labels as column names
    a                                                 ## return this as a completed data set
}
##
##
# GrabLabels() reads the first line of household_power_consumption.txt which contains the
# variable names (the column names), of the data set.
# The line is a single string with the variable names delimited by ";" which must be split 
# into 9 variable names and returned as a character vector
##
grabLabels<-function(){
    con<-file('household_power_consumption.txt')             ## connect to the data file
    labels<-readLines(con, n=1)                              ## read the first line
    close(con)                                               ## close the connection
    labels<-strsplit(labels,";")            ## split the line into column names (";" is the delimeter)
    datalabels<-labels[[1]]                 ## take the first element of labels (which is the entire list)
    datalabels                              ## return this
}
#
# plot4() plots 4 individual graphs in a 2 x 2 layout. Each plot has differing axes, scale, values and colours
#
plot4<-function(){
    data<-grabData()                        ## grab the data...
    png(filename = "plot4.png", width = 480, height = 480, units = "px") ## open png graphics device...
    par(mfcol=c(2,2))                       ## set up the graphics device layout
    topLeft<-{                              ## 1st plot - top left of grid
        plot(as.numeric(data[,3]), type="l",col="black",xlab="",xaxt="n", ## plot data column 3
             ylab="Global Active Power (kilowatts)")                      
        axis(1,at=c(0,2879/2,2879),labels=c("Thu","Fri","Sat"), col.axis = "black") ## add appropriate axis labels
    }
    bottomLeft<-{        ## Bottom - left plot has 3 series and a legend
        plot(as.numeric(data[,7]), type="l",col="black",xaxt="n",xlab="",ylab="Energy sub metering") 
        par(new=T)       ## preserve the first series when plotting the second...
        plot(as.numeric(data[,8]), type="l",col="red",axes=F,ylim=c(0,40),ylab="",xlab="")
        par(new=T)       ## preserve the previous series' when plotting the third
        plot(as.numeric(data[,9]), type="l",col="blue",axes=F,ylim=c(0,40),ylab="",xlab="")
        par(new=F)
        axis(1,at=c(0,2879/2,2879),labels=c("Thu","Fri","Sat"), col.axis = "black") ## Add the axis labels
        legend("topright",legend=c("sub_metering_1","sub_metering_2","sub_metering_3"),col=c("black","red","blue"),lty=1,cex=0.8)
         ## add the legend.
    }   
    topRight<-{ ## Top-Right plot is a single series plotted as a black line
        plot(as.numeric(data[,5]), type="l",col="black",xlab="datetime",xaxt="n", ## plot the 5th column
             ylab="Voltage") 
        axis(1,at=c(0,2879/2,2879),labels=c("Thu","Fri","Sat"), col.axis = "black") ## add the axis labels
    }
    bottomRight<-{ ## Bottom-Right is a single series plotted as a black line
        plot(as.numeric(data[,4]), type="l",col="black",xlab="datetime",xaxt="n",
             ylab="global_reactive_power") ## plot data column 4
        axis(1,at=c(0,2879/2,2879),labels=c("Thu","Fri","Sat"), col.axis = "black") ## add the axis labels
    }
    dev.off() ## close the png graphics device
}
#
plot4()                     ## call the plot function
#
