# code for Exploratory data analysis course project 1 - Plot 3.

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
## Sub metering lots three sets of values as line plots of different colours
## The scaling and axes labels are preserved.
plot3<-function(){  
    data<-grabData()   ## grab the data
    png(filename = "plot3.png", width = 480, height = 480, units = "px") ## open png graphics device...
    par(mfrow=c(1,1))  ## set up the graphics device layout
    plot(as.numeric(data[,7]), type="l",col="black",xaxt="n",xlab="",ylab="Energy sub metering") 
    par(new=T)         ## preserve the first plot line when adding the second...
    plot(as.numeric(data[,8]), type="l",col="red",axes=F,ylim=c(0,40),ylab="",xlab="")
    par(new=T)         ## preserve the previous 2 plot lines when adding the third...
    plot(as.numeric(data[,9]), type="l",col="blue",axes=F,ylim=c(0,40),ylab="",xlab="")
    par(new=F)         ## return to default overlay method.
    axis(1,at=c(0,2879/2,2879),labels=c("Thu","Fri","Sat"), col.axis = "black") ## add the lower axis labels
    legend("topright",legend=c("sub_metering_1","sub_metering_2","sub_metering_3"),col=c("black","red","blue"),lty=1,cex=0.8)
    ## add the legend at the top-right, 3 labels with 3 colours matching the plot lines.
    dev.off()   ## close the png graphics device
}
#
plot3()                     ## call the plot function
#