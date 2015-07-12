# code for Exploratory data analysis course project 1 - Plot 1.

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
## plot1 creates a plot from the data... a red histogram with appropriate labelling.
plot1<-function(){                      ##
    data<-grabData()                                                 ## Grab the data...
    png(filename = "plot1.png", width = 480, height = 480, units = "px") ## open png graphics device...
    par(mfrow=c(1,1))                                                ## set up the graphics device layout
    hist(as.numeric(data[,3]),col="red",main="Global Active Power",  ## plot a histogram with red bars
         xlab="Global Active Power (kilowatts)")                     ## y-axis label is "Global Active Power (kilowatts)
   dev.off()                                                         ## close the png graphics device 
}
##
plot1() ## call the plot function...