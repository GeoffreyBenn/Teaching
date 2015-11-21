#This script was created to allow easy visualization of average assignment (i.e. pre and post-labs) scores
#by section, using a smartsite gradebook as the starting point.
#to run the script, follow the instructions in the associated word doc, then
#replace the file name in the line below with the correct file. Then hightlight
#the whole script and run it in R.
data <- read.delim("3.13.15.txt",header=T)

#this line converts missing values to 0s
data[is.na(data)] <- 0
library(reshape2)

#this section calculates classwide averages
data2 <- data[,-1]
average <- c("Average",colMeans(data2))

#this section combines all of the scores into a single column
#for calculation of averages by section
data$id = rownames(data)
data2 <- melt(data)
means <- aggregate(value~Section+variable,data2,mean)

#this converts the data back into wide format (column for each grade)
result <- dcast(means,Section ~ variable)

#combined the class averages with the section averages
result <- rbind(result,average)

#create output file
write.table(result,"Grades by section.txt",sep="\t", row.names = FALSE)
