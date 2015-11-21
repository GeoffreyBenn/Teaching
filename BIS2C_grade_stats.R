#########################
#To run this script, open it in the RGui, then change the directory to
#the folder where the MGS is located. Make sure major_codes.txt is also
#in that same folder. Then change the file name and test name below:
 
#change the file name on the next line:
data <- read.delim("MGS.6.11.15.txt",header=T)

#specify the name of the grade item you want to analyze (must be exactly the same as in the MGS):
var <- "MT1.Total"

#Now highlight the entire script and press the "Run selection" button
#########################
library(ggplot2)
library(gridExtra)
library(grid)

#########################
#The following section produces the general stats file
#########################

####
#this section contains past exam scores:
Quarter <- c('su15','sp15','w15','f14','su14','sp14')
MT1.Total <- c(90.19,79.79,89.45,79.43,79.14,86.78)
MT2.Total <- c(78.34,73.56,81.72,76.37,79.14,71.51)
Practical.Total <- c(81.31,78.93,77.89,83.02,80.26,75.18)
Final.Total <- c(80.26,87.79,80.00,81.05,72.97,73.05)
past.exams <- data.frame(Quarter,MT1.Total,MT2.Total,Practical.Total,Final.Total,stringsAsFactors = F)
my.vars <- c("Quarter",var)
history <- past.exams[my.vars]
length1 <- length(data[[var]])
####

####
#this section deletes 0s:
data[data==0] <- NA
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
data <- completeFun(data,var)
length2 <- length(data[[var]])
elim <- length1 - length2
####

####
#this section prodcues the visuals:
mean <- round(mean(data[[var]]),digits=2)
min <- min(data[[var]])
max <- max(data[[var]])
data.a <- subset(data,group=="A")
data.b <- subset(data,group=="B")
mean.a <- round(mean(data.a[[var]]),digits=2)
mean.b <- round(mean(data.b[[var]]),digits=2)
min.a <-min(data.a[[var]])
min.b <-min(data.b[[var]])
max.a <-max(data.a[[var]])
max.b <-max(data.b[[var]])
Group <- c("Combined", "A", "B")
Mean <- c(mean,mean.a,mean.b)
Min <- c(min,min.a,min.b)
Max <- c(max,max.a,max.b)
summary <- data.frame(Group,Mean,Min,Max)
table1 <- tableGrob(summary,rows=NULL)
history <- rbind(c('Current',mean),history)
table2 <- tableGrob(history, rows=NULL)
Text <- grid.text(paste("Means calculated with NAs and 0s removed.\n",elim," 0s or NAs were removed for ",var,".",sep=""))

d <- ggplot(data, aes(get(var), fill=group)) + geom_density(alpha=.5)
d <- d + labs(x ="")

blankPanel <- grid.rect(gp=gpar(col="white"))

pdf("BIS2C basic exam analysis.pdf",paper="a4")
grid.arrange(table1,table2,d,Text,ncol=2,top=paste("Statistics for",var,sep=" "))
dev.off()

######################
#The following section produces the detailed stats (by major and class)
######################

data <- transform(data,
       major = ordered(major, levels = names( sort(-table(major)))))

e <- ggplot(data, aes(factor(major),get(var))) + geom_boxplot()
e <- e + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10),name=var)
e <- e + scale_x_discrete(name="Major (sorted by # of students per major)")
e

data <- transform(data,
       class = ordered(class, levels = c("FR","SO","JR","SR")))


f <- ggplot(data=data, aes(factor(class),get(var))) + geom_boxplot() + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10), name=var)
f <- f + scale_x_discrete(name = "Class")
f

pdf("BIS2C exam analysis by major and class.pdf",paper="a4")
grid.arrange(e,f,ncol=1,top=paste("Detailed statistics for",var,sep=" "))
dev.off()

#######################
#The following section produces the demographics file
#######################
major.codes <-read.delim("major_codes.txt",header=T)   
major.table <- xtabs(~major+class, data=data)
majors1 <- as.data.frame.matrix(major.table)
majors2 <- cbind(major = rownames(majors1),majors1)
rownames(majors2) <- NULL
majors <- merge(x=major.codes,y=majors2,by.x="major",by.y="major",setall.y=TRUE)
majors$sum <- majors$FR + majors$SO + majors$JR + majors$SR
majors$sum <- as.numeric(majors$sum)
ndx = order(majors$sum, decreasing=T)
majors_sort = majors[ndx,]
majors.print <- tableGrob(majors_sort,rows=NULL)
blankPanel <- grid.rect(gp=gpar(col="white"))
pdf("BIS2C demographic table.pdf",height = 20,width=8.5)
grid.arrange(majors.print,ncol=1,top=paste("Demographics for BIS2C current students"))
dev.off()

#######################
#The following section produces the stats by TA file
#######################

g <- ggplot(data=data, aes(factor(TA),get(var))) + geom_boxplot()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10),name=var)
g <- g + scale_x_discrete(name = "TA")
pdf("BIS2C exam stats by TA.pdf",paper="a4")
grid.arrange(g,ncol=1,top=paste(var,"stats by TA",sep=" "))
dev.off()


