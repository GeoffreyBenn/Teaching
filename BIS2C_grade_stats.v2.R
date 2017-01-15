################################################################################
#                              BIS 2 Grade Stats                               #
#                                     v.2.0                                    #
################################################################################

###the scripts below use headers specified by the perl script 'roster.parse.pl'
#headers in the MGS must be exact for the scripts to run correctly

#Libraries and packages
require(ggplot2)
require(RColorBrewer)
require(grid)
require(gridExtra)
require(stats)

#set the working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/2A_Fall2016_Analysis/BIS_2A_Fall16")

#indicate the output file name from the 'roster.parse.v4.pl' script on the next line:
data <- read.delim("BIS_2A_F16_A_B_D.txt",header=T)

#enrollment by class standing
#provides a bar plot of the number of currently enrolled freshmen, sophmores, juniors, and seniors
pdf("001 Enrollment by Class Standing.pdf",paper="a4")
class <- table(data$class)
barplot(class[order(class)],
        horiz = TRUE,
        las = 1,
        xlim = c(0, 800),
        border = NA,
        col = brewer.pal(4, "BrBG"),
        main = "Enrollment by Class Standing",
        xlab= "Number of Students")
dev.off()

#class demographics
#provides a list of enrolled majors, sorted by number and class standing
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
pdf("002 Class Demographics.pdf",height = 25,width=8.5)
grid.arrange(majors.print,ncol=1,top=paste("Demographics for current students"))
dev.off()

###############
#Midterm 1
###############

#specify the exam by the column in the MGS
var <- "MT1.Total"
term <- "F16"

#########################
#The following section produces the general stats file
#########################

####
#this section contains past exam scores:
Quarter <- c('f15','w16','s16','su16')
MT1.Total <- c(75,75,75,75)
MT2.Total <- c(75,75,75,75)
Final.Total <- c(75,75,75,75)
past.exams <- data.frame(Quarter,MT1.Total,MT2.Total,Final.Total,stringsAsFactors = F)
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
#data.c <- subset(data,group=="C")
data.d <- subset(data,group=="D")

mean.a <- round(mean(data.a[[var]]),digits=2)
mean.b <- round(mean(data.b[[var]]),digits=2)
#mean.c <- round(mean(data.c[[var]]),digits=2)
mean.d <- round(mean(data.d[[var]]),digits=2)

min.a <-min(data.a[[var]])
min.b <-min(data.b[[var]])
#min.c <-min(data.c[[var]])
min.d <-min(data.d[[var]])

max.a <-max(data.a[[var]])
max.b <-max(data.b[[var]])
#max.c <-max(data.c[[var]])
max.d <-max(data.d[[var]])

Group <- c("Combined", "A", "B", "D")
Mean <- c(mean,mean.a,mean.b, mean.d)
Min <- c(min,min.a,min.b, min.d)
Max <- c(max,max.a,max.b, max.d)
summary <- data.frame(Group,Mean,Min,Max)
table1 <- tableGrob(summary,rows=NULL)
history <- rbind(c('Current',mean),history)
table2 <- tableGrob(history, rows=NULL)
Text <- grid.text(paste("Means calculated with NAs and 0s removed.\n",elim," 0s or NAs were removed for ",var,".",sep=""))

d <- ggplot(data, aes(get(var), fill=group)) + geom_density(alpha=.5)
d <- d + labs(x ="")

blankPanel <- grid.rect(gp=gpar(col="white"))

pdf("003 Midterm 1 Analysis.pdf",paper="a4")
grid.arrange(table1,table2,d,Text,ncol=2,top=paste("Statistics for",var,sep=" "))
dev.off()

#performance by major
pdf("004 Midterm 1 Analysis by major.pdf",paper="a4")
data <- transform(data,
       major = ordered(major, levels = names( sort(-table(major)))))
e <- ggplot(data, aes(factor(major),get(var))) + geom_boxplot()
e <- e + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10),name=var)
e <- e + scale_x_discrete(name="Major (sorted by # of students per major)")
e
dev.off()

#performance by class standing
pdf("005 Midterm 1 Analysis by class standing.pdf",paper="a4")
data <- transform(data,
       class = ordered(class, levels = c("FR","SO","JR","SR")))
f <- ggplot(data=data, aes(factor(class),get(var))) + geom_boxplot() + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10), name=var)
f <- f + scale_x_discrete(name = "Class")
f
dev.off()

#performance by TA
pdf("006 Midterm 1 stats by TA.pdf",paper="a4")
g <- ggplot(data=data, aes(factor(TA),get(var))) + geom_boxplot()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,100), breaks=seq(0,100,10),name=var)
g <- g + scale_x_discrete(name = "TA")
g
dev.off()

###############
#Midterm 2
###############

#Midterm 2 Performance
var <- "MT2.Total"
term <- "F16"

#this section contains past exam scores:
Quarter <- c('f15','w16','s16','su16')
MT1.Total <- c(75,75,75,75)
MT2.Total <- c(75,75,75,75)
Final.Total <- c(75,75,75,75)
past.exams <- data.frame(Quarter,MT1.Total,MT2.Total,Final.Total,stringsAsFactors = F)
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
#data.c <- subset(data,group=="C")
data.d <- subset(data,group=="D")

mean.a <- round(mean(data.a[[var]]),digits=2)
mean.b <- round(mean(data.b[[var]]),digits=2)
#mean.c <- round(mean(data.c[[var]]),digits=2)
mean.d <- round(mean(data.d[[var]]),digits=2)

min.a <-min(data.a[[var]])
min.b <-min(data.b[[var]])
#min.c <-min(data.c[[var]])
min.d <-min(data.d[[var]])

max.a <-max(data.a[[var]])
max.b <-max(data.b[[var]])
#max.c <-max(data.c[[var]])
max.d <-max(data.d[[var]])

Group <- c("Combined", "A", "B", "D")
Mean <- c(mean,mean.a,mean.b, mean.d)
Min <- c(min,min.a,min.b, min.d)
Max <- c(max,max.a,max.b, min.d)
summary <- data.frame(Group,Mean,Min,Max)
table1 <- tableGrob(summary,rows=NULL)
history <- rbind(c('Current',mean),history)
table2 <- tableGrob(history, rows=NULL)
Text <- grid.text(paste("Means calculated with NAs and 0s removed.\n",elim," 0s or NAs were removed for ",var,".",sep=""))

d <- ggplot(data, aes(get(var), fill=group)) + geom_density(alpha=.5)
d <- d + labs(x ="")

blankPanel <- grid.rect(gp=gpar(col="white"))

pdf("007 Midterm 2 Analysis.pdf",paper="a4")
grid.arrange(table1,table2,d,Text,ncol=2,top=paste("Statistics for",var,sep=" "))
dev.off()

#performance by major
pdf("008 Midterm 2 Analysis by major.pdf",paper="a4")
data <- transform(data,
                  major = ordered(major, levels = names( sort(-table(major)))))
e <- ggplot(data, aes(factor(major),get(var))) + geom_boxplot()
e <- e + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10),name=var)
e <- e + scale_x_discrete(name="Major (sorted by # of students per major)")
e
dev.off()

#performance by class standing
pdf("009 Midterm 2 Analysis by class standing.pdf",paper="a4")
data <- transform(data,
                  class = ordered(class, levels = c("FR","SO","JR","SR")))
f <- ggplot(data=data, aes(factor(class),get(var))) + geom_boxplot() + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10), name=var)
f <- f + scale_x_discrete(name = "Class")
f
dev.off()

#performance by TA
pdf("010 Midterm 2 stats by TA.pdf",paper="a4")
g <- ggplot(data=data, aes(factor(TA),get(var))) + geom_boxplot()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10),name=var)
g <- g + scale_x_discrete(name = "TA")
g
dev.off()

###############
#Midterm 3
###############

##Midterm 3 Performance
var <- "MT3.Total"
term <- "F16"

#this section contains past exam scores:
Quarter <- c('f15','w16','s16','su16')
MT1.Total <- c(75,75,75,75)
MT2.Total <- c(75,75,75,75)
MT3.Total <- c(75,75,75,75)
Final.Total <- c(75,75,75,75)
past.exams <- data.frame(Quarter,MT1.Total,MT2.Total,MT3.Total,Final.Total,stringsAsFactors = F)
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
#data.c <- subset(data,group=="C")
data.d <- subset(data,group=="D")

mean.a <- round(mean(data.a[[var]]),digits=2)
mean.b <- round(mean(data.b[[var]]),digits=2)
#mean.c <- round(mean(data.c[[var]]),digits=2)
mean.d <- round(mean(data.d[[var]]),digits=2)

min.a <-min(data.a[[var]])
min.b <-min(data.b[[var]])
#min.c <-min(data.c[[var]])
min.d <-min(data.d[[var]])

max.a <-max(data.a[[var]])
max.b <-max(data.b[[var]])
#max.c <-max(data.c[[var]])
max.d <-max(data.d[[var]])

Group <- c("Combined", "A", "B", "D")
Mean <- c(mean,mean.a,mean.b, mean.d)
Min <- c(min,min.a,min.b, min.d)
Max <- c(max,max.a,max.b, max.d)
summary <- data.frame(Group,Mean,Min,Max)
table1 <- tableGrob(summary,rows=NULL)
history <- rbind(c('Current',mean),history)
table2 <- tableGrob(history, rows=NULL)
Text <- grid.text(paste("Means calculated with NAs and 0s removed.\n",elim," 0s or NAs were removed for ",var,".",sep=""))

d <- ggplot(data, aes(get(var), fill=group)) + geom_density(alpha=.5)
d <- d + labs(x ="")

blankPanel <- grid.rect(gp=gpar(col="white"))

pdf("011 Midterm 3 Analysis.pdf",paper="a4")
grid.arrange(table1,table2,d,Text,ncol=2,top=paste("Statistics for",var,sep=" "))
dev.off()

#performance by major
pdf("012 Midterm 3 Analysis by major.pdf",paper="a4")
data <- transform(data,
                  major = ordered(major, levels = names( sort(-table(major)))))
e <- ggplot(data, aes(factor(major),get(var))) + geom_boxplot()
e <- e + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10),name=var)
e <- e + scale_x_discrete(name="Major (sorted by # of students per major)")
e
dev.off()

#performance by class standing
pdf("013 Midterm 3 Analysis by class standing.pdf",paper="a4")
data <- transform(data,
                  class = ordered(class, levels = c("FR","SO","JR","SR")))
f <- ggplot(data=data, aes(factor(class),get(var))) + geom_boxplot() + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10), name=var)
f <- f + scale_x_discrete(name = "Class")
f
dev.off()

#performance by TA
pdf("014 Midterm 3 stats by TA.pdf",paper="a4")
g <- ggplot(data=data, aes(factor(TA),get(var))) + geom_boxplot()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,130), breaks=seq(0,130,10),name=var)
g <- g + scale_x_discrete(name = "TA")
g
dev.off()

###############
#Final Exam
###############

##Final Exam Performance
var <- "Final.Total"
term <- "F16"

#this section contains past exam scores:
Quarter <- c('f15','w16','s16','su16')
MT1.Total <- c(75,75,75,75)
MT2.Total <- c(75,75,75,75)
MT3.Total <- c(75,75,75,75)
Final.Total <- c(75,75,75,75)
past.exams <- data.frame(Quarter,MT1.Total,MT2.Total,MT3.Total,Final.Total,stringsAsFactors = F)
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
#data.c <- subset(data,group=="C")
data.d <- subset(data,group=="D")

mean.a <- round(mean(data.a[[var]]),digits=2)
mean.b <- round(mean(data.b[[var]]),digits=2)
#mean.c <- round(mean(data.c[[var]]),digits=2)
mean.d <- round(mean(data.d[[var]]),digits=2)

min.a <-min(data.a[[var]])
min.b <-min(data.b[[var]])
#min.c <-min(data.c[[var]])
min.d <-min(data.d[[var]])

max.a <-max(data.a[[var]])
max.b <-max(data.b[[var]])
#max.c <-max(data.c[[var]])
max.d <-max(data.d[[var]])

Group <- c("Combined", "A", "B", "D")
Mean <- c(mean,mean.a,mean.b, mean.d)
Min <- c(min,min.a,min.b, min.d)
Max <- c(max,max.a,max.b, max.d)
summary <- data.frame(Group,Mean,Min,Max)
table1 <- tableGrob(summary,rows=NULL)
history <- rbind(c('Current',mean),history)
table2 <- tableGrob(history, rows=NULL)
Text <- grid.text(paste("Means calculated with NAs and 0s removed.\n",elim," 0s or NAs were removed for ",var,".",sep=""))

d <- ggplot(data, aes(get(var), fill=group)) + geom_density(alpha=.5)
d <- d + labs(x ="")

blankPanel <- grid.rect(gp=gpar(col="white"))

pdf("015 Final Analysis.pdf",paper="a4")
grid.arrange(table1,table2,d,Text,ncol=2,top=paste("Statistics for",var,sep=" "))
dev.off()

#performance by major
pdf("016 Final Analysis by major.pdf",paper="a4")
data <- transform(data,
                  major = ordered(major, levels = names( sort(-table(major)))))
e <- ggplot(data, aes(factor(major),get(var))) + geom_boxplot()
e <- e + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,320), breaks=seq(0,320,10),name=var)
e <- e + scale_x_discrete(name="Major (sorted by # of students per major)")
e
dev.off()

#performance by class standing
pdf("017 Final Analysis by class standing.pdf",paper="a4")
data <- transform(data,
                  class = ordered(class, levels = c("FR","SO","JR","SR")))
f <- ggplot(data=data, aes(factor(class),get(var))) + geom_boxplot() + scale_y_continuous(limits = c(0,320), breaks=seq(0,320,10), name=var)
f <- f + scale_x_discrete(name = "Class")
f
dev.off()

#performance by TA
pdf("018 Final stats by TA.pdf",paper="a4")
g <- ggplot(data=data, aes(factor(TA),get(var))) + geom_boxplot()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(limits = c(0,320), breaks=seq(0,320,10),name=var)
g <- g + scale_x_discrete(name = "TA")
g
dev.off()

###############
#LCFF Students - can be any selected group of students (this is a subset of the total class grades)
###############

#read the LCFF data only
LCFF<- read.delim("BIS_2A_F16_A_B_D_LCFF.txt",header=T)

#read the ClassDataCombo file with LCFF status indicated
ClassDataCombo<- read.delim("BIS_2A_F16_A_B_D.txt",header=T)

#read the ClassData file with LCFF students removed
ClassDataNoLCFF<- read.delim("BIS_2A_F16_A_B_D_noLCFF.txt",header=T)

#variables for LCFF data
MT1 <- LCFF$MT1.Total
MT2 <- LCFF$MT2.Total
MT3 <- LCFF$MT3.Total
Final <- LCFF$Final.Total
Course <- LCFF$Course.Total

#variables for Course Data
qMT1 <- ClassDataCombo$MT1.Total
qMT2 <- ClassDataCombo$MT2.Total
qMT3 <- ClassDataCombo$MT3.Total
qFinal <- ClassDataCombo$Final.Total
qCourse <- ClassDataCombo$Course.Total

#variables for combo Data with LCFF status indicated
cMT1 <- ClassDataNoLCFF$MT1.Total
cMT2 <- ClassDataNoLCFF$MT2.Total
cMT3 <- ClassDataNoLCFF$MT3.Total
cFinal <- ClassDataNoLCFF$Final.Total
cCourse <- ClassDataNoLCFF$Course.Total

#performance comparison plots
pdf("019_MT1_Performance_LCFF.pdf")
j<- ggplot(data = ClassDataCombo, aes(x=MT1.Total, fill = LCFF)) + geom_density(alpha=0.2) + xlim(0, 125)
plot(j)
dev.off()

pdf("020_MT2_Performance_LCFF.pdf")
k<- ggplot(data = ClassDataCombo, aes(x=MT2.Total, fill = LCFF)) + geom_density(alpha=0.2) + xlim(0,150)
plot(k)
dev.off()

pdf("021_MT3_Performance_LCFF.pdf")
m<- ggplot(data = ClassDataCombo, aes(x=MT3.Total, fill = LCFF)) + geom_density(alpha=0.2) + xlim(10, 150)
plot(m)
dev.off()

pdf("022_Final_Performance_LCFF.pdf")
l<- ggplot(data = ClassDataCombo, aes(x=Final.Total, fill = LCFF)) + geom_density(alpha=0.2) + xlim(50, 350)
plot(l)
dev.off()

pdf("023_Course_Total_Performance_LCFF.pdf")
n<- ggplot(data = ClassDataCombo, aes(x=Course.Total, fill = LCFF)) + geom_density(alpha=0.2) + xlim(40, 110)
plot(n)
dev.off()

#Summaty statistics
#Summary Data Tables into .csv files. The file will be saved in your working directory
df <- ClassDataCombo

#make all NA cells zero. Might alter means some, but not significantly
df[is.na(df)] <- 0

#T-Tests
#F test for equal variance
var.test(df$Course.Total ~ df$LCFF)
#t test
t.test(df$Course.Total ~ df$LCFF)

summary(ClassDataNoLCFF$MT3.Total)
summary(LCFF$MT3.Total)

###############
#Regressions
###############

#here come the regressions
#new data frame, course total less MT1
ClassDataCombo$Total_less_MT1 = ClassDataCombo$Course.Total - ClassDataCombo$MT1.Total
ClassDataCombo$Total_less_MT2 = ClassDataCombo$Course.Total - ClassDataCombo$MT2.Total
ClassDataCombo$Total_less_MT3 = ClassDataCombo$Course.Total - ClassDataCombo$MT3.Total
ClassDataCombo$Total_less_Final = ClassDataCombo$Course.Total - ClassDataCombo$Final.Total
ClassDataCombo$Total_midterms = (ClassDataCombo$MT1.Total + ClassDataCombo$MT2.Total + ClassDataCombo$MT3.Total) - ClassDataCombo$Course.Total

m1 <- lm(Total_less_MT1 ~ MT1.Total, ClassDataCombo)
summary(m1)
plot(ClassDataCombo$MT1.Total, ClassDataCombo$Total_less_MT1)
abline(lm(Total_less_MT1 ~ MT1.Total, ClassDataCombo))

m2 <- lm(Total_less_MT2 ~ MT2.Total, ClassDataCombo)
summary(m2)
plot(ClassDataCombo$MT2.Total, ClassDataCombo$Total_less_MT2)
abline(lm(Total_less_MT2 ~ MT2.Total, ClassDataCombo))

m3 <- lm(Total_less_MT3 ~ MT3.Total, ClassDataCombo)
summary(m3)
plot(ClassDataCombo$MT3.Total, ClassDataCombo$Total_less_MT3)
abline(lm(Total_less_MT3 ~ MT3.Total, ClassDataCombo))

m4 <- lm(Total_less_Final ~ Final.Total, ClassDataCombo)
summary(m4)
plot(ClassDataCombo$Final.Total, ClassDataCombo$Total_less_Final)
abline(lm(Total_less_Final ~ Final.Total, ClassDataCombo))

m5 <- lm(MT1.Total ~ MT2.Total, ClassDataCombo)
summary(m5)
plot(ClassDataCombo$MT1.Total, ClassDataCombo$MT2.Total)
abline(lm(MT2.Total ~ MT1.Total, ClassDataCombo))

m6 <- lm(MT1.Total ~ MT3.Total, ClassDataCombo)
summary(m5)
plot(ClassDataCombo$MT1.Total, ClassDataCombo$MT3.Total)
abline(lm(MT3.Total ~ MT1.Total, ClassDataCombo))

m7 <- lm(MT2.Total ~ MT3.Total, ClassDataCombo)
summary(m5)
plot(ClassDataCombo$MT2.Total, ClassDataCombo$MT3.Total)
abline(lm(MT3.Total ~ MT2.Total, ClassDataCombo))
