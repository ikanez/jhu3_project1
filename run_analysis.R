# Getting and Cleaning Data Set Project

#load test data
test.subject <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/test/subject_test.txt",sep= "", blank.lines.skip = TRUE, header=FALSE)
test.x <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/test/X_test.txt", sep= "", blank.lines.skip = TRUE, header=FALSE)
test.y <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/test/y_test.txt", sep= "", blank.lines.skip = TRUE, header=FALSE)

#load labels
act.label <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/activity_labels.txt" , sep= "", blank.lines.skip = TRUE, header=FALSE )
fea.label <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/features.txt" , sep= "", blank.lines.skip = TRUE, header=FALSE )

#load train data
train.subject <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/train/subject_train.txt",sep= "", blank.lines.skip = TRUE, header=FALSE)
train.x <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/train/X_train.txt", sep= "", blank.lines.skip = TRUE, header=FALSE)
train.y <- read.csv("D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/UCIHAR/train/y_train.txt", sep= "", blank.lines.skip = TRUE, header=FALSE)

#combine test data
test.tab <- cbind(test.subject, test.y, test.x)

#combine train data
train.tab <- cbind(train.subject, train.y, train.x)

#combine both data into same table
combined.tab <- rbind(test.tab, train.tab)

#apply column label on combined table
colnames(combined.tab) <- c("Subject", "ActivityID", as.character(fea.label$V2))

#apply label on activity in combined table
colnames(combined.tab) <- c("Subject", "ActivityID", as.character(fea.label$V2))

#extract only subject, activity id, mean variables, and standard deviation variables
newtab <- cbind(combined.tab$Subject, combined.tab$ActivityID, combined.tab[,grep("mean",colnames(combined.tab))], combined.tab[,grep("std",colnames(combined.tab))] )
colnames(newtab) <- c("Subject","ActivityID")

newtab2 <- join(newtab,act.label, by= "ActivityID", type="left")

#finalise labelling (for Step #4)
mean.names <- colnames(combined.tab[grep("mean",colnames(combined.tab))])
std.names <- colnames(combined.tab[grep("std",colnames(combined.tab))])
colnames(newtab2) <- c("Subject", "ActivityID", mean.names, std.names,"ActivityLabel")

#get means for each variable, mean and std (Step #5)
library(dplyr)
final.tab <- newtab2 %>% group_by(Subject,ActivityLabel) %>% summarise_each(funs(mean), vars= grep("mean",colnames(newtab2.grouped)), grep("std",colnames(newtab2.grouped)))
#somehow the means variables didn't get labelled. rename the columns.
colnames(final.tab) <- c("Subject", "ActivityLabel", mean.names, std.names)

#print out
write.table(final.tab, file = "D:/Self_Development/Coursera - JHU/3. Getting and Cleaning Data Set/step5_output.txt", row.name=FALSE)
