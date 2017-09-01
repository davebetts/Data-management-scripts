###########################################################################
##          Updating literature list for literature review               ##
##          Using export of 001 folder in zotero to CSV file             ##
###########################################################################

## path to campus pc
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/Dissertation/Dissertation sections/literature review/Review documents")

## path to laptop
setwd("C:/Users/davebetts/Dropbox/PhD/Dissertation/Dissertation sections/literature review/Review documents")

dir()

##import data set
list<-read.csv("Exported Items.csv",header=T,na.strings=c(".",""," ")) #import csv file
dim(list) # check dimensions
str(list)
head(list)
names(list)
summary (list)

list1<-list[,!apply(list, 2, function(x) all(gsub(" ", "", x)=="", na.rm=TRUE))] #removes columns that contain only null values
str(list1)
names(list1)

list1$Author

attach(list1)
#convert special characters to plain text
unwanted_array = list('é' = 'e', '�f' = 'a', '�Y' = 's', 'ó'='o','á' = 'a', 'ü' = 'u', '�-' = 'i', 'S'='S', 's'='s', 'a'='a', 's'='s', 'Z'='Z', 
                      'z'='z', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='A', '�'='C', '�'='E', '�'='E', 'a???�'='_',
                      '�'='E', '�'='E', '�'='I', '�'='I', '�'='I', '�'='I', '�'='N', '�'='O', '�'='O', '�'='O', '�'='O', '�'='O', '�'='O', '�'='U',
                      '�'='U', '�'='U', '�'='U', '�'='Y', '�'='B', '�'='Ss', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='a', '�'='c',
                      '�'='e', '�'='e', '�'='e', '�'='e', '�'='i', '�'='i', '�'='i', '�'='i', '�'='o', '�'='n', '�'='o', '�'='o', '�'='o', '�'='o',
                      '�'='o', '�'='o', '�'='u', '�'='u', '�'='u', '�'='y', '�'='y', '�'='b', '�'='y')

chartr(paste(names(unwanted_array), collapse=''),
       paste(unwanted_array, collapse=''),
       list1$Author)
# the loop:
out <- list1$Author
for(i in seq_along(unwanted_array))
  out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)

list1$Author<-out
write.csv(list1$Author,"author.csv",row.names=F,na="")


attach(list1)
#keep the following columns
list2<-list1[,c("Author","Title","Publication.Title","Publication.Year","Item.Type", "Date.Added")]
write.csv(list2,"list.csv",row.names=F,na="")


#keep the following columns (added values)
list2<-list1[,c("Author","Title","Publication.Title","Publication.Year","Item.Type","Manual.Tags","Automatic.Tags","Notes")]
write.csv(list2,"list.csv",row.names=F,na="")

str(list2)
table(Item.Type)

rm(list=ls())
gc()

author<-as.data.frame(list1$Author)
author1<-as.data.frame(split(author,'; '))
author1

install.packages(tidyr)
library(tidyr)
separate(as.data.frame(list1$Author), col=sep = "; ")
, remove = TRUE,
         convert = FALSE, extra = "error", ...)