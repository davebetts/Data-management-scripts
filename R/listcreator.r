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
unwanted_array = list('Ã©' = 'e', 'Äf' = 'a', 'ÅY' = 's', 'Ã³'='o','Ã¡' = 'a', 'Ã¼' = 'u', 'Ã-' = 'i', 'S'='S', 's'='s', 'a'='a', 's'='s', 'Z'='Z', 
                      'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'a???²'='_',
                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

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