###########################################################################
##  Generating figures from literature search                            ##
## make sure that the csv file is updated to match the current xlsx file ##
###########################################################################

## path to campus pc
setwd("C:/Users/Sarah/Documents/Dropbox/PhD/Dissertation/Dissertation sections/literature review/Review documents")

## path to laptop
setwd("C:/Users/davebetts/Dropbox/PhD/Dissertation/Dissertation sections/literature review/Review documents")

dir()

##import data set
rv0<-read.csv("Reviews01.csv",header=T,skip=1,na.strings=c(".",""),strip.white=T) #readfrom csv file
dim(rv0) # check dimensions
str(rv0)
head(rv0)
names(rv0)


#get rid of some of the junker rows of journals
rv1<-subset(rv0,rv0$LitRev!="0",) #get rid of the rows where LitRev=0
rv1<-rv1[,!apply(rv1, 2, function(x) 
  all(gsub(" ", "", x)=="", na.rm=TRUE))] #removes columns of just null values
rv1

##############################################################
###        Number of Authors and Publications              ###
##############################################################
library(reshape)

authors<-rv1[,1:3]
str(authors)
authors<-melt.list(authors)

authors<-authors[,1]
authors<-gsub('; ','xyz',authors)
authors<-strsplit(authors,"xyz")
authors<-melt.list(authors)
authors<-na.omit(authors[,1])

authorsU<-length(unique(authors))
authorsR<-length(authors)-authorsU
authorsT<-as.data.frame(cbind(authorsU,authorsR))
colnames(authorsT)<-c("Authors","Repeats")
write.csv(authorsT,"unique_authors.csv",row.names=F)

authors<-as.data.frame(authors, row.number=F)
authors<-as.data.frame(authors, row.number=F,stringsAsFactors=F)
colnames(authors)<- "Author"
summary(authors)
unique(authors)
write.csv(authors,"authors.csv",row.names=F)
str(authors)

##############################################################
###                 Publication Years                      ###
##############################################################
### basic statistics
range(rv1$Year)
summary(rv1$Year)

#not a bar plot but similar information as hist()
plot(table(rv1$Year))

### for histogram
yrmin=min(rv1$Year) #determine earliest publication year
yrmax=max(rv1$Year) #determine latest publication year

histinfo<-hist(rv1$Year, #histogram of years of publication
     xlab="Publication Year", #X-axis label
     ylab="", # no Y-axis label
     breaks=seq(from=yrmin-0.5,to=yrmax+0.5, by=1), #number of breaks in histogram based on the range of the variable "Years"
     main="Number of Publications per Year", #Title of graph
     col="lightgrey", #color of bars
     xlim=c(yrmin-0.5,yrmax+0.5), #set range of x axis
     axes=F) #removing automatic axes labels
histinfo

### labelling
axis(2, col.axis="black", las=2) #create axis and labels for Y axis
axis(1, xaxp=c(yrmin,yrmax+1,(yrmax+1-yrmin))) #create axis and labels for X axis
box() #enclose histogram within a box

##############################################################
###                    table of journals                   ###
##############################################################

# basic information
unique(rv1$Journal)
length(unique(rv1$Journal))

library(plyr) #allows for the creation of tables in a format better for reproduction
count(rv1,"Journal")

##############################################################
###               table of journal types                   ###
##############################################################
# basic information
unique(rv1$JournalType)
length(unique(rv1$JournalType))

library(plyr) #allows for the creation of tables in a format better for reproduction
Type<-count(rv1,"JournalType")
Type

Journals<-count(rv1,c("Journal", "JournalType"))
Journals<-Journals[,c(1,3,2)]
Journals
Categories<-merge(Type, Journals,by=c("JournalType","JournalType"),all.x=T)                
Categories<-Categories[,c(3,4,1,2)]
Categories
colnames(Categories)<-c("Journal","Number of Publications","Category","Publications per Category")
write.csv(Categories[order(Categories$Category),],"Journals.csv",row.names=F)
ls(Journals)
ls(Type)

#write in rows to table for that is in the CSV that i submittedd to sarah on 1/14

##############################################################
###                     Geography                          ###
##############################################################
# create subset of geographic information
names(rv0) # double check column numbers 
geo<-rv0[,c(1:6,116:123)]
str(geo)
head(geo)
tail(geo)

##import data set
geogr0<-read.csv("geogr.csv",header=T, na.strings=c(".","")) #readfrom csv file
dim(geogr0) # check dimensions
str(geogr0)
head(geogr0)
names(geogr0)

library(plyr) #allows for the creation of tables in a format better for reproduction
count(geogr0,"Region")
count(geogr0,"Country")
count(geogr0,"State")
count(geogr0,"Locality")

count(geogr0,c("Title","Region")

table(geogr0$Title,geogr0$Region)
count(geogr0,"JournalType")
count(geogr0,"JournalType")
count(geogr0,"JournalType")
count(geogr0,"JournalType")
table(unique(geogr0$Title~geogr0Region))
table(unique(geogr0$Title~geogr0Region))
