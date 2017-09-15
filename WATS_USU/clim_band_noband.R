rm(list=ls())
gc()
dev.off()
palette("default")
require(graphics)

letter = c("r","p","t")
climvar = c("Relative Humidity","Precipitation","Air Temperature")
unts = c(" [%]"," [mm]"," [C]")
require(colorspace)
clrs=c(colorRampPalette(c("blue","purple", "orange","yellow","green3"))( 7 ),"red")

plotCol(clrs)
#####
# clrs = vector(mode="list", length=3)
# clrs[[1]]=c(colorRampPalette(c("red", "yellow1"))( 7 ),"blue")
# clrs[[2]]=c(colorRampPalette(c("blue", "white"))( 7 ),"red")
# clrs[[3]]=c(palette(heat.colors(7)),"blue")
# 
# clrs[[1]]=c(palette(heat.colors(7)),"blue")
# clrs[[2]]=c(palette(heat.colors(7)),"blue")
# clrs[[3]]=c(palette(heat.colors(7)),"blue")

# letter="r"
# climvar = "Relative Humidity"
# unts=' [%]'
# clrs=c(colorRampPalette(c("red", "yellow1"))( 7 ),"blue") ## (n)palatte(cm.colors(7))
# plotCol(clrs)
# letter="p"
# climvar = "Precipitation"
# unts=' [mm]'
# clrs=c(colorRampPalette(c("mediumblue", "lightskyblue"))( 7 ),"red") ## (n)palatte(cm.colors(7))
# plotCol(clrs)
# letter="t"
# climvar = "Air Temperature"
# unts=' [C]'
#####plotCol(clrs)
#####

for (k in 1:3){
  file = paste0(letter[k],".csv")
  label = paste0(letter[k],"full.csv")
  c1 = read.csv(file,skip=1)
  colnames(c1)[1]="date"
  head(c1)
  str(c1)
  
  
  file = paste0(letter[k],"EF.csv")
  c2 = read.csv(file,skip=1)
  colnames(c2)[1]="date"
  head(c2)
  str(c2)
  
  c3 = merge(c1,c2, by="date")
  # not a good table
  # tbl=summary(c3)
  # write.csv(tbl,paste0(letter[k],"tbl.csv"))
  
  library(reshape)
  c4=melt(c3)
  colnames(c4)[2]="catch"
  c4$elev = c4$catch
  c4$elev = gsub("[^0-9]","",c4$elev)
  c4$elev[!nzchar(c4$elev)] <- "Unbanded"
  c4$elev = as.factor(c4$elev)
  c4$catch=gsub('[0-9]+', '', c4$catch)
  c4$date=as.Date(c4$date)
  require(lubridate)
  c4$month=month(c4$date)
  head(c4)
  tail(c4)
  str(c4)
  write.csv(c4,label,row.names=FALSE)
  
  cs=unique(c4$catch)
  tbls=vector(mode = "list", length = length(cs))
  names(tbls) = cs
  # for (i in cs) {
  #   newlab = i
  #   x = c4[which(c4$catch == i),]
  #   require(reshape)
  #   x1 = cast(x, date ~ elev, value = "value")
  #   write.csv(x1,paste0(letter[k],newlab,".csv"),row.names=F)
  #   tbls[[i]]=x1
  #   x2 = x[,c("date","elev","value")]
  # plot(range(x2$date), range(x2$value),type = "n", main=newlab, xlab = "Date", ylab = paste0("Average monthly ",climvar[k],unts[k]))
  #   for (j in 2:ncol(x1)) {
  #     lines(x1[,1],x1[,j],col=j)
  #   }  
  # }
  
  names(clrs)=(sort(unique(c4$elev)))
  for (i in cs) {
    newlab = i
    newlab1=gsub("Creek.*"," Creek",newlab)
    newlab1=gsub("Fork.*"," Fork",newlab1)
    x = c4[which(c4$catch == i),]
    require(reshape)
    x1 = aggregate(value ~ catch + elev + month, data = x,mean)
    x2 = cast(x1, month ~ elev, value = "value")
    write.csv(x2,paste0("Monthly_",letter[k],newlab,".csv"),row.names=F)
    plot(range(x1$month), range(x1$value),type = "n", main=paste0(newlab1,"\nAverage ",climvar[k], " per Month"), xlab = "", ylab = paste0(climvar[k],unts[k]), xaxt="n")
    # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey93")
    axis(1,1:12,labels=month.abb)
    for (j in 2:ncol(x2)) {
      lines(x2[,1],x2[,j],col=clrs[colnames(x2)[j]],lwd=2)
    }
    lines(x2[,1],x2[,ncol(x2)],col=2,lwd=2,type="o",pch=2)
    loc = ifelse(letter[k] == "t", "topleft", "bottomleft")
    num=rep(NA,ncol(x2)-1)
    num=c(num,2)

    legend(loc,inset=c(0.03,0.06),legend=colnames(x2)[2:ncol(x2)],col=clrs[colnames(x2)[2:ncol(x2)]],bty="n",lwd=2, title=" ")
    legend(loc,inset=c(0.03,0.06),legend=rep(" ",round((ncol(x2))/1.3,0)-1),pch=num,title="Catchments", cex=1.3, bty="n")
    }
}


#####
dev.off()
#####

plot(range(tbls[[1]][,1]),range(tbls[[1]][,2:ncol(tbls[[1]])]),type="n",col=colnames(tbls[[1]][-1]))

c5=cast(c4,value ~ .)

#########
# # differences
# nc = c2[-1]
# nc[,1:6]=c2[,2:7]-c1[,2]
# nc[,7:11]=c2[,8:12]-c1[,3]
# nc[,12:16]=c2[,13:17]-c1[,4]
# nc[,17:20]=c2[,18:21]-c1[,5]
# nc[,21:23]=c2[,22:24]-c1[,6]
# nc[,23]=c2[,23]-c1[,6]
# 
# nc2=melt(nc)
# nc2[,1]=as.factor(gsub("[0-9]","",nc2[,1]))
# str(nc2)
# boxplot(value~variable, nc2)
# 
# 
# # differences
# nc = c2[-1]
# nc[,1:6]=c2[,2:7]/c1[,2]
# nc[,7:11]=c2[,8:12]/c1[,3]
# nc[,12:16]=c2[,13:17]/c1[,4]
# nc[,17:20]=c2[,18:21]/c1[,5]
# nc[,21:23]=c2[,22:24]/c1[,6]
# nc[,23]=c2[,23]/c1[,6]
# 
# nc2=melt(nc)
# nc2[,1]=as.factor(gsub("[0-9]","",nc2[,1]))
# str(nc2)
# boxplot(value~variable, nc2)
# 
# 
# # differences
# nc = c2[-1]
# nc[,1:6]=(c2[,2:7]-c1[,2])/c1[,2]
# nc[,7:11]=(c2[,8:12]-c1[,3])/c1[,3]
# nc[,12:16]=(c2[,13:17]-c1[,4])/c1[,4]
# nc[,17:20]=(c2[,18:21]-c1[,5])/c1[,5]
# nc[,21:23]=(c2[,22:24]-c1[,6])/c1[,6]
# nc[,23]=(c2[,23]-c1[,6])/c1[,6]
# 
# nc2=melt(nc)
# nc2[,1]=as.factor(gsub("[0-9]","",nc2[,1]))
# str(nc2)
# boxplot(value~variable, nc2)
# 
# # plots
# east=cbind(c1[,2],c2[,2:7])
# hayden=cbind(c1[,3],c2[,8:12])
# still=cbind(c1[,4],c2[,13:17])
# west=cbind(c1[,5],c2[,18:21])
# willow=cbind(c1[,6],c2[,22:24])
# willow[,4]=c2[,23]
# 
# 
# #
# 
# boxplot(still)
# 
#########

avg=as.data.frame(sapply(c3[-1],mean))
avg
str(avg)

avg1=as.data.frame(sapply(c1[-1],mean))
avg2=as.data.frame(sapply(c2[-1],mean))

# differences
ac=avg2
ac[1:6,]=avg2[1:6,]-avg1[1,]
ac[7:11,]=avg2[7:11,]-avg1[2,]
ac[12:16,]=avg2[12:16,]-avg1[3,]
ac[17:20,]=avg2[17:20,]-avg1[4,]
ac[21:23,]=avg2[22:24,]-avg1[5,]
ac[23,]=avg2[23,]-avg1[5,]
ac = as.data.frame(cbind(rownames(avg2),ac))
colnames(ac)=c("catch","mean")
ac[,1]=as.factor(gsub("[0-9]","",ac[,1]))
boxplot(mean~catch, ac)
summary(ac)

