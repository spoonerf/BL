install.packages(c("ff", "ffbase"))
library(ffbase)
library(ff)

big<-"D:/ebd_relNov-2016/ebd_relNov-2016/ebd_relNov-2016.txt"
sample<-"C:/Users/Fiona/Documents/BirdLife/BirdLife/ebd_US-CA-053_201401_201409_relAug-2014_SAMPLE/ebd_US-CA-053_201401_201409_relAug-2014.txt"
  
x<-read.csv.ffdf(file=sample,
              header=TRUE,
              first.rows=10,
              next.rows=10,
              sep="\t",
              colClasses=c("factor","factor", 
                           "factor","factor", 
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "numeric","numeric",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor",
                           "factor","factor","factor"))



#https://www.r-bloggers.com/big-data-analysis-for-free-in-r-or-how-i-learned-to-load-manipulate-and-save-data-using-the-ff-package/


#https://www.r-bloggers.com/if-you-are-into-large-data-and-work-a-lot-with-package-ff/

require(ffbase)
hhp <- read.table.ffdf(file=big, FUN = "read.csv", sep="\t", header=TRUE, first.rows = 50,colClasses=c("factor","factor", 
                                                                                                       "factor","factor", 
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "numeric","numeric",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor",
                                                                                                       "factor","factor","factor"))

class(hhp)

dim(hhp)
LON<-hhp[,"LONGITUDE"]
LAT<-hhp[,"LATITUDE"]

plot(LON,LAT)
## Some basic showoff
result <- list()
## Unique members, Unique combination of members and health care providers, find unexpected duplicated records
result$members <- hhp$LONGITUDE










x<-read.csv.ffdf(file=sample,
                 header=TRUE,
                 first.rows=10,
                 next.rows=10,
                 sep="\t",
                 colClasses=c(rep("character", 41)))

######################################################################################
##Getting Column Names
file_in <- file(big,"r")
file_out <- file("out.csv","a")
x <- readLines(file_in, n=1)
#sub<-gsub("\t", " ",x)

names<-unlist(strsplit(x, split="\t"))


########################################
####Getting the data - need to find a way to loop it sensibly or a function
x2 <- readLines(file_in, n=3000000)
test<-strsplit(x2, split="\t")
forty<-function(row){
  rowf<-row[1:40]    #only first 40 cols are there consistently so chucking any after this to make formatting simpler
  return(rowf)
}

test40<-lapply(test,forty)
un<-unlist(test40)

d<-matrix(un, ncol=40, byrow=T)

#data.frame(d)
colnames(d)<-names[1:40]

dm<-d[d[, "SCIENTIFIC NAME"] == "Ramphastos ambiguus",]

xy<-dm[,c(24,23)]
plot(xy)

nrow(dm)

toucan<-readOGR(dsn="C:/Users/Fiona/Documents/BirdLife/IUCN/species_22727999/species_22727999", layer= "species_22727999")

xy<-matrix(as.numeric(xy), ncol=2)
xysp<-SpatialPoints(xy)

crs(xysp)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

plot(toucan, col="grey")
points(xysp, col="red")

io<-over(xysp, toucan)


spdf<-data.frame(cbind(xy, io$ID_NO))
spdf$X3[is.na(spdf$X3)]<-0
sum(spdf$X3)/nrow(spdf)

plot(spdf$X1, spdf$X2, col=spdf$X3)

tdf<-fortify(toucan)
library(ggplot2)
p<-ggplot()+
    geom_polygon(data=tdf, aes(x=long, y=lat,  group=group), fill="light grey")+
    geom_point(data=spdf, aes(x=X1, y=X2, colour=X3))+
    theme_bw()
p

install.packages("rgeos")
library(rgeos)

xym <- spTransform(xysp, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
tcm <- spTransform(toucan, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))



apply(gDistance(xym, tcm,byid=TRUE),2,min)

gDistance(xysp,toucan)



close(file_in)
close(file_out)







B <- 30 # depends how large is one pack
while(length(sub)) {
  ind <- grep("Fulica americana", sub)
  if (length(ind)) writeLines(x[ind], file_out)
  sub <- readLines(file_in, n=B)
}




