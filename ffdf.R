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
file_in <- file(big,"r")
file_out <- file("out.csv","a")
x <- readLines(file_in, n=1)
#sub<-gsub("\t", " ",x)

names<-unlist(strsplit(x, split="\t"))


x2 <- readLines(file_in, n=3000000)
#sub2<-gsub("\t", " ",x2)
#sub2[2:length(sub2)]
test<-strsplit(x2, split="\t")
forty<-function(row){
  rowf<-row[1:40]
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
plot(toucan, col="grey")
points(xy, col="red")

xy<-matrix(as.numeric(xy), ncol=2)
xy<-SpatialPoints(xy)

crs(xy)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

over(xy, toucan)







B <- 30 # depends how large is one pack
while(length(sub)) {
  ind <- grep("Fulica americana", sub)
  if (length(ind)) writeLines(x[ind], file_out)
  sub <- readLines(file_in, n=B)
}
close(file_in)
close(file_out)





