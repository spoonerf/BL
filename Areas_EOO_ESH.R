# path<-"D:/ESH/Areas/"
# lf<-list.files(path)
# 
# df<-data.frame(Species = character(), Num_Cells = double(), Area = double())
# 
# for (i in 1:length(lf)){
#   a<-read.table(paste(path, lf[i], sep=""), sep=",")
#   df<-rbind(a,df)  
# }
# 
# colnames(df)<- c("Species", "Number_Cells", "ESH_Area_Sq_KM")
# df
bird<-read.csv("D:/Birds/Habitats.csv")
amphib<-read.csv("D:/Amphibians/Amphibians_Habitat.csv")
reptiles<-read.csv("D:/Reptiles/Reptiles_Habitat.csv")
mammals<-read.csv("D:/Mammals/Mammals_Habitat.csv")

bdf<-data.frame(bird$Scientific.name,  bird$X2016.IUCN.Red.List.Category)
bdf<-unique(bdf)
colnames(bdf)<-c("Species","Category")

adf<-data.frame(amphib$Species,amphib$Category)
adf<-unique(adf)
colnames(adf)<-c("Species", "Category")

rdf<-data.frame(reptiles$Species, reptiles$Category)
rdf<-unique(rdf)
colnames(rdf)<-c("Species", "Category")

mdf<-data.frame(mammals$Species, mammals$Category)
mdf<-unique(mdf)
colnames(mdf)<-c("Species", "Category")

sp_rl<-rbind(bdf,adf,rdf, mdf)


library(foreign)

AOO<-read.dbf("D:/Species_Ranges/AOO/BMRA_area_AOO.dbf")
aoo_df<-data.frame(AOO$binomial, AOO$Area)
colnames(aoo_df)<-c("binomial", "Area_AOO")
kba<-read.dbf("D:/Species_Ranges/Maj_Imp_And_Suitable/EOO_KBA_Intersect_Behr_Diss.dbf")

kba_df<-merge(kba, aoo_df, by="binomial")

#####AOO only species

int<-read.dbf("D:/Species_Ranges/AOO/BMRA_area_AOO_all_diss_behr_selection_nonesh_kba_intersect.dbf")
Area<-int$Area
Area_AOO<-int$Area_1
int2<-int[,c(2,8:31)]
int2$Area<-Area
int2$Area_AOO<-Area_AOO

kba_df<-rbind(kba_df, int2)

###Here  kba_df$Area is the area of intersect of each species range with each kba

kba_df$AOO_KBA_pcnt_cover<-(kba_df$Area/kba_df$Area_AOO)*100

hist(kba_df$AOO_KBA_pcnt_cover)

kba_df2<-merge(kba_df, sp_rl, by.x="binomial", by.y="Species")




head(kba_df2)

####ESH area
path<-"D:/ESH/Maj_Imp_And_Suitable/"
lf<-list.files(path, pattern = "*ESH.tif$")
bin<-gsub("_ESH.tif", "", lf)

df<-data.frame(Species = character(), Area = double())

for (i in 1:length(bin)){
  r<-raster(paste(path, bin[i], "_ESH.tif", sep = ""))
  ak<-(sum(na.omit(values(r))) * res(r)[1] * res(r)[2]) /1000000  
  a<-data.frame(bin[i], ak)
  df<-rbind(a,df)
  print(a)
  }

colnames(df)<-c("Species", "Area_ESH_KM2")

#####ESH Intersect


path<-"D:/ESH/Maj_Imp_And_Suitable/"
lf<-list.files(path, pattern = "*ESH.tif$")

r<-raster(paste(path, lf[1], sep = ""))
plot(r)

library(rgdal)
kba_shp<-readOGR(dsn = "D:/Species_Ranges/Maj_Imp_And_Suitable", layer = "EOO_KBA_Intersect_Behr_Diss")

dfi<-data.frame()

for (i in 1: length(kba_shp)){
  k<-kba_shp[i,]
  r<-raster(paste(path,as.character(kba_shp@data[i,1]), "_ESH.tif" , sep = ""))  
  m<-mask(r, k)
  ESH_int<-(sum(na.omit(values(m))) * res(m)[1] * res(m)[2]) /1000000  
  int_df<-data.frame(kba_shp@data[i,], ESH_int)
  dfi<-rbind(dfi, int_df)  
  print(ESH_int)
  }
  



ESH<-merge(dfi, df,by.x ="binomial", by.y = "Species")
ESH$ESH_KBA_pcnt_cover <- (ESH$ESH_int/ESH$Area_ESH_KM2) *100

##excluding non necessary rows
kba_range<-kba_df2[,c(1,2,27:29)]

dfall<-merge(ESH, kba_range, by = c("binomial", "SITRECID"))


plot(log10(dfall$Area_ESH_KM2),log10(dfall$Area_AOO))



#Criterion A1a iii) - Site regularly holds >= 0.5% of the global population size (ESH) and >= 5 reproductive units of a CR or EN species

dfall$A1a.3<-(dfall$ESH_KBA_pcnt_cover >= 0.5 & (dfall$Category  == "CR" | dfall$Category  == "EN")) *1

length(unique(dfall[dfall$A1a.3 == 1,]$SITRECID))

#Criterion A1b iii) - Site regularly holds >= 1% of the global population size (ESH) and >= 10 reproductive units of a VU species

dfall$A1b.3<-(dfall$ESH_KBA_pcnt_cover >= 1 & (dfall$Category  == "VU")) *1

length(unique(dfall[dfall$A1b.3 == 1,]$SITRECID))


###not sure about these two
#Criterion A1c iii) - Site regularly holds >= 0.1% of the global population size (ESH) and >= 5 reproductive units of a CR or EN species due only to population size reduction in the past or present

dfall$A1c.3<-(dfall$ESH_KBA_pcnt_cover >= 0.1 & (dfall$Category  == "CR" | dfall$Category  == "EN")) *1

length(unique(dfall[dfall$A1c.3 == 1,]$SITRECID))


#Criterion A1d iii) - Site regularly holds >= 0.2% of the global population size (ESH) and >= 10 reproductive units of a VU species

dfall$A1d.3<-(dfall$AOO_KBA_pcnt_cover >= 0.2 & (dfall$Category  == "VU")) *1

length(unique(dfall[dfall$A1d.3 == 1,]$SITRECID))



#Criterion A1a iv) - Site regularly holds >= 0.5% of the global population size (range) and >= 5 reproductive units of a CR or EN species

dfall$A1a.4<-(dfall$AOO_KBA_pcnt_cover >= 0.5 & (dfall$Category  == "CR" | dfall$Category  == "EN")) *1

length(unique(dfall[dfall$A1a.4 == 1,]$SITRECID))

#Criterion A1b iv) - Site regularly holds >= 1% of the global population size (range) and >= 10 reproductive units of a VU species

dfall$A1b.4<-(dfall$AOO_KBA_pcnt_cover >= 1 & (dfall$Category  == "VU")) *1

length(unique(dfall[dfall$A1b.4 == 1,]$SITRECID))


###not sure about these two
#Criterion A1c iv) - Site regularly holds >= 0.1% of the global population size (range) and >= 5 reproductive units of a CR or EN species due only to population size reduction in the past or present

dfall$A1c.4<-(dfall$AOO_KBA_pcnt_cover >= 0.1 & (dfall$Category  == "CR" | dfall$Category  == "EN")) *1

length(unique(dfall[dfall$A1c.4 == 1,]$SITRECID))


#Criterion A1d iv) - Site regularly holds >= 0.2% of the global population size (range) and >= 10 reproductive units of a VU species

dfall$A1d.4<-(dfall$AOO_KBA_pcnt_cover >= 0.2 & (dfall$Category  == "VU")) *1

length(unique(dfall[dfall$A1d.4 == 1,]$SITRECID))
#########

#Criterion B1 iii) - Site regularly hold >= 10% of global population size (ESH) and >= 10 reproductive units of a species

dfall$B1.3<-(dfall$AOO_KBA_pcnt_cover >= 10) *1

length(unique(dfall[dfall$B1.4 == 1,]$SITRECID))


#Criterion B1 iv) - Site regularly hold >= 10% of global population size (Range) and >= 10 reproductive units of a species

dfall$B1.4<-(dfall$AOO_KBA_pcnt_cover >= 10) *1

length(unique(dfall[dfall$B1.4 == 1,]$SITRECID))


head(dfall)
##changing NA values to 0
dfall[,c(33:41)][is.na(dfall[,c(33:41)])]<-0


sit_crit<-aggregate(dfall[,c(33:41)], by = list(dfall$SITRECID) , FUN= "sum")
sit_crit$all<-rowSums(sit_crit[,c(2:10)])

#number of kbas with no triggers under these criteria
no_trig<-sit_crit[sit_crit$all ==0,]

write.csv(no_trig, "kba_no_trigger.csv")

dfall$A1a.3[dfall$A1a.3 == 1]<-"A1a.3"
dfall$A1a.3[dfall$A1a.3 == 0]<-""

dfall$A1b.3[dfall$A1b.3 == 1]<-"A1b.3"
dfall$A1b.3[dfall$A1b.3 == 0]<-""

dfall$A1c.3[dfall$A1c.3 == 1]<-"A1c.3"
dfall$A1c.3[dfall$A1c.3 == 0]<-""

dfall$A1d.3[dfall$A1d.3 == 1]<-"A1d.3"
dfall$A1d.3[dfall$A1d.3 == 0]<-""

dfall$A1a.4[dfall$A1a.4 == 1]<-"A1a.4"
dfall$A1a.4[dfall$A1a.4 == 0]<-""

dfall$A1b.4[dfall$A1b.4 == 1]<-"A1b.4"
dfall$A1b.4[dfall$A1b.4 == 0]<-""

dfall$A1c.4[dfall$A1c.4 == 1]<-"A1c.4"
dfall$A1c.4[dfall$A1c.4 == 0]<-""

dfall$A1d.4[dfall$A1d.4 == 1]<-"A1d.4"
dfall$A1d.4[dfall$A1d.4 == 0]<-""

dfall$B1.3[dfall$B1.3 == 1]<-"B1.3"
dfall$B1.3[dfall$B1.3 == 0]<-""

dfall$B1.4[dfall$B1.4 == 1]<-"B1.4"
dfall$B1.4[dfall$B1.4 == 0]<-""

cols <- c("A1a.3" ,"A1b.3" , "A1c.3", "A1d.3", "A1a.4", "A1b.4", "A1c.4", "A1d.4", "B1.3", "B1.4" )

# create a new column `x` with all criteria columns collapsed together
dfall$criteria_triggered <- apply( dfall[ , cols ] , 1 , paste , collapse = " " )


colnames(dfall)[c(26,27,28,30,31)]<-c("Range_Area_KBA_Intersect_km2", "ESH_Area_KBA_Intersect_km2", "Total_ESH_Area_km2", "Total_Range_Area_km2", "Range_KBA_pcnt_cover")
head(dfall)


write.csv(dfall, "KBA_criteria.csv")

#AOO - ESH reduction

plot( log10(dfall$Area_AOO),((dfall$Area_AOO - dfall$Area_ESH_KM2)/dfall$Area_ESH_KM2), ylab = "Percentage reduction in size from range to ESH", xlab ="Log10 Range size (km2)")








