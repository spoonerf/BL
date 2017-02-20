#Getting all species names from BirdLife and splitting the Binomial into Genus and Species

blsp<-read.csv("BirdLife_Checklist_Version_9.csv")

blsp$Scientific.name<-as.character(blsp$Scientific.name)

GENUS<-NULL
SPECIES<-NULL
for (i in 1:length(blsp$Scientific.name)){

  GENUS[i]<-strsplit(blsp$Scientific.name, " ")[[i]][1]
  SPECIES[i]<-strsplit(blsp$Scientific.name, " ")[[i]][2]
  bin_bind<-cbind(GENUS, SPECIES)
  print(i/(length(blsp$Scientific.name)))
  }


write.csv(bin_bind, "Species_Name_split.csv")


bin_bind<-read.csv("Species_Name_split.csv", stringsAsFactors = F)
bin_bind<-bin_bind[,c(2,3)]
GENUS<-as.character(bin_bind$GENUS)
SPECIES<-as.character(bin_bind$SPECIES)


library(dismo)

#Counting number of occurrences for each species

sp_no<-NULL
for (i in 1:length(GENUS)){
  
  #sp<-gbif(GENUS[i], SPECIES[i], geo=TRUE, end=1200, removeZeros=T)
  sp<-gbif(GENUS[i], SPECIES[i], download = F)
  sp_n<-cbind(blsp$Scientific.name[i],sp)
  print(sp_n)
  sp_no<-rbind(sp_n, sp_no)
 
  #print(sp$species[1])
  #sp<-sp[,c("Country", "dateIdentified","identificationID","lat", "lon","species", "verbatimLocality")]
  sp_dups <- sp[c("lat", "lon")]
  sp[!duplicated(sp_dups),]
  #write.csv(sp, paste(paste(bin_bind[i,1], bin_bind[i,2], sep="_"), ".csv", sep=""))

            }
#some duplicates - perhaps because of sub-species


sp_df<-data.frame(sp_no)
sp_df$sp<-as.numeric(as.character(sp_df$sp))
colnames(sp_df)<- c("Species", "No_of_Occurrences")
sp_dups <- sp_df[c("Species", "No_of_Occurrences")]
sp_df<-sp_df[!duplicated(sp_dups),]

sp_df[with(sp_df, order(-No_of_Occurrences)), ]
write.csv(sp_df, "species_occurrences_gbif.csv")




#Extracting all the observation locations from GBIF for each species

sp_no<-NULL
for (i in 1:length(GENUS)){
  
  sp<-gbif(GENUS[i], SPECIES[i], geo=TRUE, removeZeros=T)
  #sp<-gbif(GENUS[i], SPECIES[i], download = F)
  sp_n<-cbind(blsp$Scientific.name[i],sp)
  #print(sp_n)
  #sp_no<-rbind(sp_n, sp_no)
  
  print(sp$species[1])
  sp<-sp[,c("Country", "dateIdentified","identificationID","lat", "lon","species", "verbatimLocality")]
  sp_dups <- sp[c("lat", "lon")]
  sp[!duplicated(sp_dups),]
  write.csv(sp, paste(paste(bin_bind[i,1], bin_bind[i,2], sep="_"), ".csv", sep=""))
  
}




