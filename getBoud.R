library(reshape)

boudgeo<-data.frame()
source("~/Dropbox/dsp/t97w84.R")

for (i in 1:nrow(temp)) {
  
  #if (appcase$Boundary[i]!="NA" | grepl("EMPTY",appcase$Boundary[i])==F) { 
    boud<-strsplit(temp$Boundary[i],split="\\(\\(")
    boud<-unlist(boud)
    boud<-gsub("\\(",replacement="",boud)
    boud<-gsub("\\)",replacement="",boud)
    boud<-boud[-1]
    boud<-strsplit(boud,split=",")
    
    for (t in 1:length(boud)) {
      boudlist<-boud[[t]]
      boudlist<-strsplit(boudlist,split=" ")
      boudlist<-unlist(boudlist)
      
      boudend<-list()
      
      for (d in 1:length(boudlist)) {
        if (boudlist[d]!="") {
          boudend<-rbind(boudend,boudlist[d])
        }
      }
      
      
      for (e in seq(1,length(boudend),2)) {
        a<-rbind(t97w84(as.numeric(boudend[e]),as.numeric(boudend[e+1])))
        a<-as.data.frame(a)
        a<-cbind(temp$AppNo[i],t,a)
        boudgeo<-rbind(boudgeo,a)
        print(paste(e,"/",i))
        rm(a)
      }
      
     }
  #}
 rm(boud)
 rm(boudend)
}

names(boudgeo)<-c("case","class","lon","lat")
#boudgeo$polygon<-paste(boudgeo$lat,boudgeo$lon,sep=",")

###0911test2####
polygons<-as.data.frame(cast(boudgeo,case+class~.))
polygons$lon<-""
polygons$lat<-""

for (i in 1:nrow(polygons)) {
  temp1<-polygons[i,c(1,2)]
  
  for (t in 1:nrow(boudgeo)) {
    if (grepl(temp1[1,1],boudgeo[t,1])==T & grepl(temp1[1,2],boudgeo[t,2])==T) {
      polygons$lon[i]<-boudgeo$lon[t]
      polygons$lat[i]<-boudgeo$lat[t]
      break
    }
  }
  if(i==nrow(polygons)) { rm(temp1)}
  print(paste(i))
}

polygons$caseclass<-paste(polygons$case,"-",polygons$class,sep="")
# ####0911test1####
# polygons<-as.data.frame(cast(boudgeo,case+class~.))
# polygons$poly<-""
# polygons$lon<-""
# polygons$lat<-""
# 
# for (i in 1:nrow(polygons)) {
#   temp1<-polygons[i,c(1,2)]
#   
#   for (t in 1:nrow(boudgeo)) {
#     if (grepl(temp1[1,1],boudgeo[t,1])==T & grepl(temp1[1,2],boudgeo[t,2])==T) {
#       #polygons$poly[i]<-paste(polygons$poly[i],boudgeo$polygon[t])
#       polygons$lon[i]<-paste(polygons$lon[i],boudgeo$lon[t],sep=",")
#       polygons$lat[i]<-paste(polygons$lat[i],boudgeo$lat[t],sep=",")
#     }
#   }
#   if(i==nrow(polygons)) { rm(temp1)}
#   print(paste(i))
# }
# 
# polygons$lon<-sub(",",replacement="",polygons$lon)
# polygons$lat<-sub(",",replacement="",polygons$lat)
