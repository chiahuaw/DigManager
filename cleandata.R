library("dplyr")
library("reshape")

#### 載入資料 ####
#list.files("~/Dropbox/Data/dig")
#system("iconv -f big5 -t utf8 ~/Dropbox/Data/dig/appcase.csv > ~/Dropbox/Data/dig/appcase_utf8.csv")
f<-file("~/Dropbox/Data/dig/AppCase.csv",encoding = "big5")
f2<-file("~/Dropbox/Data/dig/CaseIssuance.csv",encoding = "big5")
f3<-file("~/Dropbox/Data/dig/CaseCompletion.csv",encoding = "big5")
#f4<-file("~/Dropbox/Data/dig/CaseEvaluation.csv",encoding = "big5") 記載繳費金額
f5<-file("~/Dropbox/Data/dig/CaseInspection.csv",encoding = "big5")
f6<-file("~/Dropbox/Data/dig/CasePatrol.csv",encoding = "big5")
f7<-file("~/Dropbox/Data/dig/CaseRejection.csv",encoding = "big5")
f8<-file("~/Dropbox/Data/dig/AppUnit.csv",encoding = "big5")

appcase<-read.csv(f,stringsAsFactors = F,na.strings = "",allowEscapes = T) #申請
issuance<-read.csv(f2,stringsAsFactors = F,na.strings = "") #核准
completion<-read.csv(f3,stringsAsFactors = F,na.strings = "") #完工
#evaluation<-read.csv(f4,stringsAsFactors = F,na.strings = "")
inspection<-read.csv(f5,stringsAsFactors = F,na.strings = "") #完工勘驗 inspection[1,]
patrol<-read.csv(f6,stringsAsFactors = F,na.strings = "") #續證巡查 patrol[1,]
rejection<-read.csv(f7,stringsAsFactors = F,na.strings = "") #退回申請 rejection[1,]
appunit<-read.csv(f8,stringsAsFactors = F,na.strings = "")

#### 資料分析 ####
#我會想知道什麼？準時完工申報、管線異動
#要知道這些，我需要什麼資料？案件申請編號、核准編號、核准日期、
#管線異動(appcase$IsPipeUpdated)、申報完工時間



#找出已核准案件資料。
temp<-filter(appcase,appcase$AppNo %in% issuance$AppNo)

start<-Sys.time()
source("~/Dropbox/Git/Dig/getBoud.R") #取得每個案子的第一個座標
Sys.time()-start

for (i in 1:nrow(temp)) {
  temp$AppUnitNo[i]<-appunit$AppUnitName[grepl(temp$AppUnitNo[i],appunit$AppUnitNo)]
}

temp<-select(temp,AppNo,AppDate,IsPipeUpdated,AppUnitNo,DistrictNo,StartDate,EndDate,StartTime,EndTime,
             Purpose,LocationRoadName,LocationDoorplate)
issuance<-arrange(issuance,AppNo,desc(IssuanceDate))
completion<-arrange(completion,AppNo,desc(CompletionDate))

totalcase<-data.frame()
for (i in 1:nrow(temp)) {

  totalcase<-rbind(totalcase,
               cbind(temp[i,],
                     issuance[match(temp$AppNo[i],issuance$AppNo),c(4:23)],
                     completion[match(temp$AppNo[i],completion$AppNo),c(5,7)])
               )
}

for (i in c(2,6,7,14,15,16,33)) {
  totalcase[,i]<-as.Date(totalcase[,i])
}

totalcase$appyear<-format(totalcase$AppDate,format="%Y")
totalcase$workyear<-format(totalcase$ApprovedStartDate,format="%Y")

totalcase<-arrange(totalcase,AppDate)

# totalcase$lon<-""
# totalcase$lat<-""
# for (i in 1:nrow(totalcase)) {
#   a<-match(totalcase$AppNo[i],polygons$case)
#   totalcase$lon[i]<-polygons$lon[a]
#   totalcase$lat[i]<-polygons$lat[a]
#   if (i==nrow(totalcase)) { rm(a) }
# }

write.csv(totalcase,file="data/totalcase.csv",fileEncoding = "UTF-8")
write.csv(polygons,file="data/polygons.csv",fileEncoding = "UTF-8")
write.csv(boudgeo,file="data/boudgeo.csv",fileEncoding = "UTF-8")

rm(temp)
####申請中####
temp<-filter(appcase,
             (appcase$AppNo %in% issuance$AppNo)==F,
             is.na(appcase$AppDate)==F,
             (appcase$AppNo %in% rejection$AppNo)==F) %>% 
  arrange(.,desc(as.Date(AppDate))) %>%
  filter(.,Sys.Date()-as.Date(AppDate)<=90)

for (i in 1:nrow(temp)) {
  temp$AppUnitNo[i]<-appunit$AppUnitName[grepl(temp$AppUnitNo[i],appunit$AppUnitNo)]
}

start<-Sys.time()
source("~/Dropbox/Git/Dig/getBoud.R") #取得每個案子的第一個座標
Sys.time()-start

temp$lon<-""
temp$lat<-""

for (i in 1:nrow(temp)) {
  temp$lon[i]<-polygons[grepl(temp$AppNo[i],polygons$case),]$lon
  temp$lat[i]<-polygons[grepl(temp$AppNo[i],polygons$case),]$lat
}

apping<-temp

rm(temp)

write.csv(apping,file="data/apping.csv",fileEncoding = "UTF-8",row.names = F)

####描述、統計、dashboard####
#以年為單位進行
#缺完工申報日期、完工查驗日期



#管線異動
nrow(filter(totalcase,totalcase$IsPipeUpdated==1,totalcase$AppDate>=as.Date("2014-09-12")))/nrow(totalcase)
1/nrow(filter(totalcase,totalcase$IsPipeUpdated==1,totalcase$AppDate>=as.Date("2014-09-12")))

#準時完工。定義：申報完工日<=30日
filter(totalcase,
       totalcase$AppDate>=as.Date("2014-09-12")
       ) %>% 
  mutate(.,closedate=.$CompletionDate-.$ApprovedEndDate) %>%
  filter(.,!is.na(.$closedate),.$closedate<=30) %>% nrow(.) ->a

a/nrow(filter(totalcase,totalcase$AppDate>=as.Date("2014-09-12"),totalcase$ApprovedEndDate<=Sys.Date()))
