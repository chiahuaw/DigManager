library(dplyr)
library(plyr)
library(reshape)

# from:http://shiny.rstudio.com/gallery/unicode-characters.html
# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
      system('locate wqy-zenhei.ttc') != 0 &&
      !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    shiny:::download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)

####載入資料####

f <- file("data/totalcase.csv",encoding="UTF-8")
dig<-read.csv(f,stringsAsFactors = F)
f2 <- file("data/polygons.csv",encoding="UTF-8")
geolist<-read.csv(f,stringsAsFactors = F)
f3 <- file("data/apping.csv",encoding="UTF-8")
apping<-read.csv(f,stringsAsFactors = F)
f4<-file("data/buildingGeo.csv",encoding="big5")
building <- read.csv(f4,encoding="big5",stringsAsFactors=F)


#完成日期的NA值處理。
dig$CompletionDate[is.na(dig$CompletionDate)]<-""
#設定日期屬性
dig<-mutate(dig,
            AppDate=as.Date(AppDate),
            StartDate=as.Date(StartDate),
            EndDate=as.Date(EndDate),
            IssuanceDate=as.Date(ApprovedStartDate),
            ApprovedStartDate=as.Date(ApprovedStartDate),
            ApprovedEndDate=as.Date(ApprovedEndDate)
            )
dig<-arrange(dig,AppDate)
####施工中####
working<-filter(dig,
                ApprovedStartDate<=Sys.Date(),ApprovedEndDate>=Sys.Date())
####未結案####
finish<-filter(dig,
               CompletionDate=="",ApprovedEndDate <= Sys.Date(),AppDate>=as.Date("2014-09-12")) %>% 
  filter(.,Sys.Date()-ApprovedEndDate<=30)
####已逾期####
delay<-filter(dig,
              CompletionDate=="",ApprovedEndDate <= Sys.Date(),AppDate>=as.Date("2014-09-12")) %>% 
  filter(.,Sys.Date()-ApprovedEndDate>30)

####tempdata####
#1
tempweekprocess<-filter(working,ApprovedEndDate>=Sys.Date(),ApprovedEndDate<=Sys.Date()+7)
tempweekprocess$AppNo<-as.factor(as.character(tempweekprocess$AppNo))
#2
tempweek2process<-filter(finish,ApprovedEndDate+30<=Sys.Date()+7)
tempweek2process$AppNo<-as.factor(as.character(tempweek2process$AppNo))
####續證次數####
# extension<-filter(dig,
#                   AppDate>=as.Date("2015-01-01")) %>% 
#   filter(.,AppNo %in% patrol$AppNo)
# extension<-filter(patrol,AppNo %in% extension$AppNo)
# extension<-as.data.frame(cast(extension,AppNo~.,fun.aggregate = table))
# names(extension)<-c("AppNo","times")
