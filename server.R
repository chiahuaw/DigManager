
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

thm <- function() {
  theme_gray(base_family = "STHeiti") + # 讓Mac使用者能夠顯示中文, Windows使用者應省略這行
    theme(text=element_text(size=18))} # 將字體調整至18號

source("global.R")

shinyServer(function(input, output) {

  
  output$yearmap <- renderLeaflet({
    a<-filter(geolist,case %in% dig$AppNo[dig$AppDate>=as.Date("2015-01-01")]) %>% filter(.,case %in% working$AppNo)
    b<-filter(geolist,case %in% dig$AppNo[dig$AppDate>=as.Date("2015-01-01")]) %>% 
      filter(.,case %in% dig$AppNo[dig$ApprovedEndDate<=Sys.Date() | dig$ApprovedStartDate>=Sys.Date()] )
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
     addCircles(lng=a$lon,lat=a$lat,color = "#ff008d",radius=45,popup=paste(a$case,"/施工時間：",dig$ApprovedStartDate[match(a$case,dig$AppNo)],
                                                "至",dig$ApprovedEndDate[match(a$case,dig$AppNo)],"/單位：",dig$AppUnitNo[match(a$case,dig$AppNo)])) %>%
      addCircles(lng=b$lon,lat=b$lat,popup=paste(b$case,"/施工時間：",dig$ApprovedStartDate[match(b$case,dig$AppNo)],
                                                 "至",dig$ApprovedEndDate[match(b$case,dig$AppNo)],"/單位：",dig$AppUnitNo[match(b$case,dig$AppNo)])) %>%
      addCircles(lng=apping$lon,lat=apping$lat,color="#47e117",radius=15,popup=paste(apping$AppNo,"/申請時間：",apping$StartDate,
                                                 "至",apping$EndDate,"/單位：",apping$AppUnitNo))
     
  })
  
  output$yeartable<-renderDataTable(dig[dig$AppDate>=as.Date("2015-01-01"),c(2,3,5,11,12,13,15,16,17,34)])
  
  output$workingmap <- renderLeaflet({
    
    a<-filter(geolist,case %in% working$AppNo)
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(lng=a$lon,lat=a$lat,color = "#ff008d",radius=45,
                 popup=paste(a$case,"/施工時間：",dig$ApprovedStartDate[match(a$case,dig$AppNo)],"至",
                             dig$ApprovedEndDate[match(a$case,dig$AppNo)],"/單位：",dig$AppUnitNo[match(a$case,dig$AppNo)]))
    
  })
  
  output$yearprocess<- renderPlot({
    working$AppNo<-as.factor(working$AppNo)
    apping$AppNo<-as.factor(apping$AppNo)
    p <- ggplot(working, aes(colour=AppUnitNo))
    p <- p + theme_bw()
    p <- p + geom_segment(aes(x=ApprovedStartDate, 
                              xend=ApprovedEndDate, 
                              y=AppNo, 
                              yend=AppNo), 
                          size=2)
    p <- p + geom_point(aes(x=ApprovedStartDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_point(aes(x=ApprovedEndDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_text(aes(x=ApprovedStartDate,
                           y=as.numeric(AppNo)+0.25,
                           label=paste(working$ApprovedEndDate,"/",working$LocationRoadName)),
                       fontface="bold")+thm()
    #p <- p + geom_vline(aes(x=as.Date("2015-09-12"),color="red",label="Today"))
    p <- p + xlab("Duration")
    p
    
  })
  
  output$workingtable<-renderDataTable(working[,c(2,3,5,11,12,13,15,16,17,34)])
  
  output$weekmap <- renderLeaflet({
    tempweek<-filter(working,ApprovedEndDate>=Sys.Date(),ApprovedEndDate<=Sys.Date()+7)
    tempweek$AppNo<-as.factor(as.character(tempweek$AppNo))
    a<-filter(geolist,case %in% tempweek$AppNo)
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(lng=a$lon,lat=a$lat,color = "#9705FF",radius=45,
                 popup=paste(a$case,"/施工時間：",dig$ApprovedStartDate[match(a$case,dig$AppNo)],"至",
                             dig$ApprovedEndDate[match(a$case,dig$AppNo)],"/單位：",dig$AppUnitNo[match(a$case,dig$AppNo)]))
    
  })
  
  output$weekprocess<- renderPlot({
    
    #from tempdata#1
    
    p <- ggplot(tempweekprocess, aes(colour=AppUnitNo))
    p <- p + theme_bw()
    p <- p + geom_segment(aes(x=ApprovedStartDate, 
                              xend=ApprovedEndDate, 
                              y=AppNo, 
                              yend=AppNo), 
                          size=2)
    p <- p + geom_point(aes(x=ApprovedStartDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_point(aes(x=ApprovedEndDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_text(aes(x=ApprovedStartDate,
                           y=as.numeric(AppNo)+0.25,
                           label=paste(tempweekprocess$ApprovedEndDate,"/",tempweekprocess$LocationRoadName)),
                       fontface="bold")+thm()
    #p <- p + geom_vline(aes(x=as.Date("2015-09-12"),color="red",label="Today"))
    p <- p + xlab("Duration")
    p
    
  })
  
  output$weektable<-renderDataTable({
    
    #from tempdata#1
    
    tempweekprocess[,c(2,3,5,11,12,13,15,16,17,34)]})
  
  output$week2process<- renderPlot({
    
    #from tempdata#2
    
    p <- ggplot(tempweek2process, aes(colour=AppUnitNo))
    p <- p + theme_bw()
    p <- p + geom_segment(aes(x=ApprovedStartDate, 
                              xend=ApprovedEndDate, 
                              y=AppNo, 
                              yend=AppNo), 
                          size=2)
    p <- p + geom_point(aes(x=ApprovedStartDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_point(aes(x=ApprovedEndDate,
                            y=AppNo),
                        size=5)
    p <- p + geom_text(aes(x=ApprovedStartDate,
                           y=as.numeric(AppNo)+0.25,
                           label=paste(tempweek2process$ApprovedEndDate,"/",tempweek2process$LocationRoadName)),
                       fontface="bold")+thm()
    #p <- p + geom_vline(aes(x=as.Date("2015-09-12"),color="red",label="Today"))
    p <- p + xlab("Duration")
    p
    
  })
  
  output$week2table<-renderDataTable({
    
    #from tempdata#2
    
    tempweek2process[,c(2,3,5,11,12,13,15,16,17,34)]})
  
  output$colseprocess<- renderPlot({
    finish<-arrange(finish,AppUnitNo,ApprovedEndTime)
    p <- ggplot(finish,aes(colour=AppUnitNo))+
      geom_bar(aes(x=AppUnitNo,fill=AppUnitNo))+thm()
    p
  })
  
  output$closetable<-renderDataTable(finish[,c(2,3,5,11,12,13,15,16,17,34)])
  
  output$delaycase<- renderPlot({
    delay<-arrange(delay,AppUnitNo,ApprovedEndTime)
    p <- ggplot(delay,aes(colour=AppUnitNo))+
      geom_bar(aes(x=AppUnitNo,fill=AppUnitNo))+thm()
    p
  })
  
  output$delaytable<-renderDataTable(delay[,c(2,3,5,11,12,13,15,16,17,34)])
  
  output$rchecktable <- renderDataTable({
    
    #隨機挑選案號
    rcase<<-working[sample(seq(1,nrow(working),1),input$runcase),]
    
  })
  
  output$rcheckmap <- renderLeaflet({
    a<-filter(geolist,case %in% rcase$AppNo)
  
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(lng=a$lon,lat=a$lat,color = "#ff008d",radius=45,
                 popup=paste(a$case,"/施工時間：",dig$ApprovedStartDate[match(a$case,dig$AppNo)],"至",
                             dig$ApprovedEndDate[match(a$case,dig$AppNo)],"/單位：",dig$AppUnitNo[match(a$case,dig$AppNo)]))
    
  })
  
  output$rcheckdownloadData <- downloadHandler(
    filename = function() { paste('RandomCheck_',input$checkdate,'.csv',sep="")},
    content = function(file) {
      write.csv(rcase, file,row.names=FALSE,fileEncoding = "big5")
    }
  )
  
  output$buildmap <- renderLeaflet({
    a<-building[as.Date(building$發文日期)>=as.Date(input$times[1]) & as.Date(building$發文日期)<=as.Date(input$times[2]),]
    b<-filter(geolist,case %in% apping$AppNo)
    
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(lng=a$lon,lat=a$lat,color = "#ff008d",radius=10,
                 popup=paste("建號：",a[,5],"/發文時間：",a[,3])) %>%
      addCircles(lng=apping$lon,lat=apping$lat,color="#47e117",radius=45,popup=paste(apping$AppNo,"/申請時間：",apping$StartDate,
                                                                                     "至",apping$EndDate,"/單位：",apping$AppUnitNo))
    
    
  })
  
  #output$talk <-renderText( "這是一個說明頁。" )

})
