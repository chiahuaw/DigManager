
library(leaflet)
library(shiny)

shinyUI(navbarPage("金門縣道路挖掘資訊圖表看板",
                   tabPanel("當期案件檢視列管",
                            titlePanel("總覽"),
                            navlistPanel("查詢別",
                                         tabPanel("當年度申請案總覽",
                                                  leafletOutput("yearmap"),
                                                  dataTableOutput("yeartable")),
                                         tabPanel("施工中案件時間圖表",
                                                  leafletOutput("workingmap"),
                                                  plotOutput("yearprocess"),
                                                  dataTableOutput("workingtable")),
                                         tabPanel("未來七天到期案件圖表",
                                                  leafletOutput("weekmap"),
                                                  plotOutput("weekprocess"),
                                                  dataTableOutput("weektable")),
                                         tabPanel("完工但未結案管制圖表",
                                                  plotOutput("colseprocess"),
                                                  dataTableOutput("closetable")),
                                         tabPanel("未來七天結案到期案件圖表",
                                                  plotOutput("week2process"),
                                                  dataTableOutput("week2table")),
                                         tabPanel("逾期未結管制圖表",
                                                  plotOutput("delaycase"),
                                                  dataTableOutput("delaytable"))
                                         
                            )
                            
                   ),
                   tabPanel("施工督導隨機抽選",
                            navlistPanel("新建案申挖輔助檢視比對",
                                         tabPanel("步驟1：施工督導隨機抽選",
                                                  sliderInput("runcase",
                                                              "請選擇抽選案件數",
                                                              min=1,
                                                              max=length(unique(working$AppNo)),
                                                              value=2,
                                                              step=1)),
                                                  dateInput("checkdate","預定督導日期：",value=Sys.Date()),
                                                  #print(paste("系統時間：",Sys.Date())),
                                         tabPanel("步驟2：抽選結果",
                                                  leafletOutput("rcheckmap"),
                                                  dataTableOutput("rchecktable"),
                                                  downloadButton('rcheckdownloadData', '下載抽選結果CSV檔'))
                                         )
                   ),
                   tabPanel("新建案申挖整合輔助比對",
                            navlistPanel("新建案申挖整合輔助比對",
                                         tabPanel("步驟1：設定建案發照時間",
                                                  dateRangeInput("times","時間區間：",
                                                                 start=as.Date(paste(as.numeric(format(max(as.Date(building$發文日期)),format="%Y"))-1,"-01-01",sep="")),
                                                                 end=max(as.Date(building$發文日期)),
                                                                 min=min(as.Date(building$發文日期)),
                                                                 max=max(as.Date(building$發文日期))
                                                                 )
                                                  ),
                                         #print(paste("系統時間：",Sys.Date())),
                                         tabPanel("步驟2：顯示套疊地圖",
                                                  leafletOutput("buildmap"))
                                         
                                         )
                            ),
                   tabPanel("統計分析圖表"
                            )#統計分析圖表tabPanel
                   
                   )
  
)
