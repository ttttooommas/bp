library("shiny")
library("DT")
library("ggplot2")
library("readxl")
library("tidyverse")
library("shinythemes")
library("scales")
library("optimx")
library("shinyjs")

initJS <- '
shinyjs.init = function(){
    $(selector = "#tabs li a[data-value=tab1]").hide();
$(selector = "#tabs li a[data-value=tab05]").hide();
$(selector = "#tabs li a[data-value=tab21]").hide();
$(selector = "#tabs li a[data-value=tab22]").hide();
$(selector = "#tabs li a[data-value=tab3]").hide();
$(selector = "#tabs li a[data-value=tab4]").hide();
}
'

loadScreen <- "
+#loading-content {
+  position: absolute;
+  background: #000000;
+  opacity: 0.9;
+  z-index: 100;
+  left: 0;
+  right: 0;
+  height: 100%;
+  text-align: center;
+  color: #FFFFFF;
+}
+"

#DT gip number to millions
formatMillions <- JS(
  "function(values$globalTable) {",
  "return (values$globalTable / 1000000) + ' Mil.'",
  "}")

# UI
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = initJS),
  #vyber sablony
  theme = shinytheme("simplex"), #cerulean
   
   # Title
   #titlePanel("Projekce hysterezní křivky"),

        #TABS
        tabsetPanel(id="tabs",
                    #TAB Load file
                    tabPanel("Načíst/Uložit Data",value="tab0",fluid=TRUE,
                             
                                              mainPanel(
                                                h2("Vyberte způsob načtení dat / uložení"),
                                                fluidRow(
                                                  column(8,h3("Načtení dat"),
                                                         fluidRow(
                                                         column(6,h4("A) Načíst data z vašeho PC"),
                                                                fileInput("fileIn","1. Vyber svůj XLSX soubor",accept = c('text/xls','text/xlsx','.xlsx','.xls')),
                                                                p("2. Potvrď tlačítkem"),
                                                                actionButton("loadDataFromPC", "Načíst data")
                                                         ),
                                                         column(6,h4("B) Načíst data ze serveru"),
                                                                actionButton("loadData", "Načíst data")
                                                         ))
                                                  ),
                                                  column(4,h3("Uložení dat"),
                                                         p("Uložit data na server"),
                                                         actionButton("saveData", "Uložit data")
                                                  )
                                                )
                                              )             
                             ),
                    #TAB Parametry zasobniku
                    tabPanel("Parametry zásobníku",value="tab05", fluid=TRUE,
                               
                               mainPanel(
                                 uiOutput("datesALL"),
                                 sliderInput("calmDaysALL",
                                             "Min počet dní klidového stavu zásobníku:",
                                             min = 0,
                                             max = 10,
                                             value = 4),
                                 h4("Výchozí parametry:"),
                                 uiOutput("inputVychoziGip"),
                                 uiOutput("inputVychoziTeplota"),
                                 uiOutput("inputVychoziObjem"),
                                 uiOutput("inputVychoziHloubka"),
                                 actionButton("SetDefaultValues", "Nastavit jako výchozí hodnoty")
                               )),
                    #TAB plots in time
                    tabPanel("Grafy vývoje v čase",value="tab3",
                             fluid = TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 sliderInput("forCoefA",
                                             "Počet dní pro výpočet Alpha:",
                                             min = 1,
                                             max = 30,
                                             value = 10),
                                 selectInput("color2","Barva křivky", choices=c("Červená"="red","Modrá"="blue","Zelená"="green","Černá"="black"), selected="black"),
                                 #radioButtons("radio", "Grafy",
                                 #             choices = list("GIP v čase" = 1, "TlakBHP v čase" = 2, "P/z v čase" = 3, "G/(P/z) v čase" = 4,"GIPe v čase" = 5), 
                                 #             selected = 1),
                                 checkboxInput("checkboxGIP", "Vývoj GIP v čase", TRUE),
                                 checkboxInput("checkboxTlak", "Vývoj Tlaku v čase", TRUE),
                                 checkboxInput("checkboxPz", "Vývoj P/z v čase", TRUE),
                                 checkboxInput("checkboxGpz", "Vývoj G/(P/z) v čase", TRUE),
                                 checkboxInput("checkboxGIPe", "Vývoj GIPe v čase v čase", TRUE),
                                 actionButton(inputId = "useCheckboxes", label = "Zobrazit vybrané"),
                                 width = 3
                               ),
                               mainPanel(
                                 div(id="loading-content3",
                                     h1("Načítání...")),
                                 plotOutput("gipPlot"),
                                 plotOutput("tlakPlot"),
                                 plotOutput("pzPlot"),
                                 plotOutput("gpzPlot"),
                                 plotOutput("gipePlot"),
                                 plotOutput("PlotCoefA"),
                                 plotOutput("PlotCoefA2"),
                                 h4("Stažení grafu GIP v case:"),
                                 #br(),
                                 downloadButton(outputId="gipPlot_downloadPdf", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="gipPlot_downloadPng",label="Uložit graf jako Png"),
                                 h4("Stažení grafu Tlak v case:"),
                                 #br(),
                                 downloadButton(outputId="tlakPlot_downloadPdf", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="tlakPlot_downloadPng",label="Uložit graf jako Png"),
                                 h4("Stažení grafu P/Z v case:"),
                                 #br(),
                                 downloadButton(outputId="pzPlot_downloadPdf", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="pzPlot_downloadPng",label="Uložit graf jako Png"),
                                 h4("Stažení grafu G/P/z v case:"),
                                 #br(),
                                 downloadButton(outputId="gpzPlot_downloadPdf", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="gpzPlot_downloadPng",label="Uložit graf jako Png"),
                                 h4("Stažení grafu GIPe v case:"),
                                 #br(),
                                 downloadButton(outputId="gipePlot_downloadPdf", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="gipePlot_downloadPng",label="Uložit graf jako Png"),
                                 #h4("Stažení grafu 1:"),
                                 #br(),
                                 #downloadButton(outputId="downloadPdf_gip", label="Uložit graf jako Pdf"),
                                 #downloadButton(outputId="downloadPng_gip",label="Uložit graf jako Png"),
                                 h4("Stažení grafu Alpha:"),
                                 #br(),
                                 downloadButton(outputId="downloadPdf_alpha", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="downloadPng_alpha",label="Uložit graf jako Png"),
                                 h4("Stažení grafu 1/Alpha:"),
                                 #br(),
                                 downloadButton(outputId="downloadPdf_alpha2", label="Uložit graf jako Pdf"),
                                 downloadButton(outputId="downloadPng_alpha2",label="Uložit graf jako Png")
                               ))),
                    
                    #TAB Hyst krivka
                    tabPanel("Projekce hystereze",value="tab1",fluid = TRUE,
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("color","Barva křivky", choices=c("Červená"="red","Modrá"="blue","Zelená"="green","Černá"="black"), selected="black"),
                                 checkboxInput("checkboxHystDates", "Vykreslit do hysterezní křivky datum", FALSE),
                                 checkboxInput("checkboxProjekceDates", "Vykreslit do projekce datum", TRUE),
                                 
                                 h4("Hodnota Alpha:"),
                                 uiOutput("alpha"),
                                 
                                 width = 3
                               ),
                               mainPanel(
                                  fluidRow(
                                    div(id="loading-content2",
                                        h1("Načítání...")),
                                      column(7,h5("Vyber oblast a klikni pro zoom"),plotOutput("mujPlot",dblclick = "plot1_dblclick",
                                                                                        brush = brushOpts(
                                                                                          id = "plot1_brush",
                                                                                          resetOnNew = TRUE
                                                                                        ), height=500),
                                        plotOutput("PlotProjekce"),
                                        h4("Stažení grafu 1:"),
                                        br(),
                                        downloadButton(outputId="downloadPdf1", label="Uložit graf jako Pdf"), 
                                        downloadButton(outputId="downloadPng1", label="Uložit graf jako Png"),
                                        h4("Stažení grafu 2:"),
                                        br(),
                                        downloadButton(outputId="downloadPdf2", label="Uložit graf jako Pdf"), 
                                        downloadButton(outputId="downloadPng2", label="Uložit graf jako Png")
                                      ),
                                      column(5,DT::dataTableOutput("tableFilterDT"))
                                  )
                               )
                          )
                    ),
                    # #TAB Analýza vývoje tlaku v čase
                    # tabPanel("Analýza vývoje tlaku v čase",value="tab2",
                    #          fluid = TRUE,
                    #          sidebarLayout(
                    #            sidebarPanel(
                    #              uiOutput("datesAVT"),
                    #              sliderInput("calmDaysAVT",
                    #                          "Min počet dní vklidu:",
                    #                          min = 0,
                    #                          max = 10,
                    #                          value = 3),
                    #              width = 3
                    #            ),
                    #            mainPanel(
                    #              fluidRow(
                    #                column(7,
                    #                       div(id="loading-content",
                    #                           h1("Načítání...")),
                    #                       plotOutput("koefK"),
                    #                       plotOutput("koefC"),
                    #                       plotOutput("vyvojObdobi"),
                    #                       plotOutput("seasons")
                    #                       ),
                    #                column(5,DT::dataTableOutput("tableOptim"),
                    #                       h4("Stažení grafu:"),
                    #                       br()
                    #                       )
                    #              )
                    #            ))),
                    
                    #TAB Analýza vývoje tlaku v čase
                    tabPanel("Analýza vývoje P - vývoj koeficientů",value="tab21",
                               mainPanel(
                                          div(id="loading-content",
                                              h1("Načítání...")),
                                          plotOutput("koefK"),
                                          plotOutput("koefC"),
                                          plotOutput("seasons")
                               )),
                    #TAB Analýza vývoje tlaku v čase
                    tabPanel("Analýza vývoje P - vývoj jednotlivých období",value="tab22",
                               mainPanel(
                                 fluidRow(
                                   column(8,
                                          plotOutput("vyvojObdobi")
                                   ),
                                   column(4,DT::dataTableOutput("tableOptim"),
                                          h4("Stažení grafu:"),
                                          br()
                                   )
                                 )
                               )),
                    
                    #TAB Tabulka
                    tabPanel("Upravit tabulku záznamů",value="tab4", fluid=TRUE,
                             sidebarLayout(
                               
                               mainPanel(
                                 h1("Tabulka záznamů"),
                                 DT::dataTableOutput("tableDT")
                               ),
                               sidebarPanel(
                                        h1("Upravit záznam"),
                                        uiOutput("inputDatum"),
                                        uiOutput("inputVtlaceni"),
                                        uiOutput("inputTezba"),
                                        uiOutput("inputGip"),
                                        uiOutput("inputZfaktorBHP"),
                                        actionButton("aktualizovat", "Aktualizovat záznam"),
                                        width=4
                                        
                                        
                                    
                                  )
                               )
                             
                    )
                
        ) #tabsetPanel
  ) #UI

# SERVER
server <- function(input, output) {
  values <- reactiveValues()
  values$setupComplete <- TRUE
  
  output$setupComplete <- reactive({
    return(values$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
  observeEvent(input$aktualizovat, {
    values$globalTable <- changeDataValues(input$tableDT_rows_selected[1],as.numeric(input$inVtlaceni),as.numeric(input$inTezba), as.numeric(input$inGip), as.numeric(input$inZfaktorBHP),values$globalTable)
    print("updated")
  })
  
  #NOVINKA vychozi hodnoty jejich ulozeni
  observeEvent(input$SetDefaultValues, {
    values$globalVychoziGip <- as.numeric(input$inVychoziGip)
    values$globalVychoziTeplota <- as.numeric(input$inVychoziTeplota)
    values$globalVychoziObjem <- as.numeric(input$inVychoziObjem)
    values$globalVychoziHloubka <- as.numeric(input$inVychoziHloubka)
    print("Updated Default Values")
    #values$GIPe <- countGIPe()
    values$globalTable["GIPe"]<- countGIPe()
    #print(values$globalTable)
    #print(values$globalVychoziGip)
    #print(values$globalVychoziTeplota)
    #print(values$globalVychoziObjem)
    #print(values$globalVychoziHloubka)
    })
  #NOVINKA checkboxes 
  observeEvent(input$useCheckboxes, {
    
    if(input$checkboxGIP){
      show("gipPlot")
    }else{
      hide("gipPlot")
    }
    if(input$checkboxTlak){
      show("tlakPlot")
    }else{
      hide("tlakPlot")
    }
    if(input$checkboxPz){
      show("pzPlot")
    }else{
      hide("pzPlot")
    }
    if(input$checkboxGpz){
      show("gpzPlot")
    }else{
      hide("gpzPlot")
    }
    if(input$checkboxGIPe){
      show("gipePlot")
    }else{
      hide("gipePlot")
    }
  })
  countGIPe <- function(){
    #forcyklus pro spocitani GIPe
    GIPe = vector(,nrow(values$globalTable))
    GIPe[1]<-values$globalVychoziGip+values$globalTable[1,"Vtlaceni"]-values$globalTable[1,"Tezba"]
    for(index in 2:nrow(values$globalTable)){
      GIPe[index] <- GIPe[index-1]+values$globalTable[index,"Vtlaceni"]-values$globalTable[index,"Tezba"]
    }
    return(GIPe)
  }
  
  changeDataValues <- function(row_id, vtlaceni, tezba, gip, ZfaktorBHP, data){
    data$Vtlaceni[row_id] <- vtlaceni
    data$Tezba[row_id] <- tezba
    data$GIP[row_id] <- gip
    data$ZfaktorBHP[row_id] <- ZfaktorBHP
    return(data)
  }
  
  #ulozeni a nacteni dat na serveru (data uklada/nacita slozky responses)
  
  observeEvent(input$saveData, {
    saveData(values$globalTable)
  })
  
  observeEvent(input$loadData, {
    data <- loadData()
    values$globalTable <- data
    show(selector = "#tabs li a[data-value=tab1]")
    show(selector = "#tabs li a[data-value=tab05]")
    show(selector = "#tabs li a[data-value=tab21]")
    show(selector = "#tabs li a[data-value=tab22]")
    show(selector = "#tabs li a[data-value=tab3]")
    show(selector = "#tabs li a[data-value=tab4]")
    optimization()
  })
  
  observeEvent(input$loadDataFromPC, {
    values$globalTable <- uploadDataFromFile()
    print("Data loaded from PC")
    showModal(modalDialog(
      title = "Načtení Dat",
      "Data úspěšně načtena!",
      easyClose = TRUE
    ))
    show(selector = "#tabs li a[data-value=tab1]")
    show(selector = "#tabs li a[data-value=tab05]")
    show(selector = "#tabs li a[data-value=tab21]")
    show(selector = "#tabs li a[data-value=tab22]")
    show(selector = "#tabs li a[data-value=tab3]")
    show(selector = "#tabs li a[data-value=tab4]")
    values$globalTable["GIPe"]<- 0
    optimization()
  })
  saveData <- function(data) {
    if(!is.null(data)){
      fileName <- "data.csv"
      if(!dir.exists(file.path("saved"))){
        print("creating subdirectory /saved ...")
        dir.create(file.path("saved"))
      }
      write.csv(
        x = data,
        file = file.path("saved", fileName), 
        row.names = FALSE, quote = TRUE
      )
      print("Data saved")
      showModal(modalDialog(
        title = "Uložení Dat",
        "Data uložena na server!",
        easyClose = TRUE
      ))
    }
    else{
      showModal(modalDialog(
        title = "Chyba ukládání",
        "Žádná data k uložení, data nebyla uložena!",
        easyClose = TRUE
      ))
      print("Nejsou data k ulozeni")
    }
    
  }
  
  loadData <- function() {
    data <- tryCatch({
      data <- read.csv("saved/data.csv")
      print("Data loaded")
      showModal(modalDialog(
        title = "Načtení Dat",
        "Data úspěšně načtena!",
        easyClose = TRUE
      ))
      return(data)
    }, warning = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Žádná data k načtení!",
        easyClose = TRUE
      ))
      print("Žádná data na serveru!")
      NULL
    })
    data
  }
  
  #render UI
  output$row_id <- renderText({
    input$tableDT_rows_selected[1]
  })
  output$inputDatum <- renderUI({
    data<-values$globalTable
    text <- data[input$tableDT_rows_selected[1],"Datum"]
    textInput("inDatum","Datum", text)
  })
  output$inputTezba <- renderUI({
    data<-values$globalTable
    text <- data[input$tableDT_rows_selected[1],"Tezba"]
    textInput("inTezba","Těžba", text)
  })
  output$inputVtlaceni <- renderUI({
    data<-values$globalTable
    text <- data[input$tableDT_rows_selected[1],"Vtlaceni"]
    textInput("inVtlaceni","Vtláčení", text)
  })
  output$inputZfaktorBHP <- renderUI({
    data<-values$globalTable
    text <- data[input$tableDT_rows_selected[1],"ZfaktorBHP"]
    textInput("inZfaktorBHP","ZfaktorBHP", text)
  })
  output$inputGip <- renderUI({
    data<-values$globalTable
    text <- data[input$tableDT_rows_selected[1],"GIP"]
    textInput("inGip","Gas in place", text)
  })
  
  #NOVINKA Výchozí parametry
  output$inputVychoziGip <- renderUI({
    textInput("inVychoziGip","Výchozí GIP")
  })
  output$inputVychoziTeplota <- renderUI({
    textInput("inVychoziTeplota","Výchozí teplota")
  })
  output$inputVychoziObjem <- renderUI({
    textInput("inVychoziObjem","Vychozi objem")
  })
  output$inputVychoziHloubka <- renderUI({
    textInput("inVychoziHloubka","Vychozi hloubka")
  })
  
  output$datesALL <- renderUI({
    data<-values$globalTable
    fromDate <- data$Datum[1]
    toDate <- data$Datum[nrow(data)]
    dateRangeInput("dateRangeALL", 
                   "Sledované datum:",
                   start = fromDate,
                   end   = toDate,
                   format = "dd. mm. yyyy",
                   separator = "do")
  })
  
  #funkce filtrujici dny v klidu
  filterCalmDays <- function(data, calmDays){
      #Pokud vyfiltrovani CalmDays je zadouci
      if(calmDays>0){
        days = 0
        #vector o velikosti data (po inicializaci obsahuje hodnoty FALSE)
        RowsToDelete <- vector(,nrow(data))
        
        #forcyklus projede kazdy radek od data.length (nrow(data))
        for(index in 1:nrow(data)){
          if(as.numeric(data[index,2])==0 & as.numeric(data[index,3])==0){
            days=days+1
            if(days < calmDays){
              RowsToDelete[index] <- TRUE
            }
          }
          else{
            RowsToDelete[index] <- TRUE
            days = 0
          }
        }
        
        #Vymaze z data dny, ktere se nehodi (ve vectoru RowsToDelete hodnota TRUE na pozici urcene ke smazani) 
        data <- data[!RowsToDelete,]
      }
      return(data)
    }
  
  #Funkce pro filtraci datum
  filterDate<- function(data, fromDate, toDate){
    data<-data %>%
      filter(as.Date(Datum)>=fromDate) %>%
      filter(as.Date(Datum)<=toDate)
    return(data)
  }
  
  #funkce pro spocitani lin regrese z P a casu
  linRegression<- function(data){
    if(!is.null(data)){
      regression = lm(data$t~data$TlakBHP)
      coef = coefficients(regression)
    }
    
    return(coef[1])
  }
  
  #funkce pro spocitani matice a
  countCoefA<- function(data,days){
    a <- 1:dim(data)-days
    for(i in 0:(dim(data)-1-days)){
      a[i] = linRegression(data[i:(i+days),])
    }
    return(a)
  }
  
  #funkce pro spocitani D-hodnot
  countD <- function(data){
    regrese = lm((data$pz)~data$GIP, data=data)
    output$alpha <-renderUI(regrese[1])
    regreseHodnoty <- fitted(regrese)
    dValue <- 1:dim(data)[1]
    
    for(i in 1:length(dValue))
    {
      dValue[i] <- data[i, "pz"] - regreseHodnoty[i];
    }
    return(dValue)
  }
  #funkce pro rozdeleni na klidna obdobi
  getCalmIntervals <- function(data,calmDays){
    obdobi=0
    days = 0
    data$obdobi <- 0
    new=TRUE
    for(index in 1:nrow(data)){
      if(as.numeric(data[index,2])==0 & as.numeric(data[index,3])==0){
        days=days+1
        if(days >= calmDays){
          if(new==TRUE){
            obdobi=obdobi+1
            new=FALSE
          }
          data[index, "obdobi"]=obdobi
        }
      }
      else{
        new=TRUE
        days = 0
      }
    }
    
    return(data)
  }
  
  #funkce pro rozdeleni na vtlaceni/tezbu
  getCondition <- function(data){
    data$stav <- ""
    last <- ""
    for(index in 1:nrow(data)){
      if(as.numeric(data[index,2])!=0){
        data[index,"stav"] <- "vtlaceni"
        last <- "Vtlaceni"
      }
      else if(as.numeric(data[index,3])!=0){
        data[index,"stav"] <- "Tezba"
        last <- "Tezba"
      }
      else{
        data[index,"stav"] <- last
      }
    }
    return(data)
  }
  
  #funkce (k=x[1],c=x[2])
  func <- function(par, data){
    k<- par[1]
    c<- par[2]
    pLim <- par[3]
    sum((data$vecP-(k*exp(c*data$vecT)+pLim))^2)
  }
  #OPTIMALIZACE
  optimization <- function(){
    table <-values$globalTable
    table <- getCalmIntervals(table,calmDaysALL())
    table <- getCondition(table)
    optimized <- vector()
    seasons <- max(table$obdobi, na.rm = TRUE)
    count <- 0
    
    
    # coefsTable <- data.frame(season = numeric(), k = double(), c = double(), pLim = double(), from=as.Date(character()), to=as.Date(character()), condition=character(), stringsAsFactors = FALSE)
    coefsTable <- data.frame(from=as.Date(character()), to=as.Date(character()),season = numeric(), k = double(), c = double(), pLim = double(), condition=character(), stringsAsFactors = FALSE)
    
    listOfVectors <- list()
    for(season in 1:seasons){
    oneSeasonTable <- table %>%
      filter(obdobi==season)
    vecP <- vector()
    vecP <- oneSeasonTable$TlakBHP
    if(length(vecP)>5){ #pokud obdobi delsi nez 5 dny
      print(season)
      count <- count+1
      vecT <- c(1:length(vecP))
      par <- setParams(vecP)
      data = data.frame(vecP=vecP,vecT=vecT)
      #optimized <- optimx(par=par,func,data=data,method="Nelder-Mead")
      optimized <- optimx(par=par,func,data=data,method="L-BFGS-B") #L-BFGS-B IDEAL
      
      print(optimized)
      
      coefsTable[count, "season"] <- season
      coefsTable[count, "k"] <- optimized[1,1]
      coefsTable[count, "c"] <- optimized[1,2]
      coefsTable[count, "pLim"] <- optimized[1,3]
      coefsTable[count, "from"] <- as.Date(oneSeasonTable$Datum[1])
      coefsTable[count, "to"] <- as.Date(oneSeasonTable$Datum[length(oneSeasonTable$Datum)])
      coefsTable[count, "condition"] <- oneSeasonTable$stav[1]
      
      #data obdobi pro graf vyvoje
      y2 <- vector()
      y2 <- optimized[1,1]*exp(optimized[1,2]*vecT)+optimized[1,3]
      df = data.frame(vecP = vecP, vecPCounted=y2)
      listOfVectors <- c(listOfVectors, c(df))
      
    }#end for season
    else{
      print(season)
      print("nevyhovuje")
    }
    }# end if obdobi delsi nez 5 dny
    
    
    #print(coefsTable)
    values$coefsTable <- coefsTable
    values$optimDataToGraph <- listOfVectors
    return(coefsTable)
  }
  getSeasonOptim<- function(table,season){
    optimized <- vector()
    oneSeasonTable <- table %>%
      filter(obdobi==season)
    vecP <- vector()
    vecP <- oneSeasonTable$TlakBHP
    vecT <- c(1:length(vecP))
    par <- setParams(vecP)
    data = data.frame(vecP=vecP,vecT=vecT)
    #optimized <- optimx(par=par,func,data=data,method="Nelder-Mead")
    optimized <- optimx(par=par,func,data=data,method="L-BFGS-B") #L-BFGS-B IDEAL
    #data obdobi pro graf vyvoje
    y2 <- vector()
    y2 <- optimized[1,1]*exp(optimized[1,2]*vecT)+optimized[1,3]
    df = data.frame(vecP = vecP, vecPCounted=y2)
    return(df)
  }
  setParams <- function(vector){
    par <- vector()
    # POCATECNI PARAMETRY PUVODNI VYHODNOCOVANI TVARU USECKY
    # #pokud bude rostouci
    # if(vector[1]>vector[length(vector)]){
    #   if(vector[round(length(vector)/2)] < (vector[1]+vector[length(vector)])/2){
    #     par[2] = 0 #C bylo -1
    #     par[1] = 0 #K bylo 1
    #   }else{
    #     par[2] = 0 #C bylo 1
    #     par[1] = 0 #K bylo -1
    #   }
    # }else{
    #   if(vector[round(length(vector)/2)] < (vector[1]+vector[length(vector)])/2){
    #     par[2] = 0 #C bylo 1
    #     par[1] = 0 #K bylo 1
    #   }else{
    #     par[2] = 0 #C bylo -1 bylo -1
    #     par[1] = 0 #K bylo -1 bylo -1
    #   }
    #   
    # }
    par[1] = 0
    par[2] = 0
    par[3] = vector[length(vector)] #pLim 
    return(par)
  }
  
  plotVyvojObdobi <- reactive({
    if(!is.null(input$tableOptim_rows_selected[1])){
      index <- input$tableOptim_rows_selected[1]
    }
    else{
      index <- 1
    }
    season <- values$coefsTable[index,"season"]
    print(season)
    data <-values$globalTable
    data <- getCalmIntervals(data,calmDaysALL())
    data <- getCondition(data)
    df <- getSeasonOptim(data,season)
    #print(df)
    p <- ggplot(df, aes(x = c(1:length(vecP)))) + 
      geom_line(aes(y = vecP, colour = "Original P")) + 
      geom_line(aes(y = vecPCounted, colour = "Optimized function"))+xlab("Čas [dny]")+ylab("TlakBHP [MPa]")+labs(title="Vývoj tlaku BHP jednoho období")
  })
  
  plotKoefC <- reactive({
    optimized <- values$coefsTable
    p <- ggplot(optimized, aes(x = factor(c(1:length(optimized$c))), y = c))+xlab("Období")+ylab("Koeficient C")+labs(title="Vývoj koeficientu C")+geom_bar(stat="identity", width = 0.8, fill="orange")+ geom_text(aes(label = sprintf("%0.3f", round(c, digits = 3))), size = 3, vjust = 0.5, position =     "stack")
    
    })
  plotKoefK <- reactive({
    optimized <- optimization()
    hide(id = "loading-content", anim = TRUE, animType = "fade")
    p <- ggplot(optimized, aes(x = factor(c(1:length(optimized$k))), y = k))+xlab("Období")+ylab("Koeficient K")+labs(title="Vývoj koeficientu K")+geom_bar(stat="identity", width = 0.8, fill="yellow")+ geom_text(aes(label = sprintf("%0.3f", round(k, digits = 3))), size = 3, vjust = 0.5, position =     "stack")
  })
  
  plotSeasons <- reactive({
    data <- filterDate(values$globalTable,dateALL1(),dateALL2())
    data <- getCalmIntervals(data,calmDaysALL())
    data2 <- data %>%
      filter(obdobi > 0)
    p <- ggplot(data2, aes(y=as.double(TlakBHP), x=as.Date(Datum),group=factor(obdobi), colour=factor(obdobi)))+geom_line()+xlab("Datum [dny]")+ylab("TlakBHP [Mpa]")+labs(title="Intervaly klidných období")
  })
  
  #Plot 1 / coef Alpha
  plotAlpha <- reactive({
    if(dateALL2()!=0){
      data<-values$globalTable
      data <- filterDate(data, dateALL1(), dateALL2())
      data <- filterCalmDays(data, calmDaysALL())
      a <- countCoefA(data,forCoefA())
      v <- data.frame(vec=a)
      p <- ggplot(v, aes(x=as.Date(data$Datum), y=1/as.double(v$vec)))+xlab("Datum [dny]")+ylab("1/Alpha [sm³/Mpa]")+labs(title="Vývoj měrné skladovací kapacity")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
      #+theme(axis.text.x = element_text(angle=45))
    }
  })
  
  #Plot coef Alpha
  plotAlpha2 <- reactive({
    if(dateALL2()!=0){
      data<-values$globalTable
      data <- filterDate(data, dateALL1(), dateALL2())
      data <- filterCalmDays(data, calmDaysALL())
      a <- countCoefA(data,forCoefA())
      v <- data.frame(vec=a)
      p <- ggplot(v, aes(x=as.Date(data$Datum), y=as.double(v$vec)))+xlab("Čas [dny]")+ylab("Alpha [Mpa/sm³]")+labs(title="Vývoj koeficientu Alpha")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
      #+theme(axis.text.x = element_text(angle=45))
    }
  })
  
  firstDate <- reactive({
    data<-values$globalTable
    as.Date(data$Datum[0])
  })
  
  calmDaysALL <- reactive({
    as.numeric(input$calmDaysALL)
  })
   #pro vypocet alfa
   dateALL1 <- reactive({
     if(is.null(input$dateRangeALL[1])){
       0
     }
     else{
       as.Date(input$dateRangeALL[1])
     }
   })
   
   dateALL2 <- reactive({
     if(is.null(input$dateRangeALL[2])){
       0
     }
     else{
       as.Date(input$dateRangeALL[2])
     }
   })
   
   color <- reactive({
     input$color
   })
   
   color2 <- reactive({
     input$color2
   })
   
   forCoefA <- reactive({
     input$forCoefA
   })
   
   # checkboxGIP <- reactive({
   #   input$checkboxGIP
   # })
   # checkboxGIPe <- reactive({
   #   input$checkboxGIPe
   # })
   
   #ziskani vybrane row z tabulky
   output$rows = renderPrint({
     input$tableFilterDT_rows_selected
   })
   
   #Pro zoom plotu
   ranges <- reactiveValues(x = NULL, y = NULL)
   
   observeEvent(input$plot1_dblclick, {
     brush <- input$plot1_brush
     if (!is.null(brush)) {
       ranges$x <- c(brush$xmin, brush$xmax)
       ranges$y <- c(brush$ymin, brush$ymax)
       
     } else {
       ranges$x <- NULL
       ranges$y <- NULL
     }
   })
   
   inputFile <- reactive({
     input$fileIn
   })
   
   #Zpracovani souboru od uzivatele
   uploadDataFromFile<-reactive({ 
     #inputFile <- input$fileIn
     inputFile <- inputFile()
     datQ<-read_excel(inputFile$datapath, sheet="Q")
     datP<-read_excel(inputFile$datapath, sheet="P")
     dat<-merge(datQ,datP,by="Datum")
     
     #odstrani sloupce ktere se mi nehodi
     #dat <- dat[c(-5,-6,-8,-9,-10,-11,-13,-14)]
     dat <- dat[c(-5,-8,-9,-10,-13,-14)]
     
     #prejmenovani columns
     names(dat) <- c("Datum", "Vtlaceni", "Tezba","GIP","TlakTHP","TlakBHP","ZfaktorTHP","ZfaktorBHP")
     dat <- dat[-nrow(dat),]
     #print(dat)
     dat[, c(2,3,4,5,6,7,8)] <- sapply(dat[, c(2,3,4,5,6,7,8)], as.numeric)
     dat["pz"] <- dat["TlakBHP"]/dat["ZfaktorBHP"]
     for(i in 1:dim(dat)){
       dat[i,"t"] = i*24
     }
     values$globalTable <- dat
     print("vykonano uploadDataFromFile()")
     return(dat)
   })
   
  #Plot Projekce
   plotProjekce<-reactive({
     data<-values$globalTable
     if(dateALL2()!=0){
       data <- filterDate(data, dateALL1(),dateALL2())
       data <- filterCalmDays(data,calmDaysALL())
       selected <- input$tableFilterDT_rows_selected
       d <- countD(data)
       data["d"]<-d
       hide(id = "loading-content2", anim = TRUE, animType = "fade")
       if(input$checkboxProjekceDates == FALSE){
         p <- ggplot(data, aes(x=as.double(GIP)/1000000, y=as.double(d)))+xlab("Gas in place [x10⁶ sm³]")+ylab("Hodnota d")+geom_path(colour=color())+geom_point(data = data[selected,], aes(x=as.double(GIP)/1000000, y=as.double(d)),size=3, color="red")+
         geom_text(data = data[selected,],aes(label = Datum),vjust = 0, nudge_y = 0)+labs(title="Projekce (d)")
       }
       else{
         p <- ggplot(data, aes(x=as.double(GIP)/1000000, y=as.double(d)))+xlab("Gas in place [x10⁶ sm³]")+ylab("Hodnota d")+geom_path(colour=color())+geom_point(data = data[selected,], aes(x=as.double(GIP)/1000000, y=as.double(d)),size=3, color="red")+
           geom_text(data = data,aes(label = Datum),vjust = 0, nudge_y = 0)+labs(title="Projekce (d)")
         #geom_text(data = data[selected,],aes(label = Datum),vjust = 0, nudge_y = 0)+labs(title="Projekce (d)")
       }
       
     }
   })
   
   #hysterezni krivka
   plotHyst <- reactive({
     if(dateALL2()!=0){
       selected <- input$tableFilterDT_rows_selected
       data<-values$globalTable
       data <- filterDate(data,dateALL1(),dateALL2())
       data <- filterCalmDays(data,calmDaysALL())
       if(input$checkboxHystDates == FALSE){
         p <- ggplot(data, aes(x=as.double(GIP)/1000000, y=as.double(pz)))+xlab("Gas in place (x10⁶ sm³)")+ylab("P/z")+geom_path(colour=color())+
           coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
           geom_point(data = data[selected,], aes(x=as.double(GIP)/1000000, y=as.double(pz)),size=3, color="red")+
           geom_text(data = data[selected,],aes(label = Datum),vjust = 0, nudge_y = 0.5)+labs(title="Hysterezní křivka")#+geom_smooth(method="lm", se=F)
       }
       else{
         p <- ggplot(data, aes(x=as.double(GIP)/1000000, y=as.double(pz)))+xlab("Gas in place (x10⁶ sm³)")+ylab("P/z")+geom_path(colour=color())+
           coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
           geom_point(data = data[selected,], aes(x=as.double(GIP)/1000000, y=as.double(pz)),size=3, color="red")+
           geom_text(data = data,aes(label = Datum),vjust = 0, nudge_y = 0.5)+labs(title="Hysterezní křivka")#+geom_smooth(method="lm", se=F)
         
       }
       }
   })
   
   #plots GIP, P, p/z puvodni pro radiobox
   #renderPlots <- function(data, type){
  #   if(type==1){
  #     p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIP)/1000000))+xlab("Datum [dny]")+ylab("Gas in place [x10⁶ sm³]")+labs(title="Průběh GIP (množství plynu v zásobníku)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
  #   }
  #   else if(type==2){
  #     p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(TlakBHP)))+xlab("Datum [dny]")+ylab("TlakBHP [Mpa]")+labs(title="Průběh Tlaku BHP")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
  #   }
  #   
  #   else if(type==3){
  #     p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(pz)))+xlab("Datum [dny]")+ylab("P/z")+labs(title="Průběh P/z")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
  #   }
  #   else if(type==4){
  #     p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIP)/as.double(pz)))+xlab("Datum [dny]")+ylab("G/(P/z)")+labs(title="Průběh G(P/z)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
  #   }
  #   if(type==5){
  #     p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIPe)/1000000))+xlab("Datum [dny]")+ylab("Experim. GIPe [x10⁶ sm³]")+labs(title="Průběh GIPe (množství plynu v zásobníku)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
  #   }
  #   return(p)
  # }
   
   gipPlot <- reactive({
     #if(input$checkboxGIP){
       if(dateALL2()!=0){
         data<-values$globalTable
         data <- filterDate(data, dateALL1(), dateALL2())
         # if(checkboxGIP()){
         #   show("gipPlot")
         # }else{
         #   hide("gipPlot")
         # }
         hide(id="loading-content3",anim = TRUE, animType = "fade")
         #p <- renderPlots(data, input$radio)
         p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIP)/1000000))+xlab("Datum [dny]")+ylab("Gas in place [x10⁶ sm³]")+labs(title="Průběh GIP (množství plynu v zásobníku)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
         p
     }
     #}
   })
   
   tlakPlot <- reactive({
     #if(input$checkboxTlak){
     if(dateALL2()!=0){
       data<-values$globalTable
       data <- filterDate(data, dateALL1(), dateALL2())
       hide(id="loading-content3",anim = TRUE, animType = "fade")
       #p <- renderPlots(data, input$radio)
       p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(TlakBHP)))+xlab("Datum [dny]")+ylab("TlakBHP [Mpa]")+labs(title="Průběh Tlaku BHP")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
       p
     }
       #}
   })
   
   pzPlot <- reactive({
     #if(input$checkboxPz){
     if(dateALL2()!=0){
       data<-values$globalTable
       data <- filterDate(data, dateALL1(), dateALL2())
       hide(id="loading-content3",anim = TRUE, animType = "fade")
       #p <- renderPlots(data, input$radio)
       p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(pz)))+xlab("Datum [dny]")+ylab("P/z")+labs(title="Průběh P/z")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
       p
     }
     #}
   })
   
   gpzPlot <- reactive({
     #if(input$checkboxGpz){
     if(dateALL2()!=0){
       data<-values$globalTable
       data <- filterDate(data, dateALL1(), dateALL2())
       hide(id="loading-content3",anim = TRUE, animType = "fade")
       #p <- renderPlots(data, input$radio)
       p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIP)/as.double(pz)))+xlab("Datum [dny]")+ylab("G/(P/z)")+labs(title="Průběh G(P/z)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
       p
     }
     #}
   })
   
   gipePlot <- reactive({
     #if(input$checkboxGIPe){
     if(dateALL2()!=0){
       data<-values$globalTable
       data <- filterDate(data, dateALL1(), dateALL2())
       hide(id="loading-content3",anim = TRUE, animType = "fade")
       #p <- renderPlots(data, input$radio)
       p <- ggplot(data, aes(x=as.Date(Datum), y=as.double(GIPe)/1000000))+xlab("Datum [dny]")+ylab("Experim. GIPe [x10⁶ sm³]")+labs(title="Průběh GIPe (množství plynu v zásobníku)")+geom_line(colour=color2())+scale_x_date(labels = date_format("%d/%m/%y"))
       p
     }
     #}
   })
# RENDER PLOTU
   
   #Plot Alpha
   output$PlotCoefA <- renderPlot({
     print(plotAlpha())
   })
   #Plot 1/Alpha
   output$PlotCoefA2 <- renderPlot({
     print(plotAlpha2())
   })
   #Plot Projekce
   output$PlotProjekce <- renderPlot({
     print(plotProjekce())
   })
   
   #Plot hysterezni krivka
   output$mujPlot <- renderPlot({
     print(plotHyst())
   })
   
   #PlotS in time
   output$timePlot <- renderPlot({
     print(timePlot())
   })
   #Plot GIP in time
   output$gipPlot <- renderPlot({
     #if(checkboxGIP()==TRUE){
    #   show("gipPlot")
    # }
    # else{
    #   hide("gipPlot")
    # }
     print(gipPlot())
   })
   #Plot tlak in time
   output$tlakPlot <- renderPlot({
     # if(input$checkboxTlak==TRUE){
     #   show("tlakPlot")
     # }
     # else{
     #   hide("tlakPlot")
     # }
     print(tlakPlot())
   })
   #Plot pz in time
   output$pzPlot <- renderPlot({
     # if(input$checkboxPz==TRUE){
     #   show("pzPlot")
     # }
     # else{
     #   hide("pzPlot")
     # }
     print(pzPlot())
   })
   #Plot gpz in time
   output$gpzPlot <- renderPlot({
     # if(input$checkboxGpz==TRUE){
     #   show("gpzPlot")
     # }
     # else{
     #   hide("gpzPlot")
     # }
     print(gpzPlot())
   })
   #Plot gipe in time
   output$gipePlot <- renderPlot({
     # if(checkboxGIPe()==TRUE){
     #   show("gipePlot")
     # }
     # else{
     #   hide("gipePlot")
     # }
     print(gipePlot())
   })
   
   # Plot Obdobi
   output$seasons <- renderPlot({
     print(plotSeasons())
   })
   
   #Plot koeficient K
   output$koefK <- renderPlot({
     print(plotKoefK())
   })
   
   #Plot koeficient C
   output$koefC <- renderPlot({
     print(plotKoefC())
   })
   
   #Plot vyvoj jednoho obdobi
   output$vyvojObdobi <- renderPlot({
     print(plotVyvojObdobi())
   })
   
# RENDER TABULEK
   
   #Tabulka bez filtru DT
   output$tableDT <- DT::renderDataTable(
                                                
                                               datatable(
                                                  values$globalTable,rownames = FALSE, 
                                                  options = list( 
                                                    # initComplete = JS(
                                                    #                 "function(settings, json) {",
                                                    #                 "$(this.api().table().header()).css({'background-color': '#b81d1d', 'color': '#fff'});",
                                                    #                 "}"),
                                                                  pageLength=100,
                                                                  lengthMenu = c(10, 50, 100, 500),
                                                                  columnDefs = list(list(visible=FALSE, targets=c(6,8,9,10))),
                                                                  columnDefs = list(list(targets="GIP",render=formatMillions))
                                                                ),
                                                  selection="single",
                                                  colnames = c("Vtláčení [Sm³]" = "Vtlaceni","Těžba [Sm³]" = "Tezba","Z-faktor" = "ZfaktorBHP","GIP [Mil. Sm³]"="GIP","Tlak THP [MPa]"="TlakTHP","Tlak BHP [MPa]"="TlakBHP","P / z"="pz")
                                                        )%>% formatDate(1, "toLocaleDateString")
                                                        %>% formatRound(columns = 6, digits = 2)
                                                        %>% formatRound(columns = 5, digits = 2)
                                                        %>% formatRound(columns = "P / z", digits = 2),
                                               server = TRUE)
   
   #Tabulka filtr DT
   output$tableFilterDT <- DT::renderDataTable(
     datatable(
                if(dateALL2()!=0){filterCalmDays(filterDate(values$globalTable, dateALL1(), dateALL2()), calmDaysALL())},
                rownames = FALSE,
                options = list(
                  # initComplete = JS(
                  #                                 "function(settings, json) {",
                  #                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                  #                                 "}"),
                                pageLength=50,
                                lengthMenu = c(10, 25, 50, 100, 500),
                                columnDefs = list(list(visible=FALSE, targets=c(5,7,8,10)))
                              ),
                colnames = c("GIP [mil. Sm³]" = "GIP","P / z" = "pz","Tlak BHP [MPa]"="TlakBHP")
             )%>% formatDate(1, method = 'toLocaleDateString') 
              %>% formatRound(columns = "Tlak BHP [MPa]", digits = 2)
                #%>% formatRound(columns = "Z-faktor", digits = 2)
                %>% formatRound(columns = "P / z", digits = 2), 
     server = TRUE
    )
   
   #Tabulka koeficientu optimalizace sloupce FROM, TO, SEASON, K, C, Plim
   output$tableOptim <- DT::renderDataTable(
     datatable(values$coefsTable,
               rownames = FALSE,
       options = list(
         # initComplete = JS(
         # "function(settings, json) {",
         # "$(this.api().table().header()).css({'background-color': '#b81d1d', 'color': '#fff'});",
         # "}"),
         pageLength=50,
         columnDefs = list(list(visible=FALSE, targets=2))
       ),
       colnames = c("Od:" = "from","Do:" = "to","Koeficient k"="k","Koeficient c"="c","Limitni P"="pLim","Stav zásobníku"="condition"),
       selection="single"
     )%>% formatDate(c(1,2), method = 'toLocaleDateString')
     %>% formatRound(columns = c(4,5,6), digits = 4),
     server = TRUE
   )
   
# STAZENI PLOTU V PNG PDF
   
   #Download handler PNG hysteresni krivka
   output$downloadPng1 <- downloadHandler(
     filename = "plot_hysteresis.png",
     content= function(file){
       selected <- input$tableFilterDT_rows_selected
       #open device, create plot, close device
       png(file)
        print(plotHyst())
       dev.off()
     }
   )
   #Download handler PNG projekce
   output$downloadPng2 <- downloadHandler(
     filename = "plot_projection.png",
     content= function(file){
       selected <- input$tableFilterDT_rows_selected
       #open device, create plot, close device
       png(file)
       print(plotProjekce())
       dev.off()
     }
   )
   
   #Download handler PDF hysteresni krivka
   output$downloadPdf1 <- downloadHandler(
     filename = "plot_hysteresis.pdf",
     content= function(file){
       selected <- input$tableFilterDT_rows_selected
       #open device, create plot, close device
       pdf(file)
       print(plotHyst())
       dev.off()
     }
   )
   #Download handler PDF projekce
   output$downloadPdf2 <- downloadHandler(
     filename = "plot_projection.pdf",
     content= function(file){
       selected <- input$tableFilterDT_rows_selected
       #open device, create plot, close device
       pdf(file)
       print(plotProjekce())
       dev.off()
     }
   )
   
   #Download handler PDF plots in time
   #output$downloadPdf_gip <- downloadHandler(
  #   filename = "plot_intime.pdf",
  #   content= function(file){
  #     #open device, create plot, close device
  #     pdf(file)
  #     print(timePlot)
  #     dev.off()
  #   }
  # )
   #Download handler PNG plots in time
   #output$downloadPng_gip <- downloadHandler(
  #   filename = "plot_intime.png",
  #   content= function(file){
  #     #open device, create plot, close device
  #     png(file)
  #     print(timePlot())
  #     dev.off()
  #  }
  # )
   #gipPlot Download handler PDF
   output$gipPlot_downloadPdf <- downloadHandler(
     filename = "gipPlot.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(gipPlot())
       dev.off()
     }
   )
   #gipPlot Download handler PNG
   output$gipPlot_downloadPng <- downloadHandler(
     filename = "gipPlot.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(gipPlot())
       dev.off()
     }
   )
   #tlakPlot Download handler PDF
   output$tlakPlot_downloadPdf <- downloadHandler(
     filename = "tlakPlot.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(tlakPlot())
       dev.off()
     }
   )
   #tlakPlot Download handler PNG
   output$tlakPlot_downloadPng <- downloadHandler(
     filename = "tlakPlot.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(tlakPlot())
       dev.off()
     }
   )
   #pzPlot Download handler PDF
   output$pzPlot_downloadPdf <- downloadHandler(
     filename = "pzPlot.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(pzPlot)
       dev.off()
     }
   )
   #pzPlot Download handler PNG
   output$pzPlot_downloadPng <- downloadHandler(
     filename = "pzPlot.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(pzPlot())
       dev.off()
     }
   )
   #gpzPlot Download handler PDF
   output$gpzPlot_downloadPdf <- downloadHandler(
     filename = "gpzPlot.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(gpzPlot)
       dev.off()
     }
   )
   #gpzPlot Download handler PNG
   output$gpzPlot_downloadPng <- downloadHandler(
     filename = "gpzPlot.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(gpzPlot())
       dev.off()
     }
   )
   #gipePlot Download handler PDF
   output$gipePlot_downloadPdf <- downloadHandler(
     filename = "gipePlot.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(gipePlot)
       dev.off()
     }
   )
   #gipePlot Download handler PNG
   output$gipePlot_downloadPng <- downloadHandler(
     filename = "gipePlot.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(gipePlot())
       dev.off()
     }
   )
   
   #Download handler PDF alpha
   output$downloadPdf_alpha <- downloadHandler(
     filename = "plot_alpha.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(plotAlpha())
       dev.off()
     }
   )
   #Download handler PNG alpha
   output$downloadPng_alpha <- downloadHandler(
     filename = "plot_alpha.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(plotAlpha())
       dev.off()
     }
   )
   
   #Download handler PDF 1/alpha
   output$downloadPdf_alpha2 <- downloadHandler(
     filename = "plot_alpha2.pdf",
     content= function(file){
       #open device, create plot, close device
       pdf(file)
       print(plotAlpha2())
       dev.off()
     }
   )
   #Download handler PNG 1/alpha
   output$downloadPng_alpha2 <- downloadHandler(
     filename = "plot_alpha2.png",
     content= function(file){
       #open device, create plot, close device
       png(file)
       print(plotAlpha2())
       dev.off()
     }
   )
   }

# Run the application 
shinyApp(ui = ui, server = server)