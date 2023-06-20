library(shiny)
library(readxl)
library(data.table)
library(reshape)
library(RSQLite)
library(DT)
library(ggplot2)
library(plotly)
library(googleVis)
library(dplyr)

ui <- shinyUI(fluidPage(
  
  titlePanel("Aplikacja - Rafal Kaliszczuk 82443"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Pobieranie danych",
                         HTML(paste0("<hr>Pobierz dane ze strony GUS:<hr>")),
                         actionButton(inputId = "open_page_GUS", label = "Otworz polaczenie http"),
                         HTML(paste0("<hr>Pobierz dane ze strony EUROSTAT:<hr>")),
                         actionButton(inputId = "open_page_EUROSTAT", label = "Otworz polaczenie http"),
                         HTML(paste0("<hr>Teraz<hr>")),
                         actionButton(inputId = "GusBazaDanych", label = "Dodaj tabele z danymi z GUS do bazy danych"),
                         actionButton(inputId = "EurostatBazaDanych", label = "Dodaj tabele z danymi z EUROSTAT do bazy danych")
                ),
                
                tabPanel("SQL",
                         textInput("sqlQueryInput",label = "Zapytanie SQL", value = "Wprowadz zapytanie SQL"),
                         DT::dataTableOutput("tabelaSQL"),
                         downloadButton("fileOutPath", label = "Eksportuj dane"),
                         plotlyOutput("wykresSQL")
                         
                ),
                
                
                tabPanel("Mapka EUROSTAT", 
                         htmlOutput("EurostatMapa"),
                         htmlOutput("EurostatMapaWzgledna"),
                         textInput("startEUR", "Okres od:", as.Date("2020-01-1", '%Y-%m-%d')),
                         textInput("stopEUR", "Okres do:", as.Date("2021-12-1", '%Y-%m-%d')),
                         textInput("startTimeWzglednyMapa", "Okres wzgledny od:", as.Date("2015-01-1", '%Y-%m-%d')),
                         textInput("stopTimeWzglednyMapa", "Okres wzgledny do:", as.Date("2019-12-1", '%Y-%m-%d'))
                         
                ),
                tabPanel("Szereg czasowy Eurostat", 
                         column(6, plotlyOutput("szeregEurostat")),
                         column(6, plotlyOutput("szeregEurostatWzgledny")),
                         selectInput("geo", "Country:",
                                     list(`Countries` = list("Poland", "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany (until 1990 former territory of the FRG)",  "Estonia", "Ireland", "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania",  "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",  "Portugal", "Romania",  "Slovenia", "Slovakia", "Finland", "Sweden", "Iceland", "Liechtenstein", "Norway", "Switzerland",  "United Kingdom", "Montenegro", "Albania", "Serbia", "Andorra", "Armenia", "Georgia"))),
                         selectInput("sex", "Sex:",
                                     list(`Sex` = list("Total", "Males", "Females"))),
                         textInput("startDate", "Okres od:", as.Date("2020-01-1", '%Y-%m-%d')),
                         textInput("stopDate", "Okres do:", as.Date("2021-12-1", '%Y-%m-%d')),
                         textInput("startTimeWzgledny", "Okres wzgledny od:", as.Date("2015-01-1", '%Y-%m-%d')),
                         textInput("stopTimeWzgledny", "Okres wzgledny do:", as.Date("2019-12-1", '%Y-%m-%d'))
                ),
                
                tabPanel("Mapka GUS",
                         htmlOutput("GUSmapa"),
                         htmlOutput("GUSmapaWzgledna"),
                         textInput("startGUS", "Okres od:", as.Date("2020-01-1", '%Y-%m-%d')),
                         textInput("stopGUS", "Okres do:", as.Date("2021-12-1", '%Y-%m-%d')),
                         textInput("startTimeWzglednyMapaGUS", "Okres wzgledny od:", as.Date("2015-01-1", '%Y-%m-%d')),
                         textInput("stopTimeWzglednyMapaGUS", "Okres wzgledny do:", as.Date("2019-12-1", '%Y-%m-%d'))
                         
                ),
                tabPanel("Szereg czasowy GUS", 
                         column(6, plotlyOutput("szeregGUS")),
                         column(6, plotlyOutput("szeregGUSwzgledny")),
                         selectInput("woj", "Region:",
                                     list(`Region` = list("Polska", "Malopolskie", "Slaskie", "Wielkopolskie", "Zachodniopomorskie", "Lubuskie", "Dolnoslaskie",  "Opolskie", "Kujawsko-Pomorskie", "Warminsko-Mazurskie", "Pomorskie", "Lodzkie", "Swietokrzyskie", "Lubelskie", "Podkarpackie", "Podlaskie", "Mazowieckie"))),
                         selectInput("plec", "Plec:",
                                     list(`Plec` = list("Ogolem", "Mezczyzni", "Kobiety"))),
                         selectInput("grupaWiekowa", "Grupa wiekowa:",
                                     list(`Grupa wiekowa` = list("0 - Inf", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - Inf"))),
                         textInput("dataStart", "Okres od:", as.Date("2020-01-1", '%Y-%m-%d')),
                         textInput("dataStop", "Okres do:", as.Date("2021-12-1", '%Y-%m-%d')),
                         textInput("czasWzglednyStart", "Okres wzgledny od:", as.Date("2015-01-1", '%Y-%m-%d')),
                         textInput("czasWzglednyStop", "Okres wzgledny do:", as.Date("2019-12-1", '%Y-%m-%d'))
                ),
                tabPanel("Raport", actionButton(inputId = "generujRaport", label = "Generuj raport Markdown"))

    )
    
  )
)
)


server <- shinyServer(function(input, output) {
  
  sqlVar <- reactiveValues(
    sqlText = NULL 
  )
  
  observeEvent(input$sqlQueryInput,{
    sqlVar$sqlText <- input$sqlQueryInput
  })
  
  output$plainSQLText <- renderPrint({
    return(cat(paste(sqlVar$sqlText,"\n")))
  })
  
  observeEvent(input$open_page_GUS,{
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })  
  
  observeEvent(input$open_page_EUROSTAT,{
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
  })  
  
  
  observeEvent(input$GusBazaDanych,{
    
    dataDir    <- file.path(getwd(),"data")
    if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}
    
    res <- try({

      unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)

      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      try({
        lapply(dir(),function(f){
          file.rename(
            from=f,
            to = gsub(" ","_",gsub("\x88","l",f))
          )
        })
      })
      setwd(hd)
      
    })
    
    if(inherits(res,"try-error")){
      print("Proces zakonczyl sie niepowidzeniem")
    }else{
      print("Proces przebiegl pomyslnie")

      fileNames <- file.path("zgony_wg_tygodni",grep("^Zgony",dir("zgony_wg_tygodni"),value=T, useBytes = T))
      
      czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
        
        d <- as.data.frame(read_excel(f,sheet=sheet))
        colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
        d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
        
        tygodnie <- 1:(ncol(d)-3)
        tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
        colnames(d)[4:ncol(d)] <- tygodnie
        
        d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
        colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
        d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
        d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
        d$Liczba[is.na(d$Liczba)] <- 0
        d <- cbind("Plec"=plec,d)
        
        return(d)
        
      }
      
      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      
      try({
        mainRet <- do.call("rbind",lapply(dir(),function(f){
          print(f)
          
          ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
          mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
          kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
          
          dane <- rbind(ogolem,mezczyzni,kobiety)
          
          tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
          tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
            return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
          }))
          tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(as.character(tygodnie$Tydzien),"-"),function(x){x[2]})))
          rownames(tygodnie) <- NULL
          
          dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
          dane <- dane[,-which(colnames(dane)=="Tydzien")]
          
          dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
          dane$Liczba <- as.integer(dane$Liczba)
          
          dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
          dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
          
          return(dane)
          
        }))
        
      })
      
      dfGUS <- mainRet
      dfGUS$Od <- as.character(dfGUS$Od)
      dfGUS$Do <- as.character(dfGUS$Do)
      
      setwd(hd)
      
      con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
      dbWriteTable(con, "gus", dfGUS, overwrite = TRUE, row.names = FALSE)
      dbDisconnect(con)

    }
  })
  
  
  
  
  observeEvent(input$EurostatBazaDanych,{
    
    dataDir <- file.path(getwd(),"data")
    unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)
    dfEurostat <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
    
    dfEurostat$Value <- as.integer(gsub(",|:","",dfEurostat$Value))
    dfEurostat$TIME <- paste(gsub("W", "-", dfEurostat$TIME), 1, sep = "-")
    
    con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
    
    dbWriteTable(con, "eurostat", dfEurostat, overwrite = TRUE, row.names = FALSE)
    
    dbDisconnect(con)
    
  })
  
  
  zapytanieSQL <- function (sqlText = input$sqlQueryInput){
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    dfSQL <-  data.frame()
    try({
      dfSQL <- dbGetQuery(con, sqlText)
    })
    dbDisconnect(con)
    return(dfSQL)
    
  }
  
  output$tabelaSQL <- DT::renderDataTable({
    
    DT::datatable(zapytanieSQL(), options = list(lengthMenu = seq(10,100,10), pageLength = 10))
    
  })
  
  output$fileOutPath <- downloadHandler(
    
    filename = function() { 
      return(paste0(gsub("-","_",as.character(Sys.Date())),"_out.csv")) 
    },
    content = function(file) {
      write.table(
        zapytanieSQL(),
        file,
        sep=";",dec=".", row.names=F, col.names=T)
    }
  )
  
  
  output$wykresSQL <- renderPlotly({
    
    dfSQLwykres <- zapytanieSQL()
    
    if(nrow(dfSQLwykres)){
      
      dfSQLwykres$timestamp <- as.Date(dfSQLwykres$timestamp, "%Y-%m-%d")
      
      wykres <- ggplotly(ggplot(dfSQLwykres,aes(x=timestamp, y=value, col=variable)) + geom_line() + xlab("Timestamp") + ylab("Value") + labs(col="Variable"))
      
    }else{
      wykres <- ggplot(data.frame())
    }
    return(wykres)
  })
  
  dfGUSmapa <- reactive({
    
    conGUS <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    df <- data.frame(dbGetQuery(conGUS, "SELECT * FROM gus"))
    
    dbDisconnect(conGUS)
    
    df$Od <- as.Date(df$Od, "%Y-%m-%d")
    #df$Do <- as.Date(df$Do, "%Y-%m-%d")
    
    df1 <- na.omit(df) 
    
    dataStartGUS <- as.Date(input$startGUS, "%Y-%m-%d")
    dataStopGUS <- as.Date(input$stopGUS, "%Y-%m-%d")
    
    df2 <- df1 %>% filter((Od > dataStartGUS & Od < dataStopGUS))
    
    df3 <- df2 %>% select(Region, Region_id,  Liczba) %>% filter(Region_id %in% c("PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62")) %>% group_by(Region) %>% summarise(Total = sum(Liczba))
    
    df3$Region <- tolower(df3$Region)
    
    df4 <- df3 %>% mutate(Region = replace(df3$Region, df3$Region == "makroregion województwo mazowieckie", "mazowieckie" ))
    
    df5 <- df4 %>% mutate(Region = replace(df4$Region, df4$Region == "lódzkie", "lodzkie" ))
    
    return(df5)
    
  })
  
  dfGUSmapaWzgledna <- reactive({
    
    conGUS <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    df <- data.frame(dbGetQuery(conGUS, "SELECT * FROM gus"))
    
    dbDisconnect(conGUS)
    
    df$Od <- as.Date(df$Od, "%Y-%m-%d")
    #df$Do <- as.Date(df$Do, "%Y-%m-%d")
    
    df1 <- na.omit(df) 
    
    dataStartGUS <- as.Date(input$startGUS, "%Y-%m-%d")
    dataStopGUS <- as.Date(input$stopGUS, "%Y-%m-%d")
    startTimeWzglednyMapaGUS <- as.Date(input$startTimeWzglednyMapaGUS, '%Y-%m-%d') 
    stopTimeWzglednyMapaGUS <- as.Date(input$stopTimeWzglednyMapaGUS, '%Y-%m-%d')
    avgWzglednaMapaGUS <- df1 %>% filter((Od > startTimeWzglednyMapaGUS & Od < stopTimeWzglednyMapaGUS)) 
    avgWzglednaMapaGUS <- mean(avgWzglednaMapaGUS$Liczba, na.rm = TRUE)
    
    df2 <- df1 %>% filter((Od > dataStartGUS & Od < dataStopGUS))
    
    df3 <- df2 %>% select(Region, Region_id,  Liczba) %>% filter(Region_id %in% c("PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62")) %>% group_by(Region) %>% summarise(Total = sum(Liczba))
    
    df3$Region <- tolower(df3$Region)
    
    df4 <- df3 %>% mutate(Region = replace(df3$Region, df3$Region == "makroregion województwo mazowieckie", "mazowieckie" ))
    
    df5 <- df4 %>% mutate(Region = replace(df4$Region, df4$Region == "lódzkie", "lodzkie" ))
    
    df5$srednia <- avgWzglednaMapaGUS
    df5$wartoscWzgledna <- (df5$Total)/(df5$srednia)
    return(df5)
    
  })
  
  output$GUSmapa <- renderGvis({
    
    gvisGeoChart(dfGUSmapa(), "Region", "Total",
                 options=list(region="PL",
                              displayMode="regions",
                              resolution="provinces",
                              width=1000, height=1000))
    
  })
  
  output$GUSmapaWzgledna <- renderGvis({
    
    gvisGeoChart(dfGUSmapaWzgledna(), "Region", "wartoscWzgledna",
                 options=list(region="PL",
                              displayMode="regions",
                              resolution="provinces",
                              width=1000, height=1000))
    
  })
  
  dfEurostatMapa <- reactive({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    df <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    df$TIME <- as.Date(df$TIME, "%Y-%m-%d")
    
    df1 <- na.omit(df)
    
    startEUR <- as.Date(input$startEUR, "%Y-%m-%d")
    stopEUR <- as.Date(input$stopEUR, "%Y-%m-%d")
    
    df2 <- df1 %>% filter((TIME > startEUR & TIME < stopEUR))
    
    df3 <- df2 %>% select(GEO, Value)  %>% group_by(GEO) %>% summarise(Total = sum(Value))
    
    df4 <- df3 %>% mutate(GEO = replace(df3$GEO, df3$GEO == "Germany (until 1990 former territory of the FRG)", "Germany" ))
    
    df5 <- df4 %>% mutate(GEO = replace(df4$GEO, df4$GEO == "Czechia", "Czech Republic" ))
    
    return(df5)
    
  })
  
  dfEurostatMapaWzgledna <- reactive({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    df <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    df$TIME <- as.Date(df$TIME, "%Y-%m-%d")
    
    df1 <- na.omit(df)
    
    startEUR <- as.Date(input$startEUR, "%Y-%m-%d")
    stopEUR <- as.Date(input$stopEUR, "%Y-%m-%d")
    startTimeWzglednyMapa <- as.Date(input$startTimeWzglednyMapa, '%Y-%m-%d') 
    stopTimeWzglednyMapa <- as.Date(input$stopTimeWzglednyMapa, '%Y-%m-%d')
    avgWzglednaMapa <- df1 %>% filter((TIME > startTimeWzglednyMapa & TIME < stopTimeWzglednyMapa)) 
    avgWzglednaMapa <- mean(avgWzglednaMapa$Value, na.rm = TRUE)
    
    df2 <- df1 %>% filter((TIME > startEUR & TIME < stopEUR))
    
    df3 <- df2 %>% select(GEO, Value)  %>% group_by(GEO) %>% summarise(Total = sum(Value))
    
    df4 <- df3 %>% mutate(GEO = replace(df3$GEO, df3$GEO == "Germany (until 1990 former territory of the FRG)", "Germany" ))
    
    df5 <- df4 %>% mutate(GEO = replace(df4$GEO, df4$GEO == "Czechia", "Czech Republic" ))
    
    df5$srednia <- avgWzglednaMapa
    df5$wartoscWzgledna <- (df5$Total)/(df5$srednia)
    
    return(df5)
    
  })
  
  output$EurostatMapa <- renderGvis({
    
    gvisGeoChart(dfEurostatMapa(), "GEO", "Total",
                 options=list(region="150",
                              width=1000, height=1000))
    
  })
  
  
  output$EurostatMapaWzgledna <- renderGvis({
    
    gvisGeoChart(dfEurostatMapaWzgledna(), "GEO", "wartoscWzgledna",
                 options=list(region="150",
                              width=1000, height=1000))
    
  })
  
  
  output$szeregEurostat <- renderPlotly({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregEurostat <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    dfSzeregEurostat$TIME <- as.Date(dfSzeregEurostat$TIME, "%Y-%m-%d")
    
    geo <- input$geo
    sex <- input$sex
    startDate <- as.Date(input$startDate, '%Y-%m-%d') 
    stopDate <- as.Date(input$stopDate, '%Y-%m-%d')
    
    wykresEurostat <- dfSzeregEurostat %>% filter(GEO == geo, SEX == sex, (TIME > startDate & TIME < stopDate))
    
    wykres <- ggplotly(ggplot(data = wykresEurostat, aes(x= TIME, y= Value)) + geom_line())
    
    return(wykres)
    
  })
  
  
  output$szeregEurostatWzgledny <- renderPlotly({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregEurostatWzgledny <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    dfSzeregEurostatWzgledny$TIME <- as.Date(dfSzeregEurostatWzgledny$TIME, "%Y-%m-%d")
    
    geoWzgledne <- input$geo
    sexWzgledna <- input$sex
    startDateWzgledna <- as.Date(input$startDate, '%Y-%m-%d') 
    stopDateWzgledna <- as.Date(input$stopDate, '%Y-%m-%d')
    startTimeWzgledny <- as.Date(input$startTimeWzgledny, '%Y-%m-%d') 
    stopTimeWzgledny <- as.Date(input$stopTimeWzgledny, '%Y-%m-%d')
    avgWzgledna <- dfSzeregEurostatWzgledny %>% filter((TIME > startTimeWzgledny & TIME < stopTimeWzgledny)) 
    avgWzgledna <- mean(avgWzgledna$Value, na.rm = TRUE)
    
    wykresEurostatWzgledny <- dfSzeregEurostatWzgledny %>% filter(GEO == geoWzgledne, SEX == sexWzgledna, (TIME > startDateWzgledna & TIME < stopDateWzgledna))
    wykresEurostatWzgledny$srednia <- avgWzgledna
    
    wykresWzgledny <- ggplotly(ggplot(data = wykresEurostatWzgledny, aes(x= TIME, y= Value/avgWzgledna)) + geom_line() + ylab("Wartosc wzgledna"))
    
    return(wykresWzgledny)
    
  })
  
  
  output$szeregGUS <- renderPlotly({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregGUS <- data.frame(dbGetQuery(con, "SELECT * FROM gus"))
    
    dbDisconnect(con)
    
    dfSzeregGUS$Od <- as.Date(dfSzeregGUS$Od, "%Y-%m-%d")
    
    dfSzeregGUS <- as.data.frame(dfSzeregGUS) %>% filter(Region_id %in% c("PL", "PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62"))
    dfSzeregGUS1 <- dfSzeregGUS %>% mutate(Region = replace(dfSzeregGUS$Region, dfSzeregGUS$Region == "Makroregion WojewÃ³dztwo Mazowieckie", "Mazowieckie" ))
    dfSzeregGUS2 <- dfSzeregGUS1 %>% mutate(Region = replace(dfSzeregGUS1$Region, dfSzeregGUS1$Region == "LÃ³dzkie", "Lodzkie" ))
    
    woj <-  input$woj
    plec <- input$plec
    grupaWiekowa <- input$grupaWiekowa
    dataStart <- as.Date(input$dataStart, '%Y-%m-%d') 
    dataStop <- as.Date(input$dataStop, '%Y-%m-%d') 
    
    
    wykresGUS <- dfSzeregGUS2 %>% filter(Region == woj, Plec == plec, Grupa_wiekowa == grupaWiekowa, (Od > dataStart & Od < dataStop))
    
    wykres <- ggplotly(ggplot(data = wykresGUS, aes(x = Od, y = Liczba)) + geom_line() + xlab("Data"))
    
    return(wykres)
    
  })
  
  
  
  
  output$szeregGUSwzgledny <- renderPlotly({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregGUSwzgledny <- data.frame(dbGetQuery(con, "SELECT * FROM gus"))
    
    dbDisconnect(con)
    
    dfSzeregGUSwzgledny$Od <- as.Date(dfSzeregGUSwzgledny$Od, "%Y-%m-%d")
    
    dfSzeregGUSwzgledny <- as.data.frame(dfSzeregGUSwzgledny) %>% filter(Region_id %in% c("PL", "PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62"))
    dfSzeregGUSwzgledny1 <- dfSzeregGUSwzgledny %>% mutate(Region = replace(dfSzeregGUSwzgledny$Region, dfSzeregGUSwzgledny$Region == "Makroregion WojewÃ³dztwo Mazowieckie", "Mazowieckie" ))
    dfSzeregGUSwzgledny2 <- dfSzeregGUSwzgledny1 %>% mutate(Region = replace(dfSzeregGUSwzgledny1$Region, dfSzeregGUSwzgledny1$Region == "LÃ³dzkie", "Lodzkie" ))
    
    wojWzgledne <-  input$woj 
    plecWzgledna <- input$plec 
    grupaWiekowaWzgledna <- input$grupaWiekowa 
    dataStartWzgledna <- as.Date(input$dataStart, '%Y-%m-%d') 
    dataStopWzgledna <- as.Date(input$dataStop, '%Y-%m-%d') 
    
    czasWzglednyStart <- as.Date(input$czasWzglednyStart, '%Y-%m-%d')
    czasWzglednyStop <- as.Date(input$czasWzglednyStop, '%Y-%m-%d')
    sredniaWzgledna <- dfSzeregGUSwzgledny2 %>% filter((Od > czasWzglednyStart & Od < czasWzglednyStop)) 
    sredniaWzgledna <- mean(sredniaWzgledna$Liczba, na.rm = TRUE)
    
    wykresGUSwzgledny <- dfSzeregGUSwzgledny2 %>% filter(Region == wojWzgledne, Plec == plecWzgledna, Grupa_wiekowa == grupaWiekowaWzgledna, (Od > dataStartWzgledna & Od < dataStopWzgledna))
    wykresGUSwzgledny$srednia <- sredniaWzgledna
    
    wykres <- ggplotly(ggplot(data = wykresGUSwzgledny, aes(x = Od, y = Liczba/sredniaWzgledna)) + geom_line() + xlab("Czas") + ylab("Wartosc wzgledna"))
    
    return(wykres)
    
  })
  
  szeregEurostatWzglednyRaport <- reactive({
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregEurostatWzgledny <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    dfSzeregEurostatWzgledny$TIME <- as.Date(dfSzeregEurostatWzgledny$TIME, "%Y-%m-%d")
    
    geoWzgledne <- input$geo
    sexWzgledna <- input$sex
    startDateWzgledna <- as.Date(input$startDate, '%Y-%m-%d') 
    stopDateWzgledna <- as.Date(input$stopDate, '%Y-%m-%d')
    startTimeWzgledny <- as.Date(input$startTimeWzgledny, '%Y-%m-%d') 
    stopTimeWzgledny <- as.Date(input$stopTimeWzgledny, '%Y-%m-%d')
    avgWzgledna <- dfSzeregEurostatWzgledny %>% filter((TIME > startTimeWzgledny & TIME < stopTimeWzgledny)) 
    avgWzgledna <- mean(avgWzgledna$Value, na.rm = TRUE)
    
    wykresEurostatWzgledny <- dfSzeregEurostatWzgledny %>% filter(GEO == geoWzgledne, SEX == sexWzgledna, (TIME > startDateWzgledna & TIME < stopDateWzgledna))
    wykresEurostatWzgledny$srednia <- avgWzgledna
    
    return(wykresEurostatWzgledny)
  })
  
  szeregGUSRaport <- reactive({
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregGUS <- data.frame(dbGetQuery(con, "SELECT * FROM gus"))
    
    dbDisconnect(con)
    
    dfSzeregGUS$Od <- as.Date(dfSzeregGUS$Od, "%Y-%m-%d")
    
    dfSzeregGUS <- as.data.frame(dfSzeregGUS) %>% filter(Region_id %in% c("PL", "PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62"))
    dfSzeregGUS1 <- dfSzeregGUS %>% mutate(Region = replace(dfSzeregGUS$Region, dfSzeregGUS$Region == "Makroregion WojewÃ³dztwo Mazowieckie", "Mazowieckie" ))
    dfSzeregGUS2 <- dfSzeregGUS1 %>% mutate(Region = replace(dfSzeregGUS1$Region, dfSzeregGUS1$Region == "LÃ³dzkie", "Lodzkie" ))
    
    woj <-  input$woj 
    plec <- input$plec  
    grupaWiekowa <- input$grupaWiekowa 
    dataStart <- as.Date(input$dataStart, '%Y-%m-%d') 
    dataStop <- as.Date(input$dataStop, '%Y-%m-%d') 
    
    
    wykresGUS <- dfSzeregGUS2 %>% filter(Region == woj, Plec == plec, Grupa_wiekowa == grupaWiekowa, (Od > dataStart & Od < dataStop))
    
    return(wykresGUS)
  })
  
  szeregGUSWzglednyRaport <- reactive({
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregGUSwzgledny <- data.frame(dbGetQuery(con, "SELECT * FROM gus"))
    
    dbDisconnect(con)
    
    dfSzeregGUSwzgledny$Od <- as.Date(dfSzeregGUSwzgledny$Od, "%Y-%m-%d")
    
    dfSzeregGUSwzgledny <- as.data.frame(dfSzeregGUSwzgledny) %>% filter(Region_id %in% c("PL", "PL9","PL42","PL41","PL72","PL22","PL52","PL21","PL71","PL43","PL81","PL61","PL51","PL82","PL84","PL63","PL62"))
    dfSzeregGUSwzgledny1 <- dfSzeregGUSwzgledny %>% mutate(Region = replace(dfSzeregGUSwzgledny$Region, dfSzeregGUSwzgledny$Region == "Makroregion WojewÃ³dztwo Mazowieckie", "Mazowieckie" ))
    dfSzeregGUSwzgledny2 <- dfSzeregGUSwzgledny1 %>% mutate(Region = replace(dfSzeregGUSwzgledny1$Region, dfSzeregGUSwzgledny1$Region == "LÃ³dzkie", "Lodzkie" ))
    
    wojWzgledne <-  input$woj 
    plecWzgledna <- input$plec 
    grupaWiekowaWzgledna <- input$grupaWiekowa 
    dataStartWzgledna <- as.Date(input$dataStart, '%Y-%m-%d') 
    dataStopWzgledna <- as.Date(input$dataStop, '%Y-%m-%d') 
    
    czasWzglednyStart <- as.Date(input$czasWzglednyStart, '%Y-%m-%d')
    czasWzglednyStop <- as.Date(input$czasWzglednyStop, '%Y-%m-%d')
    sredniaWzgledna <- dfSzeregGUSwzgledny2 %>% filter((Od > czasWzglednyStart & Od < czasWzglednyStop)) 
    sredniaWzgledna <- mean(sredniaWzgledna$Liczba, na.rm = TRUE)
    
    wykresGUSwzgledny <- dfSzeregGUSwzgledny2 %>% filter(Region == wojWzgledne, Plec == plecWzgledna, Grupa_wiekowa == grupaWiekowaWzgledna, (Od > dataStartWzgledna & Od < dataStopWzgledna))
    wykresGUSwzgledny$srednia <- sredniaWzgledna
    
    return(wykresGUSwzgledny)
  })
  
  szeregEurostatRaport <- reactive({
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    
    dfSzeregEurostat <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    dfSzeregEurostat$TIME <- as.Date(dfSzeregEurostat$TIME, "%Y-%m-%d")
    
    geo <- input$geo
    sex <- input$sex
    startDate <- as.Date(input$startDate, '%Y-%m-%d') 
    stopDate <- as.Date(input$stopDate, '%Y-%m-%d')
    
    wykresEurostat <- dfSzeregEurostat %>% filter(GEO == geo, SEX == sex, (TIME > startDate & TIME < stopDate))
    
    return(wykresEurostat)
  })
  
  observeEvent(input$generujRaport,{
    
    par <- list(dfGUSmapa = dfGUSmapa(), dfEurostatMapa = dfEurostatMapa(), szeregEurostatRaport = szeregEurostatRaport(),
                dfEurostatMapaWzgledna = dfEurostatMapaWzgledna(),zapytanieSQL = zapytanieSQL(), szeregEurostatWzglednyRaport = szeregEurostatWzglednyRaport(), 
                dfGUSmapaWzgledna = dfGUSmapaWzgledna() ,szeregGUSRaport = szeregGUSRaport(), szeregGUSWzglednyRaport = szeregGUSWzglednyRaport())
    
    if(file.exists("Raport.html")) unlink("Raport.html")
    if(file.exists("par")) unlink("par")
    if(dir.exists("cache")) unlink("cache",recursive = T, force = T)
    
    save(par,file="par")
    
    library(knitr)
    library(markdown)
    library(rmarkdown)
    
    knit(input='Raport.Rmd', output="tmp.md",envir=new.env())
    markdownToHTML(file="tmp.md", output="Report.html")
    
    unlink("tmp.md")
    unlink("par")

  })
  
}) 

shinyApp(ui = ui, server = server)
