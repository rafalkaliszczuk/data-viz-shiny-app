### Package names
packages <- c(
  "shiny", 
  "data.table", 
  "reshape", 
  "RSQLite", 
  "DT", 
  "ggplot2", 
  "plotly", 
  "googleVis", 
  "dplyr", 
  "tidyr", 
  "eurostat"
)

### Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

### Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# library(shiny)
# library(data.table)
# library(reshape)
# library(RSQLite)
# library(DT)
# library(ggplot2)
# library(plotly)
# library(googleVis)
# library(dplyr)
# library(tidyr)
# library(eurostat)


ui <- shinyUI(fluidPage(
  
  titlePanel("App visualizing COVID deaths data from Eurostat"),
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Start page",
                         HTML("<hr>
                               <b>Welcome to my Shiny App project!</b>
                               <p>This Shiny application is designed to visualize COVID-19 death-related data from Eurostat. It allows users to execute SQL queries, create interactive plots, and display data on a map.</p>
                               </hr>
                               <br><p>In the start page, you can access the raw dataset from Eurostat used for analysis and add relevant table to the RSQLLite database.</p>"),
                              
                         
                         HTML("<hr><b>You can download the raw dataset in a .zip format from Eurostat website here</b></hr>"),
                    
                         actionButton(inputId = "open_page_eurostat", 
                                      label = "Open http:// connection and download the newest Deaths by week and sex (demo_r_mwk_ts)"),
                         
                         HTML(paste0("<hr><b>Next, add table with Eurostat data to the RSQLLite database:</b><hr>")),
                         actionButton(inputId = "database_eurostat", 
                                      label = "Add Eurostat data")
                ),
                tabPanel("SQL",
                         fluidRow(
                           column(12,
                                  HTML("<br><p>Here, after creating the database, you can explore, manipulate and visualize the Eurostat dataset using SQL queries.</p>
                                       <p>Enter your custom SQL queries to extract specific information from the dataset and view the results in an interactive table.</p>
                                       <p>In order for a visual to be displayed, you have to specify at least the following columns: 
                                       <ul>
                                           <li><b>[sex]</b></li>
                                           <li><b>[date_int]</b></li>
                                           <li><b>[geo]</b></li>
                                           <li><b>[values]</b></li>
                                       </ul>
                                       <p>e.g. SELECT [sex], [date_int], [geo], [values] FROM eurostat WHERE [geo] = 'UK'</p>
                                       <p>After finishing your analysis you can download your data as a .csv file</p></br>"),
                                  textInput("sql_query_input",
                                            label = "Enter your SQL query", 
                                            value = "SELECT * FROM eurostat WHERE geo = 'PL'",
                                            width = '100%')
                           )
                         ),
                         fluidRow(
                           column(12,
                                  DT::dataTableOutput("SQL_table")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  HTML("<hr><b><p>Download the queried data</p></b></hr>"),
                                  column(12, downloadButton("file_output_path", 
                                                            label = "Export queried data into .csv file"))
                           )
                         ),
                         fluidRow(
                           column(12,
                                  HTML("<hr><b>Plot of the queried data</b></hr>"),
                                  plotlyOutput("SQL_plot")
                           )
                         )
                         
                ),
                tabPanel("Map", 
                         
                         HTML("<br><p>In the Map section, visualize data on an interactive map of Europe.</p> 
                              <p>Specify the time frame to observe data trends and patterns across different regions.</p></br>"),
                         
                         fluidRow(
                           column(6, textInput("start_eurostat", "Okres od:", "2021-01-01")),
                           column(6, textInput("stop_eurostat", "Okres do:", "2021-12-01"))
                         ),
                         
                         fluidRow(
                           column(12, htmlOutput("eurostat_map"))
                         )
                ),
                tabPanel("Time series", 
                         
                         HTML("<br><p>Go ahead and explore the dataset through time in the Time series section.</p></br> 
                              <p>Select specific countries and genders to generate interactive time series plots. Define a time frame to analyze the temporal trends of COVID-19-related data.</p>"),
                         
                         fluidRow(
                           column(6, 
                                  selectInput("country", 
                                              "Country:",
                                              list(`Countries` = list(
                                                "Netherlands", "Poland", "Sweden", "Switzerland", "Germany", "Malta",
                                                "Slovenia", "Austria", "Belgium", "Cyprus", "Finland", "France",
                                                "Hungary", "Norway", "Bulgaria", "Denmark", "Estonia", "Greece",
                                                "Spain", "Croatia", "Ireland", "Iceland", "Italy", "Liechtenstein",
                                                "Lithuania", "Luxembourg", "Latvia", "Portugal", "Armenia", "Czechia",
                                                "Romania", "Slovakia", "Serbia", "Montenegro", "Albania"
                                                )
                                              )
                                  )
                           ),
                           column(6,
                                  selectInput("sex", 
                                              "Sex:",
                                              list(`Sex` = list("Total", "Male", "Female"))
                                  )
                           )
                         ),
                         fluidRow(
                           column(6,
                                  textInput("start_date_eurostat_ts", 
                                            "Okres od:", 
                                            "2021-01-01"
                                  )
                           ),
                           column(6,
                                  textInput("stop_date_eurostat_ts", 
                                            "Okres do:", 
                                            "2021-12-01"
                                  )
                           )
                         ),
                         plotlyOutput("eurostat_ts")
                ),
                tabPanel("Report", 
                         
                         HTML("<br><p>Generate the report with your visuals in HTML format.</br></p>
                               <p>Utilize the app's functionalities to perform analyses and create visualizations, then export your findings into HTML report for sharing and documentation purposes</p><br>"),
                         actionButton(inputId = "generate_report", 
                                      label = "Generate Markdown report")
                )
                
    )
    
  )
)
)


server <- shinyServer(function(input, output) {
  
  SQL_var <- reactiveValues(
    SQL_text = NULL 
  )
  
  observeEvent(input$sql_query_input, {
    SQL_var$SQL_text <- input$sql_query_input
    }
  )
  
  output$plain_sql_text <- renderPrint({
    return(cat(paste(SQL_var$SQL_text, "\n")))
    }
  )
  
  observeEvent(input$open_page_eurostat, {
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
    }
  )  

  
  ### Creating Eurostat database table 
  observeEvent(input$database_eurostat,{
    
    ### Preprocessing Eurostat data
    df_eurostat <- get_eurostat("demo_r_mwk_ts")
    
    df_eurostat <- df_eurostat %>%
      left_join(
        rbind(eurostat::eu_countries, eurostat::efta_countries, eurostat::eu_candidate_countries), 
        by = c("geo" = "code")
      ) %>%  
      filter(geo != "EU27_2020") %>% 
      mutate(
        year = as.numeric(substr(TIME_PERIOD, 1, 4))
        #week = substr(time, 5, 7)
      ) %>%
      filter(year >= 2019)
   
    df_eurostat <- df_eurostat %>% 
      select(-label, -unit, - year) %>% 
      mutate(
        sex = case_when(
          sex == 'F' ~ 'Female',
          sex == 'M' ~ 'Male',
          sex == 'T' ~ 'Total'
        )
      )
     
    ### Handling Armenia as it does not exist in the Eurostat country tables 
    df_eurostat$name <- ifelse(df_eurostat$geo == "AM", "Armenia", df_eurostat$name)
    
    ### Converting time to the Date format 
    #df_eurostat$date <- paste(df_eurostat$year, df_eurostat$week, "1", sep = "-")
    #df_eurostat$date <- ISOweek::ISOweek2date(df_eurostat$date)
    
    df_eurostat$date_int <- as.numeric(paste0(substr(df_eurostat$TIME_PERIOD, 1, 4), substr(df_eurostat$TIME_PERIOD, 6, 7), substr(df_eurostat$TIME_PERIOD, 9, 10)))
    df_eurostat$TIME_PERIOD <- as.character(df_eurostat$TIME_PERIOD)
    
    ### Writing preprocessed data to the RSQLLite database
    con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
    dbWriteTable(con, "eurostat", df_eurostat, overwrite = TRUE, row.names = FALSE)
    dbDisconnect(con)
    
  })
  
  ### SQL Query handling
  SQL_query <- function(SQL_text = input$sql_query_input){
    
    con <- dbConnect(dbDriver("SQLite"),dbname="database.db")
    df_SQL <-  data.frame()
    
    try({
      df_SQL <- dbGetQuery(con, SQL_text)
    })
    
    dbDisconnect(con)
    return(df_SQL)
    
  }
  
  ### Results of the SQL query
  output$SQL_table <- DT::renderDataTable({
    
    DT::datatable(SQL_query(), 
                  options = list(
                              lengthMenu = seq(10, 100, 10), 
                              pageLength = 10)
                  )
    
  })
  
  ### Handling the button exporting queried data
  output$file_output_path <- downloadHandler(
    filename = function() {
      return(paste0(gsub("-", "_", as.character(Sys.Date())), "_out.csv"))
    },
    
    content = function(file) {
      write.table(
        SQL_query(),
        file,
        sep = ";",
        dec = ".",
        row.names = F,
        col.names = T
      )
    }
  )
  
  ### Plotting data returned by SQL query
  output$SQL_plot <- renderPlotly({
    df_SQL_plot <- SQL_query()
    
    if (nrow(df_SQL_plot)) {
      df_SQL_plot$date <- as.Date(as.character(df_SQL_plot$date_int), "%Y%m%d")
      
      plot <- ggplotly(
                  ggplot(df_SQL_plot, aes(x = date, y = values, col = sex)) +
                    geom_line() +
                    xlab("Date") +
                    ylab("Value") +
                    labs(col = "Sex")
        )
      
      ### Plot of empty data frame
    } else{
      plot <- ggplot(data.frame())
    }
    
    return(plot)
  })
  
  ### Preparing data for the Eurostat map
  df_eurostat_map <- reactive({
    
    con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
    
    df <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    df$date <- as.Date(as.character(df$date_int), "%Y%m%d")
    
    df <- na.omit(df)
  
    df$name <- ifelse(df$name == "Czechia", "Czech Republic", df$name)
      
    start_eurostat <- as.Date(gsub("-", "", input$start_eurostat), "%Y%m%d")
    stop_eurostat <- as.Date(gsub("-", "", input$stop_eurostat), "%Y%m%d")
    
    df <- df %>% 
      filter(sex == "Total", (date > start_eurostat & date < stop_eurostat)) %>% 
      select(name, values) %>% 
      group_by(name) %>% 
      summarise(Total = sum(values))
    
    return(df)
    
  })

  ### Output of the Eurostat map data as a gvisGeoChart
  output$eurostat_map <- renderGvis({
    
    gvisGeoChart(df_eurostat_map(), 
                 "name", 
                 "Total",
                 options = list(region = "150", 
                              width = 1000, 
                              height = 1000)
                 )
    
  })
  

  # porpawic nazwy country w input
  ### Preparing the Eursotat time series data 
  output$eurostat_ts <- renderPlotly({
    
    con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
    
    df <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    df$date <- as.Date(as.character(df$date_int), "%Y%m%d")
    
    country_input <- input$country
    sex_input <- input$sex
    
    start_date_input <- as.Date(gsub("-", "", input$start_date_eurostat_ts), "%Y%m%d") 
    stop_date_input <- as.Date(gsub("-", "", input$stop_date_eurostat_ts), "%Y%m%d")
    
    eurostat_plot <- df %>% 
      filter(name == country_input, sex == sex_input, (date > start_date_input & date < stop_date_input))
    
    plot <- ggplotly(
      ggplot(data = eurostat_plot, aes(x= date, y = values)) + 
        geom_line() +
        scale_x_date(limits = c(start_date_input, stop_date_input))
      )
    
    return(plot)
    
  })

  
  eurostat_ts_report <- reactive({
    
    con <- dbConnect(dbDriver("SQLite"), dbname = "database.db")
    
    df <- data.frame(dbGetQuery(con, "SELECT * FROM eurostat"))
    
    dbDisconnect(con)
    
    df$date <- as.Date(as.character(df$date_int), "%Y%m%d")
    
    country_input <- input$country
    sex_input <- input$sex
    
    start_date_input <- as.Date(gsub("-", "", input$start_date_eurostat_ts), "%Y%m%d") 
    stop_date_input <- as.Date(gsub("-", "", input$stop_date_eurostat_ts), "%Y%m%d")
    
    eurostat_plot <- df %>% 
      filter(name == country_input, sex == sex_input, (date > start_date_input & date < stop_date_input))

      return(eurostat_plot)
  })
  
  observeEvent(input$generate_report,{
    
    par <- list(df_eurostat_map = df_eurostat_map(), eurostat_ts_report = eurostat_ts_report(),
                SQL_query = SQL_query())
    
    if(file.exists("Report.html")) unlink("Report.html")
    if(file.exists("par")) unlink("par")
    if(dir.exists("cache")) unlink("cache",recursive = T, force = T)
    
    save(par,file="par")
    
    library(knitr)
    library(markdown)
    library(rmarkdown)
    
    knit(input='Report.Rmd', output="tmp.md",envir=new.env())
    markdownToHTML(file="tmp.md", output="Report.html")
    
    unlink("tmp.md")
    unlink("par")
  })
  
}) 

shinyApp(ui = ui, server = server)
