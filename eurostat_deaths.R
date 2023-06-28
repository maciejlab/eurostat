library(ggplot2)
library(shiny)
library(DT)
library(DBI)
library(googleVis)
library(RSQLite)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Deaths in Europe based on Eurostat"),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
          
                tabPanel("Data downloading",
                         HTML(paste0("<h4>1. Download the data from the EUROSTAT website<h4>")),
                         HTML(paste0("<h4>2. Save it to the data/ directory<h4>")),
                         HTML(paste0("<h4>3. Extract the archive in the data/ folder<h4>")),
                         actionButton(inputId = "open_page_EUROSTAT", label = "Open EUROSTAT website"),
                         img(src = "eurdata.jpg"),
                         HTML(paste0("<h4>4. Create EUROSTAT database<h4>")),
                         actionButton(inputId = "create", label = "Create"),
                         textOutput("success")

                ),
                
                tabPanel("Map of Europe",
                         column(7, HTML(paste0("<p>Visualization in absolute values<p>")),
                                htmlOutput("get_abs_map"),
                                textInput("date_range_1",
                                          label = "Enter date range (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2020.01.01-2021.12.01"
                                ),
                                actionButton(inputId = "load_range_1", label = "Load")),

                         column(7, HTML(paste0("<p>Deaths per 100,000 population<p>")),
                                htmlOutput("get_rel_map"),
                                textInput("date_range_2",
                                          label = "Enter date range (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2015.01.01-2019.12.31"
                                ),
                                actionButton(inputId = "load_range_2", label = "Load"))
                ),

                tabPanel("Time series",
                         column(7, HTML(paste0("<p>Visualization in absolute values<p>")),
                                plotlyOutput("ts_abs",width = "800px", height = "500px"),
                                textInput("date_range_3",
                                          label = "Enter date range (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2012.01.01-2021.12.31"
                                ),
                                actionButton(inputId = "load_range_3", label = "Load")),

                         column(7, HTML(paste0("<p>Relative visualization<p>")),
                                plotlyOutput("ts_rel", width = "800px", height = "500px"),
                                textInput("date_range_4",
                                          label = "Enter date range (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2013.01.01-2021.12.31"
                                ),
                                actionButton(inputId = "load_range_4", label = "Load"))
                ),
                
                tabPanel("SQL",
                         textInput("sqlQuery",
                                   label = "Your SQL query",
                                   value = "select time, geo, value from eurostat where time like '2000%'"
                         ),
                         actionButton(inputId = "execute_SQL", label = "Load"),
                         DT::dataTableOutput("table1"),
                         downloadButton("download_data", label = "Download results of SQL query"),
                         plotlyOutput("plot")
                )
                
    )
  )
  
)


server <- function(input, output) {
  
  create_db <- reactive({
    
    dataDir <- file.path(getwd(),"data")

    if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}
    
    full_df <- read.table(file="data/demo_r_mwk_ts_linear.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
    
    selected_columns <- c("geo", "TIME_PERIOD", "OBS_VALUE", "sex")
    
    df <- full_df[selected_columns]
    
    colnames(df)[1] <- "geo"
    colnames(df)[2] <- "time"
    colnames(df)[3] <- "value"
    colnames(df)[4] <- "sex"
    
    try({
      write.table(df,file="data/eurostat_deaths.csv",sep=",",dec=",",row.names=F)
    })
    
    dbName <- file.path(dataDir,"eurostat_deaths.db")
    
    con <- dbConnect(
      dbDriver("SQLite"),
      dbname = dbName
    )

    try({
      df <- read.table(file=file.path(dataDir,"eurostat_deaths.csv"),sep=",",dec=",",header=T)
      dbWriteTable(con, "eurostat", df, overwrite = TRUE, row.names = FALSE)
    })
    
    dbDisconnect(con)
  })
  
  observeEvent(input$open_page_EUROSTAT,{
    browseURL("https://ec.europa.eu/eurostat/databrowser/view/DEMO_R_MWK_TS/default/table?lang=en")
  })
  
  observeEvent(input$create, {
    create_db()
    output$success <- renderText({
      "The database has been created, you can use the next tabs!"
    })
  })
  
  observeEvent(input$execute_SQL, {
    
    query <- eventReactive(input$sqlQuery,{
      
      conn <- dbConnect(RSQLite::SQLite(), "data/eurostat_deaths.db")
      
      result <- dbGetQuery(
        conn,
        statement = input$sqlQuery)
      
      return(result)
    })
    
    output$table1 <- renderDataTable({
      baza <- query()
      baza
    },
    options = list(pageLength = 10, info = FALSE,
                   lengthMenu = list(c(seq(10,100,10)), c(seq(10,100,10))) ))
    
    output$download_data <- downloadHandler(
      
      filename = function() { 
        return(paste0(gsub("-","_",as.character(Sys.Date())),"_out.csv")) 
      },
      content = function(file) {
        write.table(
          query(),
          file,
          sep=",",dec=".", row.names=F, col.names=T)
      }
    )
    
  })
  
  get_data <- function(date_from, date_to) {
    
    conn <- dbConnect(RSQLite::SQLite(), "data/eurostat_deaths.db")
    
    try({
      d <- dbGetQuery(conn, "select * from eurostat")
    })
    
    year <- substr(d$time, 1, 4)
    week <- substr(d$time, 7, 8)
    first_day_of_week <- as.Date(paste0(year, "-01-01"))
    output_date <- first_day_of_week + (as.integer(week) - 1) * 7
    d$time <- output_date
    
    aggrEu <- d[d$sex=="T",c("time","geo", "value")]
    
    aggrEu <- aggrEu[(aggrEu$time>= as.Date(date_from,format= "%Y-%m-%d") & aggrEu$time <= as.Date(date_to,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    aggrEu <- aggregate(aggrEu$value ~ aggrEu$geo, aggrEu, sum)
    
    colnames(aggrEu)[1] <- "geo"
    colnames(aggrEu)[2] <- "value"
    
    aggrEu <- aggrEu[aggrEu$geo != "EU27_2020", ]
    
    dbDisconnect(conn)
    
    return(aggrEu)
    
  }
  
  get_data_no_aggr <- function(date_from, date_to) {
    
    conn <- dbConnect(RSQLite::SQLite(), "data/eurostat_deaths.db")
    
    try({
      d <- dbGetQuery(conn, "select * from eurostat")
    })
    
    year <- substr(d$time, 1, 4)
    week <- substr(d$time, 7, 8)
    first_day_of_week <- as.Date(paste0(year, "-01-01"))
    output_date <- first_day_of_week + (as.integer(week) - 1) * 7
    d$time <- output_date
    
    aggrEu <- d[d$sex=="T",c("time","geo", "value")]
    
    aggrEu <- aggrEu[(aggrEu$time>= as.Date(date_from,format= "%Y-%m-%d") & aggrEu$time <= as.Date(date_to,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    aggrEu <- aggrEu[aggrEu$geo != "EU27_2020", ]
    
    dbDisconnect(conn)
    
    return(aggrEu)
  }
  
  population <- function() {
    country <- c("AL", "AD", "AT", "BE", "BA", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GR", "ES", "NL", "IE", "IS", "LI", "LT", "LU", "LV", "MK", "MT", "MD", "MC", "DE", "NO", "PL", "PT", "RU", "RO", "SM", "RS", "SK", "SI", "CH", "SE", "UA", "VA", "HU", "UK", "IT")
    
    population <- c(2876591, 77265, 9249773, 11556273, 3507017, 7025037, 4089400, 1217816, 10689209, 5850755, 1328976, 5540720, 67391582, 10741165, 46723749, 17420000, 4977400, 366130, 38380, 2790848, 634730, 1956625, 2077132, 514564, 2660133, 39242, 83020000, 5387002, 37970000, 10295909, 144463451, 19608302, 33931, 6963764, 5450421, 2066888, 8570146, 10327665, 44000000, 82517, 9660351, 60148229, 59685227)
    
    population_df <- data.frame(geo = country, population = population)
    
    return(population_df)
  }
  
  eurostat_abs <- eventReactive(input$load_range_1,{
    
    date_string <- input$date_range_1
    
    date_string <- strsplit(as.character(date_string),"-")
    
    date_from <- gsub('\\.','-',date_string[[1]][1])
    date_to <- gsub('\\.','-',date_string[[1]][2])
    
    result = get_data(date_from,date_to)
    
    return(result)
    
  })
  
  eurostat_rel <- eventReactive(input$load_range_2,{
    
    date_string <- input$date_range_2
    
    date_string <- strsplit(as.character(date_string),"-")
    
    date_from <- gsub('\\.','-',date_string[[1]][1])
    date_to <- gsub('\\.','-',date_string[[1]][2])
    
    result = get_data(date_from,date_to)
    
    return(result)
    
  })  
  
  timeseries_abs <- eventReactive(input$load_range_3,{
    
    date_string <- input$date_range_3
    
    date_string <- strsplit(as.character(date_string),"-")
    
    date_from <- gsub('\\.','-',date_string[[1]][1])
    date_to <- gsub('\\.','-',date_string[[1]][2])
    
    result = get_data_no_aggr(date_from,date_to)
    
    return(result)
    
  })
  
  timeseries_rel <- eventReactive(input$load_range_4,{
    
    date_string <- input$date_range_4
    
    date_string <- strsplit(as.character(date_string),"-")
    
    date_from <- gsub('\\.','-',date_string[[1]][1])
    date_to <- gsub('\\.','-',date_string[[1]][2])
    
    result = get_data_no_aggr(date_from,date_to)
    
    return(result)
    
  })
  
  output$get_abs_map <- renderGvis({
    
    gvisGeoChart(eurostat_abs(), "geo", "value",
                 options=list(region="150",
                              displayMode="countries"
                 ))
  })
  
  output$get_rel_map <- renderGvis({
    
    eurostat_data <- eurostat_rel()
    
    merged_df <- merge(eurostat_data, population(), by = "geo")
    
    merged_df$deaths_per_100k <- round((merged_df$value / merged_df$population) * 100000)
    
    merged_df$geo[merged_df$geo == "UK"] <- "GB"
    
    gvisGeoChart(merged_df, "geo", "deaths_per_100k",
                 options=list(region="150",
                              displayMode="countries"
                 ))
  })
  
  output$ts_abs <- renderPlotly({
    
    aggrEu <- timeseries_abs()
    
    aggrEu$time <- as.POSIXct(aggrEu$time,tz="GMT")
    
    img <- (
      ggplot(aggrEu,aes(x=time,y=value,col=geo))
      + 
        geom_line()
    )
    
    return(img)
    
  })
  
  output$ts_rel <- renderPlotly({
    
    eurostat_data <- timeseries_rel()
    
    merged_df <- merge(eurostat_data, population(), by = "geo")
    
    merged_df$deaths_per_100k <- round((merged_df$value / merged_df$population) * 100000)
    
    merged_df$geo[merged_df$geo == "UK"] <- "GB"
    
    eurostat_data$time <- as.POSIXct(eurostat_data$time,tz="GMT")
    
    img <- (
      ggplot(merged_df,aes(x=time,y=deaths_per_100k,col=geo))
      + 
        geom_line()
    )
    
    return(img)
  })
  
}

shinyApp(ui = ui, server = server)