#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyverse)
library(here)
library(lubridate)
library(shinyTime)
library(gsw)
library(scales)
#library(mooreasgd)



# Define UI for application that has multiple tabs 
ui <- fluidPage(
  
  # theme
  #theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Application title
  titlePanel("Process Onset HOBO Logger Data")
  ,
  
  sidebarLayout(
    
    
    sidebarPanel(

      # Input: choose what kind of file to process
      selectInput("filetype", "Select data type",
                  choices = c("Conductivity-Temperature" = "ct_type",
                              "Water Level" = "depth_type",
                              pH = "ph_type"),
                  selected = "ct_type")
      ,

      
      
      # # Include clarifying text ----
      # helpText("Select a csv data file containing logger data", 
      #          "of your chosen type. Next select relevant data processing",
      #          "parameters (number of rows to skip when reading file, etc.) ",
      #          "Finally, input calibration data if relevant.")
      # ,
      # 
      
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      ,
      
      # How many lines to skip at get go
      numericInput("skip.num", "Number of lines to skip", 1, 
                   min = NA, max = NA, step = NA)
      ,
      
      # Input: Checkbox if file has header ----
      checkboxInput("col.names",
                    "Keep column names", 
                    TRUE)
      ,
      
      
      # Input: Select whether logger recorded in F or C ----
      radioButtons("log.temp", "Recorded temperature units",
                   choices = c(C = "degC",
                               'F' = "degF"),
                   selected = "degC")
      ,
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      ,
      

      # Horizontal line ----
      tags$hr()
      ,
      
      
      # Include clarifying text ----
      helpText("LOGGER LAUNCH DETAILS")
      ,
      
      textInput("launch.start", "Enter date and time of launch start (M/D/YYYY H:M:S)",
                value = ("03/18/2022 13:00:00"))
      ,
      
      textInput("launch.end", "Enter date and time of launch end (M/D/YYYY H:M:S)",
                value = ("03/22/2022 12:00:00"))
      ,
      
      
      ####### CONDUCTIVITY - TEMPERATURE panel options
      conditionalPanel(condition = "input.filetype == 'ct_type'",
                       
                       
                       # Input: Select whether logger recorded in high or low or both ranges ----
                       radioButtons("range", "CT logger range",
                                    choices = c(High = "high.range",
                                                Low = "low.range",
                                                Both = "both.range"),
                                    selected = "high.range")
                       ,
                       
                       
                       # Include clarifying text ----
                       helpText("LOGGER CALIBRATION DETAILS")
                       ,
                       
                       # Input: user inputs the electrical conductivity value of the calibration reference solution
                       numericInput("calValue", "Calibration value (uS/cm)",
                                    value = 53800)
                       ,
                       
                       # Input: Buttons for EC vs SC calibration
                       radioButtons("ec.sc.cal", "Calibration value type",
                                    choices = c('Electrical Conductivity' = "EC",
                                                'Specific Conductance' = "SC"),
                                    selected = "SC")
                       ,
                       
                       # Input: user inputs the temperature value at time of calibration
                       numericInput("calTemp", "Calibration temperature (C)",
                                    value = 29.3)
                       ,
                       
                       
                       # Input: user inputs the date of calibration
                       dateInput("date_input", "Enter calibration date (M/D/YYYY)",
                                 value = lubridate::mdy("03/22/2022"))
                       ,
                       
                       
                       # Input: user inputs the time of calibration
                       textInput("time_input", "Enter calibration time in 24h format (H:M:S)", 
                                 value = ("16:20:00"))
                       
                       
                       
                       # # Input: Buttons for calibration types
                       # radioButtons("cal.time", "Timing of calibration",
                       #              choices = c('Pre-Deployment' = "predeployment",
                       #                          'Post-Deployment' = "postdeployment",
                       #                          'Both' = "both"),
                       #              selected = "predeployment"),
                       # 
                       # radioButtons("cal.ref", "Type of calibration",
                       #              choices = c('One Reference' = "one.ref",
                       #                          'Two References' = "two.ref"),
                       #              selected = "one.ref"),
                       # 
                       
                       
      ), 
      
      
      
      ####### WATER LEVEL panel options
      conditionalPanel(condition = "input.filetype == 'depth_type'"
        
        
      ),
      
      
      
      
      ####### pH panel options
      conditionalPanel(condition = "input.filetype == 'ph_type'"

        
      )
      
      
    ) 
    
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      tabsetPanel(id = 'tab.id',
                  # titles of tabs and what is displayed in each
                  
                  tabPanel(title = "Conductivity-Temperature",
                           tableOutput("ct.raw"),
                           plotOutput("ct.plot"),  # Output: Plot of CT by date, colored by temp ----
                           tableOutput("cal.contents"),
                           plotOutput("calibrated.plot"),
                           downloadButton('downloadData', "Download CSV")
                  ),
                  
                  
                  tabPanel(title = "Water Level",
                           textOutput("depth.text"),
                           tableOutput("depth.raw"),
                           plotOutput("depth.plot"),
                           downloadButton('downloadData', "Download CSV")
                  ),
                  
                  
                  tabPanel(title = "pH",
                           textOutput("ph.text"),
                           tableOutput("ph.raw"),
                           plotOutput("ph.plot"),
                           downloadButton('downloadData', "Download CSV")
                  )
                  
      )
      
      
      
    )
  )
)


######################################################################
######################################################################


## Define server logic to read selected file ----
server <- function(input, output) {
  
  
  
  #################################################################
  # READ IN DATA FILE 
  #################################################################
  
  df.a <- reactive ({
    
    req(input$file1) # will not run until file is selected
    
    df1 <- read_csv(input$file1$datapath,
                    col_names = input$col.names,
                    skip = input$skip.num)
    return(df1)
    
  })
  
  #################################################################
  # PROCESSING RAW CT
  #################################################################
  
  df.b <- reactive ({
    
    req(input$file1) # will not run until file is selected
    
    if(input$tab.id == "Conductivity-Temperature"){
      
      if(input$range == "high.range"){
        
        df2 <- df.a() %>%
          dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
          dplyr::select(LoggerID, contains('Date'), contains("High Range"), contains("Temp")) %>%
          dplyr::rename(Date = contains("Date"),
                        TempInSitu = contains("Temp"),
                        E_Conductivity = contains("High Range")) %>% 
          dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
          tidyr::drop_na()
        
        # correct for temperature
        if(input$log.temp == "degC" & median(df2$TempInSitu) > 40){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
        } else if(input$log.temp == "degF" & median(df2$TempInSitu) < 40){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu * 9 / 5) + 32)
        }
        
        
      } else if(input$range == "low.range") {
        
        df2 <- df.a() %>%
          dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
          dplyr::select(contains('Date'), contains("Low Range"), contains("Temp")) %>%
          dplyr::rename(Date=contains("Date"),
                        TempInSitu=contains("Temp"),
                        E_Conductivity=contains("Low Range")) %>%
          dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
          # mutate(TempInSitu = if_else(input$log.temp == "degC", TempInSitu, ((TempInSitu - 32) * 5 / 9))) %>%
          tidyr::drop_na()
        
        # correct for temperature
        if(input$log.temp == "degC" & median(df2$TempInSitu) > 40){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
        } else if(input$log.temp == "degF" & median(df2$TempInSitu) < 40){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu * 9 / 5) + 32)
        }
        
        
      } else if(input$range == "both.range") {
        
        df2 <- df.a() %>%
          dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
          dplyr::select(contains('Date'), LoggerID, contains("High Range"), contains("Low Range"), contains("Temp")) %>%
          dplyr::rename(Date=contains("Date"),
                        TempInSitu=contains("Temp"),
                        E_Conductivity_High=contains("High Range"),
                        E_Conductivity_Low=contains("Low Range")) %>%
          dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
          # dplyr::mutate(TempInSitu = if_else(input$log.temp == "degC", TempInSitu, ((TempInSitu - 32) * 5 / 9))) %>%
          tidyr::drop_na()

        # correct for temperature
        if(input$log.temp == "degC" & mean(df2$TempInSitu) > 50){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
        } else if(input$log.temp == "degF" & mean(df2$TempInSitu) < 50){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu * 9 / 5) + 32)
        }
        
      }
      
      
      
      
      
      
      
      
      
    } else if(input$tab.id == "Water Level"){
      
      df2 <- c("Water Level processing is a work in progress.")
      
      
      df2 <- df.a() %>%
        dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
        dplyr::select(contains('Date'), LoggerID, contains("Temp"), contains("Abs Pres"), contains("Water Level")) %>%
        dplyr::rename(date = contains("Date"),
                      TempInSitu = contains("Temp"),
                      AbsPressure_kPa = contains("Abs Pres"),
                      Depth_m = contains("Water Level")) %>%
        tidyr::drop_na()
    
      
      
      
    
    } else if(input$tab.id == "pH"){
      
    
      df2 <- df.a() %>%
        dplyr::select(contains('Date'), contains("temp"), mV, pH) %>%
        dplyr::rename(Date = contains("Date"),
                      TempInSitu = contains("Temp")) %>%
        tidyr::drop_na()
      
      
    }
    
    return(df2)
    
  })
  
  
  
  
  #################################################################
  # CALIBRATION
  #################################################################
  
  
  df.c <- reactive({
    
    req(input$file1) # will not run until file is selected
    
    # unite date and time of calibration inputs for data filter
    inputDate <- paste(input$date_input, input$time_input)
    
    
    # extract recorded temperature and EC value from logger at calibration time
    log.temp <- df.b() %>%
      filter(Date == inputDate) %>% 
      select(TempInSitu) %>% 
      as.numeric()
    
    log.ec<-df.b() %>%
      filter(Date == inputDate) %>%
      select(E_Conductivity) %>% 
      as.numeric()
    
    
    # Offset between the calibration reference and the logger reading
    if(input$ec.sc.cal == 'ec'){ # if input value is EC
      
      offset.ec <- input$calValue - log.ec # calculate offset using EC value
      
    } else { # if input value is SC
      
      sal <- gsw_SP_from_C(C = input$calValue * 0.001, t = 25, p = 10) # calculate practical salinity at 25C
      cal.ec <- 1000 * gsw_C_from_SP(SP = sal, t = input$calTemp, p = 10) # back calculate EC from salinity at recorded temp
      
      offset.ec <- cal.ec - log.ec # calculate offset using EC value
      
    }
    
    offset.temp<-input$calTemp - log.temp # calculate temperature offset
    
    
    # Apply offset to logger data
    df3 <- df.b() %>%
      dplyr::mutate(EC_Cal = df.b()$E_Conductivity + offset.ec,
                    TempInSitu_Cal = df.b()$TempInSitu + offset.temp) %>%
      
      select(Date, LoggerID, TempInSitu_Cal, EC_Cal) %>% # only keep calibrated values and what we need
      rename(ECond.mS.cm = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
             TempInSitu = TempInSitu_Cal) %>%   # rename calibrated temperature readings, calibrated to secondary probe, if available
      mutate(Salinity_psu = gsw_SP_from_C(C = ECond.mS.cm*0.001, t = TempInSitu, p = 10))
    
    
    # filter between launch dates
    df3 <- df3 %>%
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end)))
    
    
    return(df3)
    
  })
  
  
  
  
  
  
  
  
  
  
  

  
  
  
#################################################################
  # CT OUTPUTS
################################################################# 
  
  
  output$ct.raw <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
  })
  
  
  output$ct.plot <- renderPlot({
    
    dfbplot <- df.b() %>% 
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end)))
    
    # return plot of data
    myplot <- ggplot(data = dfbplot, 
                     aes(x = Date, 
                         y = E_Conductivity, 
                         color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      scale_x_discrete(breaks= dfbplot$Date[seq(0, length(dfbplot$Date), by = length(dfbplot$Date)/4)]) +
      labs(y = "Electrical Conductivity (uS/cm)",
           color = "Temperature (C)")
    
    return(myplot)
  })    
  
  
  output$cal.contents <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.c()))
    }
    
    else {
      return(df.c())
    }
    
  })
  
  
  
  
  output$calibrated.plot <- renderPlot({
    
    
    calplot <- ggplot(data = df.c(),
                      aes(x = Date,
                          y = Salinity_psu,
                          color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      scale_x_discrete(breaks= df.c()$Date[seq(0, length(df.c()$Date), by = length(df.c()$Date)/4)]) +
      labs(y = "Salinity (psu)",
           color = "Temperature (C)")
    
    return(calplot)
  })
  
  
  
  # Download calibrated csv file
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_calibrated_',input$file1) # original file has .csv as part of filename alread
    },
    content = function(con) {
      write_csv(df.c(), con)
    }
  )
  
  
  
  
  
  
  
  #################################################################
  # WATER LEVEL OUTPUTS
  ################################################################# 
  
  
  
  output$depth.text <- renderText({
    
    mytext <- "Water level processing is in progress..."
    
    return(mytext)
  })
  
  
  output$depth.raw <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
  })
  
  
  output$depth.plot <- renderPlot({
    
    
    depthplot <- ggplot(data = df.b(),
                      aes(x = Date,
                          y = -Depth_m,
                          color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      scale_x_discrete(breaks= df.b()$Date[seq(0, length(df.b()$Date), by = length(df.b()$Date)/4)]) +
      labs(y = "Depth (m)",
           color = "Temperature (C)")
    
    return(depthplot)
  })
  
  
  # Download calibrated csv file
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_trimmed_',input$file1) # original file has .csv as part of filename alread
    },
    content = function(con) {
      write_csv(df.b(), con)
    }
  )
  
  
  
  
  #################################################################
  # pH OUTPUTS
  ################################################################# 
  
  output$ph.text <- renderText({
    
    mytext <- "pH processing is in progress..."
    
    return(mytext)
  })
  
  
  
  output$ph.raw <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
  })
  
  
  output$ph.plot <- renderPlot({
    
    
    phplot <- ggplot(data = df.b(),
                      aes(x = Date,
                          y = pH,
                          color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      scale_x_discrete(breaks= df.b()$Date[seq(0, length(df.b()$Date), by = length(df.b()$Date)/4)]) +
      labs(y = "pH (NBS)",
           color = "Temperature (C)")
    
    return(phplot)
  })
  
  
  # Download calibrated csv file
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_trimmed_',input$file1) # original file has .csv as part of filename alread
    },
    content = function(con) {
      write_csv(df.b(), con)
    }
  )
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)

