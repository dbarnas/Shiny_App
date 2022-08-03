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
  theme = bslib::bs_theme(bootswatch = "minty",
                          version = 5),
  
  # Application title
  titlePanel("Process Onset HOBO Logger Data")
  ,

  sidebarLayout(
    
    
    sidebarPanel(


      # Include clarifying text ----
      helpText(HTML("1. Select a csv data file containing logger data", 
               "of your chosen type. <br>2. Select relevant data processing",
               "parameters (number of rows to skip when reading file, etc.) ",
               "<br>3. Input calibration data if relevant."))
      ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      # Input: choose what kind of file to process
      selectInput("filetype", "Select data type",
                  choices = c("Conductivity-Temperature" = "ct_type",
                              "Water Level" = "depth_type",
                              pH = "ph_type"),
                  selected = "ct_type")
      ,
      

      # Input: Select a file ----
      fileInput("file1", "Choose a CSV file",
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
      
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      ,
      
      
      # Input: Select whether logger recorded in F or C ----
      radioButtons("log.temp", "Recorded temperature units",
                   choices = c(C = "degC",
                               'F' = "degF"),
                   selected = "degC")
      ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      # Include clarifying text ----
      helpText("LOGGER LAUNCH DETAILS")
      ,
      
      textInput("launch.start", "Enter date and time of launch start (M/D/YYYY H:M:S)",
                value = ("07/17/2022 8:14:00"))
      ,
      
      textInput("launch.end", "Enter date and time of launch end (M/D/YYYY H:M:S)",
                value = ("07/25/2022 07:56:00"))
      ,
      
      conditionalPanel(condition = "input.filetype == 'ct_type'",
      
      # Input: Select whether logger recorded in high or low or both ranges ----
      radioButtons("range", "CT Range",
                   choices = c(High = "high.range",
                               Low = "low.range"
                               #Both = "both.range"
                               ),
                   selected = "high.range")
      ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      # Input: Buttons for specifying single or double calibration
      radioButtons("single.double.cal", "Single or Double Calibration",
                   choices = c('Single' = "single",
                               'Double' = "double"),
                   selected = "single")
      ,
      
      # Include clarifying text ----
      helpText("LOGGER CALIBRATION DETAILS (Single or Pre-launch)")
      ,
      
      # Input: user inputs the electrical conductivity value of the calibration reference solution
      numericInput("calValue", "Calibration value (uS/cm)",
                   value = 56258)
      ,
      
      # Input: Buttons for EC vs SC calibration
      radioButtons("ec.sc.cal", "Calibration value type",
                   choices = c('Electrical Conductivity' = "EC",
                               'Specific Conductance' = "SC"),
                   selected = "SC")
      ,
      
      # Input: user inputs the temperature value at time of calibration
      numericInput("calTemp", "Calibration Temperature (C)",
                   value = 25.7)
      ,
      
      
      # Input: user inputs the date of calibration
      dateInput("date_input", "Enter Date",
                value = lubridate::mdy("07/14/2022"))
      ,
      
      
      # Input: user inputs the time of calibration
      textInput("time_input", "Enter time in 24h format (H:M:S)", 
                value = ("17:08:00"))
      ,
      
      #########################################################################
      #########################################################################
      #########################################################################
      
      # Include clarifying text ----
      helpText("LOGGER CALIBRATION DETAILS (Post-launch)")
      ,
      
      # Input: user inputs the electrical conductivity value of the calibration reference solution
      numericInput("calValue.post", "Calibration value (uS/cm)",
                   value = 56314)
      ,
      
      # Input: Buttons for EC vs SC calibration
      radioButtons("ec.sc.cal.post", "Calibration value type",
                   choices = c('Electrical Conductivity' = "EC",
                               'Specific Conductance' = "SC"),
                   selected = "SC")
      ,
      
      # Input: user inputs the temperature value at time of calibration
      numericInput("calTemp.post", "Calibration Temperature (C)",
                   value = 29.3)
      ,
      
      
      # Input: user inputs the date of calibration
      dateInput("date_input.post", "Enter Date",
                value = lubridate::mdy("07/27/2022"))
      ,
      
      
      # Input: user inputs the time of calibration
      textInput("time_input.post", "Enter time in 24h format (H:M:S)", 
                value = ("07:54:00"))
      
      
      ) # end of CT condition
      
    )
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(
    
      
      tabsetPanel(id = 'tab.id',
                  # titles of tabs and what is displayed in each
                  conditionalPanel(condition = "input.filetype == 'ct_type'",
                                   h3(img(src='silbigerlabart_jkerlin.png', align = "left", height = 160, width = 150),
                                      "CT Logger Data"), # add image inline with header
                           tabPanel(title = "Conductivity-Temperature",
                           tableOutput("contents"),
                           plotOutput("plot"),  # Output: Plot of CT by date, colored by temp ----
                           tableOutput("cal.contents"),
                           plotOutput("calibrated.plot"),
                           
                           downloadButton('downloadCT', "Download CSV") # download csv file
                           
                  )),
                  
                  conditionalPanel(condition = "input.filetype == 'depth_type'",
                                   h3(img(src='silbigerlabart_jkerlin.png', align = "left", height = 160, width = 150),
                                      "Water Level Logger Data"), # add image inline with header
                  tabPanel(title = "Water Level",
                           textOutput("depth.text"),
                           tableOutput("depth.table"),
                           plotOutput("depth.plot"),
                           downloadButton('downloadDepth', "Download CSV") # download csv file
                  )),
                  
                  conditionalPanel(condition = "input.filetype == 'ph_type'",
                                   h3(img(src='silbigerlabart_jkerlin.png', align = "left", height = 160, width = 150),
                                      "pH Logger Data"), # add image inline with header
                  tabPanel(title = "pH",
                           textOutput("ph.text"),
                           tableOutput("ph.table"),
                           plotOutput("ph.plot"),
                           downloadButton('downloadpH', "Download CSV") # download csv file
                  )),
                  
                  # Horizontal line ----
                  tags$hr()
                  ,
                  # Information and lab logo at bottom of page
                  img(src='Silbiger_Lab_Logo.png', align = "center", height = 70, width = 140),
                  helpText(HTML("<br>Shiny App created in RStudio by Danielle Barnas - Last updated May 2022 <br>Circular logo created by Jamie Kerlin"))
      )
      
      
      
    )
  )
)




































######################################################################
######################################################################


## Define server logic to read selected file ----
server <- function(input, output) {
  
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  
  
  #################################################################
  # READ IN DATA FILE 
  #################################################################
  
  df.a <- reactive ({
    
    req(input$file1)
    
    df1 <- read_csv(input$file1$datapath,
                    col_names = input$col.names,
                    skip = input$skip.num)
    return(df1)
    
  })
  
  #################################################################
  # PROCESSING RAW DATA
  #################################################################
  
  df.b <- reactive ({
    
    req(input$file1)
    
    if(input$filetype == "ct_type"){
      
      if(input$range == "high.range"){ ## HIGH RANGE CT LOGGER DATA
        
        df2 <- df.a() %>%
          dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
          dplyr::select(LoggerID, contains('Date'), contains("High Range"), contains("Temp")) %>%
          dplyr::rename(Date = contains("Date"),
                        TempInSitu = contains("Temp"),
                        E_Conductivity = contains("High Range")) %>% 
          dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
          tidyr::drop_na()
        
        # correct for temperature in F
        if(input$log.temp == "degF"){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
        } 
        
        
      } else if(input$range == "low.range") { ## LOW RANGE CT LOGGER DATA
        
        df2 <- df.a() %>%
          dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
          dplyr::select(contains('Date'), contains("Low Range"), contains("Temp")) %>%
          dplyr::rename(Date=contains("Date"),
                        TempInSitu=contains("Temp"),
                        E_Conductivity=contains("Low Range")) %>%
          dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
          # mutate(TempInSitu = if_else(input$log.temp == "degC", TempInSitu, ((TempInSitu - 32) * 5 / 9))) %>%
          tidyr::drop_na()
        # correct for temperature in F
        if(input$log.temp == "degF"){
          df2 <- df2 %>% 
            mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
        } 
        
        
      }# else if(input$range == "both.range") { ## IF BOTH HIGH AND LOW CT LOGGER DATA
        
      #   df2 <- df.a() %>%
      #     dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
      #     dplyr::select(LoggerID, contains('Date'), contains("High Range"), contains("Low Range"), contains("Temp")) %>%
      #     dplyr::rename(Date=contains("Date"),
      #                   TempInSitu=contains("Temp"),
      #                   E_Conductivity_High=contains("High Range"),
      #                   E_Conductivity_Low=contains("Low Range")) %>%
      #     dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
      #     # dplyr::mutate(TempInSitu = if_else(input$log.temp == "degC", TempInSitu, ((TempInSitu - 32) * 5 / 9))) %>%
      #     tidyr::drop_na()
      #   # correct for temperature in F
      #   if(input$log.temp == "degF"){
      #     df2 <- df2 %>% 
      #       dplyr::mutate(TempInSitu = (TempInSitu - 32) * 5 / 9)
      #   } 
      #   
      # }
      
    } else if(input$filetype == "depth_type"){ ## WATER LEVEL LOGGER DATA
      
      df2 <- df.a() %>%
        dplyr::mutate(LoggerID = str_extract(string = colnames(df.a())[3], pattern = "[0-9]{8}")) %>%   # add column for Logger ID
        dplyr::select(contains('Date'), LoggerID, contains("Temp"), contains("Abs Pres"), contains("Water Level")) %>%
        dplyr::rename(Date = contains("Date"),
                      TempInSitu = contains("Temp"),
                      AbsPressure_kPa = contains("Abs Pres"),
                      Depth_m = contains("Water Level")) %>%
        dplyr::mutate(Date = as.character(mdy_hms(Date))) %>% 
        tidyr::drop_na() %>% 
        filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end)))
      
      
    } else if(input$filetype == "ph_type"){ ## PH LOGGER DATA
      
      
      df2 <- df.a() %>%
        dplyr::select(contains('Date'), contains("temp"), mV, pH) %>%
        dplyr::rename(Date = contains("Date"),
                      TempInSitu = contains("Temp")) %>%
        dplyr::mutate(Date = as.character(ymd_hms(Date))) %>% 
        tidyr::drop_na() %>% 
        filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end)))
      
    }
    
    return(df2)
    
  })
  
  
  
  
  #################################################################
  # CT LOGGER CALIBRATION
  #################################################################
  
  
  df.c <- reactive({
    
    req(input$file1)
    
    if(input$single.double.cal == 'single'){ # if single point calibration
    
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
    
    ################# end single calibration
    
    
    } else if(single.double.cal == 'double') { # if two point calibration
      
      # unite date and time of calibration inputs for data filter
      inputDate.pre <- paste(input$date_input, input$time_input)
      inputDate.post <- paste(input$date_input.post, input$time_input.post)
      
      
      # extract recorded temperature and EC value from logger at PRE calibration time
      log.temp.pre <- df.b() %>%
        filter(Date == inputDate.pre) %>% 
        select(TempInSitu) %>% 
        as.numeric()
      
      log.ec.pre<-df.b() %>%
        filter(Date == inputDate.pre) %>%
        select(E_Conductivity) %>% 
        as.numeric()
      
      # extract recorded temperature and EC value from logger at POST calibration time
      log.temp.post <- df.b() %>%
        filter(Date == inputDate.post) %>% 
        select(TempInSitu) %>% 
        as.numeric()
      
      log.ec.post<-df.b() %>%
        filter(Date == inputDate.post) %>%
        select(E_Conductivity) %>% 
        as.numeric()
      
      
      # Offset between the calibration reference and the logger reading
      if(input$ec.sc.cal.post == 'ec'){ # if input value is EC
        
        offset.ec.pre <- input$calValue - log.ec.pre # calculate offset using EC value
        offset.ec.post <- input$calValue.post - log.ec.post
        
      } else { # if input value is SC
        
        # Pre-launch
        sal.pre <- gsw_SP_from_C(C = input$calValue * 0.001, t = 25, p = 10) # calculate practical salinity at 25C
        cal.ec.pre <- 1000 * gsw_C_from_SP(SP = sal.pre, t = input$calTemp, p = 10) # back calculate EC from salinity at recorded temp
        
        offset.ec.pre <- cal.ec.pre - log.ec.pre # calculate offset using EC value
        
        # Post-launch
        sal.post <- gsw_SP_from_C(C = input$calValue.post * 0.001, t = 25, p = 10) # calculate practical salinity at 25C
        cal.ec.post <- 1000 * gsw_C_from_SP(SP = sal.post, t = input$calTemp.post, p = 10) # back calculate EC from salinity at recorded temp
        
        offset.ec.post <- cal.ec.post - log.ec.post # calculate offset using EC value
        
      }
      
      offset.temp.pre <- input$calTemp - log.temp.pre # calculate temperature offset
      offset.temp.post <- input$calTemp.post - log.temp.post
      
      # Apply offset to logger data
      # pre-launch calibration
      df3.pre <- df.b() %>%
        dplyr::mutate(EC_Cal = df.b()$E_Conductivity + offset.ec.pre,
                      TempInSitu_Cal = df.b()$TempInSitu + offset.temp.pre) %>%
        
        select(Date, LoggerID, TempInSitu_Cal, EC_Cal) %>% # only keep calibrated values and what we need
        rename(EC_Cal.pre = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_Cal.pre = TempInSitu_Cal)  # rename calibrated temperature readings, calibrated to secondary probe, if available
        
      
      # post-launch calibration
      df3.post <- df.b() %>%
        dplyr::mutate(EC_Cal = df.b()$E_Conductivity + offset.ec.post,
                      TempInSitu_Cal = df.b()$TempInSitu + offset.temp.post) %>%
        
        select(Date, LoggerID, TempInSitu_Cal, EC_Cal) %>% # only keep calibrated values and what we need
        rename(EC_Cal.post = EC_Cal, # rename electrical conductivity column as .1 for first/only time point
               TempInSitu_Cal.post = TempInSitu_Cal)   # rename calibrated temperature readings, calibrated to secondary probe, if available
        
      
      ### Drift Compensation
      df3 <- full_join(df3.pre, df3.post) # join both calibration time point dataframes
      
      drift <- df3 %>% 
        filter(between(Date, inputDate.pre, inputDate.post)) %>% # selects data between calibration times
        mutate(drift_EC = (EC_Cal.post - EC_Cal.pre)/length(drift$Date),   # Drift correction factor
               drift_Temp = (TempInSitu_Cal.post - TempInSitu_Cal.pre)/length(drift$Date))
      
      drift.cor.ec <- as.numeric(drift$drift_EC[1]) # correction value
      drift.cor.temp <- as.numeric(drift$drift_Temp[1])

      df3 <- df3 %>% 
        filter(between(Date, inputDate.pre, inputDate.post)) %>% # selects all data between calibration time points
        arrange(Date) %>%
        mutate(drift_EC = drift.cor.ec, # establish a column filled with the drift correction values
               drift_Temp = drift.cor.temp,
               EC.correction=cumsum(drift_EC), # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
               Temp.correction=cumsum(drift_Temp)) %>% 
        mutate(TempInSitu = TempInSitu_Cal.pre + Temp.correction,
               ECond.mS.cm = EC_Cal.pre + EC.correction) %>% 
        select(Date, LoggerID, TempInSitu, ECond.mS.cm) %>% 
        mutate(Salinity_psu = gsw_SP_from_C(C = ECond.mS.cm, t = TempInSitu, p = 10))
      
      
      
    }
    
    # filter between launch dates
    df3 <- df3 %>%
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end)))
    
    
    return(df3)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  #################################################################
  # CT OUTPUTS
  #################################################################    
  
  output$contents <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
  })
  
  
  output$plot <- renderPlot({
    
    dfbplot <- df.b() %>% 
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end))) %>% 
      mutate(Date = ymd_hms(Date))
    
    # return plot of data
    myplot <- ggplot(data = dfbplot, 
                     aes(x = Date, 
                         y = E_Conductivity, 
                         color = TempInSitu)) +
      geom_point() +
      theme_bw() +
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
    
    
    calplot <- df.c() %>% 
      mutate(Date = ymd_hms(Date)) %>%
      ggplot(aes(x = Date,
                 y = Salinity_psu,
                 color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      labs(y = "Salinity (psu)",
           color = "Temperature (C)")
    
    return(calplot)
  })
  
  
  
  
  
  # Download calibrated csv file
  
  output$downloadCT <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_calibrated_',input$file1) # original file has .csv as part of filename already
    },
    content = function(con) {
      write_csv(df.c(), con)
    }
  )
  
  
  
  
  
  
  
  #################################################################
  # WATER LEVEL OUTPUTS
  ################################################################# 
  
  
  
  # output$depth.text <- renderText({
  #   
  #   mytext <- "Water level processing is in progress..."
  #   
  #   return(mytext)
  # })
  
  
  output$depth.table <- renderTable({
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
    
  })
  
  output$depth.plot <- renderPlot({
    
    depthplot <- df.b() %>% 
      mutate(Date = ymd_hms(Date)) %>% 
      ggplot(aes(x = Date,
                 y = -Depth_m,
                 color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      labs(y = "Depth (m)",
           color = "Temperature (C)") +
      ylim(min(-df.b()$Depth_m) - 0.2,0)
    
    return(depthplot)
    
  })
  
  
  # Download calibrated csv file
  
  output$downloadDepth <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_calibrated_',input$file1) # original file has .csv as part of filename already
    },
    content = function(con) {
      write_csv(df.c(), con)
    }
  )
  
  
  
  #################################################################
  # pH OUTPUTS
  ################################################################# 
  
  # output$ph.text <- renderText({
  #   
  #   mytext <- "pH processing is in progress..."
  #   
  #   return(mytext)
  # })
  
  
  output$ph.table <- renderTable({
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
    }
    
    else {
      return(df.b())
    }
    
  })
  
  
  output$ph.plot <- renderPlot({
    
    phplot <- df.b() %>% 
      mutate(Date = ymd_hms(Date)) %>% 
      ggplot(aes(x = Date,
                 y = pH,
                 color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      labs(y = "pH (NBS)",
           color = "Temperature (C)")
    
    return(phplot)
    
  })
  
  
  # Download calibrated csv file
  
  output$downloadpH <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),'_calibrated_',input$file1) # original file has .csv as part of filename alread
    },
    content = function(con) {
      write_csv(df.c(), con)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)

