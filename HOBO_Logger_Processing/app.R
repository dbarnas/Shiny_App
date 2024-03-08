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
    
      
      # # How many lines to skip at get go
      # numericInput("skip.num", "Number of lines to skip", 1, 
      #              min = NA, max = NA, step = NA)
      # ,
      
      # # Input: Checkbox if file has header ----
      # checkboxInput("col.names",
      #               "Keep column names", 
      #               TRUE)
      # ,
      
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               Tail = "tail",
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
                value = ("03/23/2022 8:14:00"))
      ,
      
      textInput("launch.end", "Enter date and time of launch end (M/D/YYYY H:M:S)",
                value = ("03/30/2022 07:56:00"))
      ,
      
      conditionalPanel(condition = "input.filetype == 'ct_type'",
      
      # Input: Select whether logger recorded in high or low or both ranges ----
      # radioButtons("range", "CT Range",
      #              choices = c(High = "high.range",
      #                          Low = "low.range"
      #                          #Both = "both.range"
      #                          ),
      #              selected = "high.range")
      # ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      # Input: Buttons for specifying single or double calibration
      radioButtons("single.double.cal", "Single or Double Calibration",
                   choices = c('One Calibration Time Point' = "single",
                               'Two (Pre- and Post-Deployment) Calibration Time Points' = "double"),
                   selected = "single")
      ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      # Include clarifying text ----
      helpText("LOGGER CALIBRATION DETAILS (Single Time Point Calibration or Pre-launch)")
      ,
      
      # Input: user inputs the date of calibration
      dateInput("date_input", "Enter Date",
                value = lubridate::mdy("03/23/2022"))
      ,
      
      # Input: user inputs the time of calibration
      textInput("time_input", "Enter time in 24h format (H:M:S)", 
                value = ("18:00:00"))
      ,
      
      # Input: Buttons for EC vs SC calibration
      radioButtons("ec.sc.cal", "Calibration value type",
                   choices = c('Electrical Conductivity' = "EC",
                               'Specific Conductance' = "SC"),
                   selected = "SC")
      ,
      
      # Input: user inputs the electrical conductivity value of the calibration reference solution
      numericInput("calValue", "Calibration value (uS/cm)",
                   value = 56258)
      ,
      
      # Input: user inputs the temperature value at time of calibration
      numericInput("calTemp", "Calibration Temperature (C)",
                   value = 25.7)
      ,
      
      # Horizontal line ----
      tags$hr()
      ,
      
      #########################################################################
      #########################################################################
      #########################################################################
      
      # Include clarifying text ----
      helpText("POST-LAUNCH LOGGER CALIBRATION DETAILS (only if Two Time Points is selected above)")
      
      ,
      
      # Input: user inputs the date of calibration
      dateInput("date_input.post", "Enter Date",
                value = lubridate::mdy("03/29/2022"))
      ,
      
      # Input: user inputs the time of calibration
      textInput("time_input.post", "Enter time in 24h format (H:M:S)", 
                value = ("17:08:00"))
      ,
      
      # Input: Buttons for EC vs SC calibration
      radioButtons("ec.sc.cal.post", "Calibration value type",
                   choices = c('Electrical Conductivity' = "EC",
                               'Specific Conductance' = "SC"),
                   selected = "SC")
      ,
      
      # Input: user inputs the electrical conductivity value of the calibration reference solution
      numericInput("calValue.post", "Calibration value (uS/cm)",
                   value = 56258)
      ,
      
      
      # Input: user inputs the temperature value at time of calibration
      numericInput("calTemp.post", "Calibration Temperature (C)",
                   value = 25.7)

      
      
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
                           
                           downloadButton('downloadCT', "Download CSV"), # download csv file
                           downloadButton('downloadCTPlot', "Download Plot") # download plot
                           
                  )),
                  
                  conditionalPanel(condition = "input.filetype == 'depth_type'",
                                   h3(img(src='silbigerlabart_jkerlin.png', align = "left", height = 160, width = 150),
                                      "Water Level Logger Data"), # add image inline with header
                  tabPanel(title = "Water Level",
                           textOutput("depth.text"),
                           tableOutput("depth.table"),
                           plotOutput("depth.plot"),
                           
                           downloadButton('downloadDepth', "Download CSV"), # download csv file
                           downloadButton("downloadDepthPlot", "Download Plot") # download plot
                  )),
                  
                  conditionalPanel(condition = "input.filetype == 'ph_type'",
                                   h3(img(src='silbigerlabart_jkerlin.png', align = "left", height = 160, width = 150),
                                      "pH Logger Data"), # add image inline with header
                  tabPanel(title = "pH",
                           textOutput("ph.text"),
                           tableOutput("ph.table"),
                           plotOutput("ph.plot"),
                           
                           downloadButton('downloadpH', "Download CSV"), # download csv file
                           downloadButton("downloadpHPlot", "Download Plot") # download plot
                  )),
                  
                  # Horizontal line ----
                  tags$hr()
                  ,
                  # Information and lab logo at bottom of page
                  img(src='Silbiger_Lab_Logo.png', align = "center", height = 70, width = 140),
                  helpText(HTML("<br>Shiny App created in RStudio by Danielle Barnas - Last updated March 2024 <br>Circular logo created by Jamie Kerlin"))
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
    
    if(input$filetype == "ct_type"){
      df1 <- read_csv(input$file1$datapath,
                    col_names = TRUE, # using default choice instead
                    skip = 1) 
    } else if(input$filetype == "depth_type"){
      df1 <- read_csv(input$file1$datapath,
                      col_names = TRUE, # using default choice instead
                      skip = 1) 
    } else if(input$filetype == "ph_type"){
      df1 <- read_csv(input$file1$datapath,
                      col_names = TRUE, # using default choice instead
                      skip = 2) 
    }
    
    return(df1)
    
  })
  
  #################################################################
  # PROCESSING RAW DATA
  #################################################################
  
  df.b <- reactive ({
    
    req(input$file1)
    
    if(input$filetype == "ct_type"){
      
      # if(input$range == "high.range"){ ## HIGH RANGE CT LOGGER DATA
        
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
    
    
    } else { # if two point calibration
      
      # unite date and time of calibration inputs for data filter
      inputDate.pre <- paste(input$date_input, input$time_input)
      inputDate.post <- paste(input$date_input.post, input$time_input.post)
      
      logger_cal_length <- df.b() %>% 
        filter(between(ymd_hms(Date), ymd_hms(inputDate.pre), ymd_hms(inputDate.post)))
      
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
      
      # use salinity instead of EC
      # logger EC values at time of calibration as Practical Salinity
      sal.insitu.pre <- gsw_SP_from_C(C = log.ec.pre * 0.001, t = log.temp.pre, p = 10)
      sal.insitu.post <- gsw_SP_from_C(C = log.ec.post * 0.001, t = log.temp.post, p = 10)
      
      
      # Offset between the calibration reference and the logger reading
      if(input$ec.sc.cal.post == 'ec'){ # if input value is EC
        
        # calibration values
        sal.pre <- gsw_SP_from_C(C = input$calValue * 0.001, t = input$calTemp, p = 10)
        sal.post <- gsw_SP_from_C(C = input$calValue.post * 0.001, t = input$calTemp.post, p = 10)
        
        # drift offset = calibration - logger
        offset.sal.pre <- sal.pre - sal.insitu.pre
        offset.sal.post <- sal.post - sal.insitu.post
  
        drift.cor.sal = (offset.sal.post - offset.sal.pre) / nrow(logger_cal_length) # total units drifted over time / total units
        
        
        
      } else { # if input value is SC
        
        # calibration values
        sal.pre <- gsw_SP_from_C(C = input$calValue * 0.001, t = 25, p = 10) # calculate practical salinity at 25C
        sal.post <- gsw_SP_from_C(C = input$calValue.post * 0.001, t = 25, p = 10) # calculate practical salinity at 25C
        
        # drift offset = calibration - logger
        offset.sal.pre <- sal.pre - sal.insitu.pre
        offset.sal.post <- sal.post - sal.insitu.post
        
        drift.cor.sal = (offset.sal.post - offset.sal.pre) / nrow(logger_cal_length) # total units drifted over time / total units
        
      
      }
      
      # temperature drift offset = calibration - logger
      offset.temp.pre <- input$calTemp - log.temp.pre # calculate temperature offset
      offset.temp.post <- input$calTemp.post - log.temp.post
      
      drift.cor.temp = (offset.temp.post - offset.temp.pre) / nrow(logger_cal_length) # total units drifted over time / total units
      
      
  
      ### Drift Compensation
      # df3 <- full_join(df3.pre, df3.post) # join both calibration time point dataframes
      df3 <- df.b() %>% 
        filter(between(ymd_hms(Date), ymd_hms(inputDate.pre), ymd_hms(inputDate.post))) %>% 
        arrange(Date) %>%
        
        # first correct temperature drift
        mutate(Temp.drift.init = offset.temp.pre,
               drift.cor.val = drift.cor.temp, # establish a column filled with the drift correction values
               drift.correction.temp = cumsum(drift.cor.val), # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
               Temp.drift.fin = Temp.drift.init + drift.correction.temp, # add the drift correction value to EC.drift.1 sequentially until we reach EC.drift.2 in final time point
               TempInSitu = TempInSitu + Temp.drift.fin) %>%  
        select(-drift.cor.val) %>% 
        
        # calculate practical salinity using new temperature
        mutate(Salinity_psu.uncor = gsw_SP_from_C(C = E_Conductivity * 0.001, t = TempInSitu, p = 10)) %>%  # USING SALINITY FOR OFFSET INSTEAD OF EC
        
        # then correct salinity drift
        mutate(Sal.drift.init = offset.sal.pre,
               drift.cor.val = drift.cor.sal, # establish a column filled with the drift correction values
               drift.correction.sal=cumsum(drift.cor.val), # fill the drift correction column with sequentially larger drift corrections from correlation value to full drift
               Sal.drift.fin = Sal.drift.init + drift.correction.sal, # add the drift correction value to EC.drift.1 sequentially until we reach EC.drift.2 in final time point
               Salinity_psu = Salinity_psu.uncor + Sal.drift.fin) %>% 
        select(-drift.cor.val)
        
    }
    
    # filter between launch dates
    df3 <- df3 %>%
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end))) %>% 
      select(LoggerID, Date, TempInSitu, Salinity_psu)
    
    
    return(df3)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################################################################
  # CT OUTPUTS
  #################################################################    
  
  output$contents <- renderTable({
    
    
    # return either head() or View() output
    if(input$disp == "head") {
      return(head(df.b()))
      
    } else if(input$disp == "tail") {
      return(tail(df.b()))
    
    } else {
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
    
    
    # return either head(), tail(), or View() output
    if(input$disp == "head") {
      return(head(df.c()))
    
    } else if(input$disp == "tail") {
      
      return(tail(df.c()))
    
    } else {
      return(df.c())
    }
    
  })
  
  
  # Download calibrated csv file
  
  output$downloadCT <- downloadHandler(
    filename = function() {
      paste0("Calibrated_",input$file1) # original file has .csv as part of filename already
    },
    content = function(con) {
      write_csv(df.c(), con)
    }
  )
  
  
  
  
  
  # create plot
  my_ctplot <- reactive ({
    
    dfcplot <- df.c() %>% 
      filter(between(ymd_hms(Date), mdy_hms(input$launch.start), mdy_hms(input$launch.end))) %>% 
      mutate(Date = ymd_hms(Date))
    
    calplot <- dfcplot %>% 
      ggplot(aes(x = Date,
                 y = Salinity_psu,
                 color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      labs(y = "Salinity (psu)",
           color = "Temperature (C)")
    
    return(calplot)
  })
  
  # render plot for output
  output$calibrated.plot <- renderPlot({
    
    my_ctplot()
    
  })
  
  
  # Download output plot
  output$downloadCTPlot <- downloadHandler(
    
    filename = function() { 
      paste0("plot_",input$file1,".png")
    },
    content = function(con2) {
      png(con2) #, paper = "default")
      plot(my_ctplot())
      dev.off()
    }
  )
  
  
  #################################################################
  # WATER LEVEL OUTPUTS
  ################################################################# 
  

  
  
  output$depth.table <- renderTable({
    
    # return either head(), tail(), or View() output
    if(input$disp == "head") {
      return(head(df.b()))
      
    } else if(input$disp == "tail") {
      
      return(tail(df.b()))
      
    } else {
      return(df.b())
    }
    
  })
  
  
  # Download calibrated csv file
  
  output$downloadDepth <- downloadHandler(
    filename = function() {
      paste0("Processed_",input$file1) # original file has .csv as part of filename already
    },
    content = function(dep) {
      write_csv(df.b(), dep)
    }
  )
  
  # create plot
  my_depthplot <- reactive ({
    
    depthplot <- df.b() %>% 
      mutate(Date = ymd_hms(Date)) %>% 
      ggplot(aes(x = Date,
                 y = -Depth_m,
                 color = TempInSitu)) +
      geom_point() +
      theme_bw() +
      labs(y = "Depth (m)",
           color = "Temperature (C)")
    
    return(depthplot)
  })
  
  # render plot for output
  output$depth.plot <- renderPlot({
    
    my_depthplot()
    
  })

  
  # Download output plot
  output$downloadDepthPlot <- downloadHandler(
    
    filename = function() { 
      paste0("plot_",input$file1,".png")
    },
    content = function(dep2) {
      png(dep2) #, paper = "default")
      plot(my_depthplot())
      dev.off()
    }
  )


  
  #################################################################
  # pH OUTPUTS
  ################################################################# 
  

  
  output$ph.table <- renderTable({
    
    # return either head(), tail(), or View() output
    if(input$disp == "head") {
      return(head(df.b()))
      
    } else if(input$disp == "tail") {
      
      return(tail(df.b()))
      
    } else {
      return(df.b())
    }
    
  })
  
  # Download calibrated csv file
  
  output$downloadpH <- downloadHandler(
    filename = function() {
      paste0("Processed_",input$file1) # original file has .csv as part of filename already
    },
    content = function(peh) {
      write_csv(df.b(), peh)
    }
  )
  
  
  # create plot
  my_phplot <- reactive ({
    
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
  
  # render plot output
  output$ph.plot <- renderPlot({
    
    my_phplot()
    
  })
  
  # Download output plot
  output$downloadpHPlot <- downloadHandler(
    filename = function() { 
      paste0("plot_",input$file1,".png")
      },
    content = function(peh2) {
      png(peh2) #, paper = "default")
      plot(my_phplot())
      dev.off()
    }
  )

  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)

