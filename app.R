library(shiny)
library(shinythemes)
library(shinyjs)
library(highcharter)
library(dplyr)
library(readxl)
library(shinymanager)
library(keyring)

#setwd("C:/Users/gmatejka/r/shiny/deploy/kpi") # remove for online deployment
kpi_by_slide <- read_excel("kpi_app/slide_and_position_of_graphs.xlsx", col_names = T)

data <- readRDS("kpi_app/kpi.rds")
data$q1_q3_delta <- data$q3 - data$q1
data$Date <- as.Date(data$Date, "%d.%m.%Y")
data <- data[order(data$Date),]

kpi_by_slide$KPI <- kpi_by_slide$Kennzahl
data <- left_join(data, kpi_by_slide, by="KPI")

data <- data[order(data$Industry, data$Slide, data$Position, data$Date),]




source("kpi_app/colors.R", local=TRUE)

#https://github.com/StatisMike/shiny.reglog

  # credentials
  credentials <- data.frame(
    user = c("gabriel", "ewald", "markus", "robert", "victor"),
    password = c("revision", "revision", "revision", "revision", "revision"),
    stringsAsFactors = FALSE
  )
  
  ui <- fluidPage(
    
    theme = shinytheme("spacelab"),
    
    tags$head(
      tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '),
      
      tags$style("html, body { height: 100%; width: 100%}"),
      
      tags$style("#panel1 {
            height: 100px;
            position: fixed;
            background: lightgrey;
            padding: 20px
        }"),
      
      tags$style("#panel2 {
            overflow: auto;
            margin-left: 5px;
            position: center;
            padding: 20px
        }"),
      
      tags$style("#panel3 {
            overflow: auto;
            margin-left: 5px;
            position: center;
            padding: 20px
        }"),
      
      tags$style("#panel4 {
            background: darkblue;
            padding: 10px;
            color: white

      }"),
      
    ),
    
    absolutePanel(id = "panel1",
                  
                  height = "100%", width = "20%", right = "80%",
                  
                  selectizeInput(inputId= "industry", label= "Select Industry",
                                 choices= sort(unique(data$Industry)),
                                 selected= sort(unique(data$Industry))[1],
                                 multiple=F),
                  
                  fluidRow(
                    column(6,actionButton(inputId = "prev_industry", label = "Previous", style='width:140px; height:25px; padding:1px')),
                    column(6,actionButton(inputId = "next_industry", label = "Next", style='width:140px; height:25px; padding:1px; position: relative; left: -50px;'))
                  ),
                  HTML("<br><br>"),
                  
                  selectizeInput(inputId="kpi_group", label= "Select KPI Page",
                                 choices= unique(data$Group),
                                 selected= unique(data$Group)[1],
                                 multiple=F),
                  
                  fluidRow(
                    column(6,actionButton(inputId = "prev_kpi_group", label = "Previous", style='width:140px; height:25px; padding:1px')),
                    column(6,actionButton(inputId = "next_kpi_group", label = "Next", style='width:140px; height:25px; padding:1px; position: relative; left: -50px;'))
                  )
    ),
    
    absolutePanel(id = "panel2",
                  top = "5%", left = "25%", height = "85%", width = "35%", right = "0%", bottom = "20%",
                  fluidRow(
                    highchartOutput("hcontainer1", width="650px", height="400px"),
                    HTML("<br><br><br>"),
                    highchartOutput("hcontainer2", width="650px", height="400px")
                  ),
    ),
    
    absolutePanel(id = "panel3",
                  top = "5%", left = "60%", height = "85%", width = "40%", right = "0%", bottom = "20%",
                  fluidRow(
                    highchartOutput("hcontainer3", width="650px", height="400px"),
                    HTML("<br><br><br>"),
                    highchartOutput("hcontainer4", width="650px", height="400px")
                  ),
    ),
    
    absolutePanel(id = "panel4",
                  top = "90%", left = "20%", height = "10%", width = "80%", right = "0%", bottom = "0",
                  p("Digital version of the KPI Study DACH-Region created by WU Vienna Revision and KPMG Austria"),
                  p("The full version is available on our website: www.wu.ac.at/revision/transferstudien")
    )
  )
  
  
  set_labels(
    language = "en",
    "Please authenticate" = HTML('KPI-Studie DACH Region', '<br><br>', '<i>Digital Version</i>', '<br><br>','<small>v0.1 (in development) <br>https://www.wu.ac.at/revision/</small>'),
    "Username:" = "User:",
    "Password:" = "Password:",
    "Login" = "Einsteigen"
  )

  
  # change auth ui background
  ui <- secure_app(ui,
                   theme = shinythemes::shinytheme("spacelab"),
                   background  = "linear-gradient(rgba(0, 0, 50, 0), rgba(0, 0, 0, 0.01)),
                   url('https://i.ibb.co/QfcPYD9/background.png')  no-repeat center fixed;",
                   enable_admin = TRUE)
  
  
  server <- function(input, output, session) {
    
    # # call the server part
    # # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
    
    observe({
      print(input$shinymanager_where)
      print(input$shinymanager_language)
    })
    
    
    # Helper functions to get the previous and next item in a vector
    get_prev <- function(x) {
      idx <- which(unique(data$Industry) == x)
      if (idx == 1) {
        return(x)
      } else {
        return(unique(data$Industry)[idx-1])
      }
    }
    
    get_next <- function(x) {
      idx <- which(unique(data$Industry) == x)
      if (idx == length(unique(data$Industry))) {
        return(x)
      } else {
        return(unique(data$Industry)[idx+1])
      }
    }
    
    observeEvent(input$prev_industry, {
      industry <- isolate(input$industry)
      new_industry <- get_prev(industry)
      updateSelectizeInput(session, inputId = "industry", selected = new_industry)
    })
    
    observeEvent(input$next_industry, {
      industry <- isolate(input$industry)
      new_industry <- get_next(industry)
      updateSelectizeInput(session, inputId = "industry", selected = new_industry)
    })
    
    
    # Helper functions to get the previous and next item in a vector
    get_prev2 <- function(x2) {
      idx2 <- which(unique(data$Group) == x2)
      if (idx2 == 1) {
        return(x2)
      } else {
        return(unique(data$Group)[idx2-1])
      }
    }
    
    get_next2 <- function(x2) {
      idx2 <- which(unique(data$Group) == x2)
      if (idx2 == length(unique(data$Group))) {
        return(x2)
      } else {
        return(unique(data$Group)[idx2+1])
      }
    }
    
    observeEvent(input$prev_kpi_group, {
      kpi_group <- isolate(input$kpi_group)
      new_kpi_group <- get_prev2(kpi_group)
      updateSelectizeInput(session, inputId = "kpi_group", selected = new_kpi_group)
    })
    
    observeEvent(input$next_kpi_group, {
      kpi_group <- isolate(input$kpi_group)
      new_kpi_group <- get_next2(kpi_group)
      updateSelectizeInput(session, inputId = "kpi_group", selected = new_kpi_group)
    })
    
    

    
    output$path2<-renderPrint(paste0(shinybrowser::get_width()))
    
    sorted <- reactive({
      data[data$Industry == input$industry &
             data$Group == input$kpi_group, ]
    })
    
    sorted1 <- reactive({
      sorted()[sorted()$KPI == unique(sorted()$KPI)[1],]
    })
    
    sorted2 <- reactive({
      sorted()[sorted()$KPI == unique(sorted()$KPI)[2],]
    })
    
    sorted3 <- reactive({
      sorted()[sorted()$KPI == unique(sorted()$KPI)[3],]
    })
    
    sorted4 <- reactive({
      sorted()[sorted()$KPI == unique(sorted()$KPI)[4],]
    })
    
    
    ### Chart 1 ###
    output$hcontainer1 <- renderHighchart({
      hc <- highchart() %>%
        hc_add_series(data = as.numeric(sorted1()$q1_q3_delta),
                      type = "area",
                      name = "25 - 75 Quantile",
                      showInLegend = TRUE,
                      color=c_grey,
                      fillOpacity=1,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted1()$q1),
                      type = "area",
                      name = "",
                      showInLegend = FALSE,
                      color=c_grey,
                      fillOpacity=0,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted1()$median),
                      type = "line",
                      name = "Median",
                      dashStyle = "Dash",
                      lineWidth=2,
                      color = c_blue_dr,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE),
                      dataLabels = list(enabled = TRUE, 
                                        format = "{y:.1f}", 
                                        style = list(color = c_blue_dr))) %>%
        
        hc_add_series(data = as.numeric(sorted1()$mn),
                      type = "line",
                      name = "Long-Term Average",
                      dashStyle = "Dash",
                      lineWidth=1,
                      color = c_blue_lt,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE)) %>%
        
        # hc_annotations(
        #   list(
        #     labels = list(
        #       list(
        #         point = list(x=650, y=+120),
        #         text = sprintf("%0.0f", max(data$mn)),
        #         textAlign = "right",
        #         x = 0,
        #         y = 0,
        #         verticalAlign = "middle",
      #         backgroundColor = "transparent",
      #         borderWidth = 1,
      #         borderColor = c_blue_lt,
      #         padding = 5,
      #         style = list(fontWeight = "bold", color = c_blue_lt)
      #       )
      #     )
      #   )
      # ) %>%
      
      
      hc_title(text = paste0(input$industry, " | ", sorted1()$KPI[1]), style = list(color = c_blue_dr)) %>%
        
        hc_xAxis(categories = sorted1()$Date,
                 labels = list(format = '{value: %d.%m.%Y}')) %>%
        
        hc_yAxis(title = list(text = ""),
                 labels = list(format = "{value}%")) %>%
        
        # hc_tooltip(valueDecimals = 2,
        #            pointFormat = "Value: {point.y}%") %>%
        
        hc_tooltip(enabled = FALSE) %>%
        
        hc_legend(enabled = TRUE) %>%
        
        hc_credits(enabled = TRUE,
                   text = "Source: S&P Capital IQ, WU Revision",
                   style = list(fontSize = "10px"),
                   href = "https://www.wu.ac.at/revision/") %>%
        
        hc_plotOptions(
          area=list(stacking="normal",
                    enableMouseTracking=FALSE
          ),
          states = list(hover = list(enabled = FALSE)))
      hc
      
    })
    
    ### Chart 2 ###
    output$hcontainer2 <- renderHighchart({
      hc <- highchart() %>%
        hc_add_series(data = as.numeric(sorted2()$q1_q3_delta),
                      type = "area",
                      name = "25 - 75 Quantile",
                      showInLegend = TRUE,
                      color=c_grey,
                      fillOpacity=1,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted2()$q1),
                      type = "area",
                      name = "",
                      showInLegend = FALSE,
                      color=c_grey,
                      fillOpacity=0,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted2()$median),
                      type = "line",
                      name = "Median",
                      dashStyle = "Dash",
                      lineWidth=2,
                      color = c_blue_dr,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE),
                      dataLabels = list(enabled = TRUE, 
                                        format = "{y:.1f}", 
                                        style = list(color = c_blue_dr))) %>%
        
        hc_add_series(data = as.numeric(sorted2()$mn),
                      type = "line",
                      name = "Long-Term Average",
                      dashStyle = "Dash",
                      lineWidth=1,
                      color = c_blue_lt,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE)) %>%
        
        # hc_annotations(
        #   list(
        #     labels = list(
        #       list(
        #         point = list(x=650, y=+120),
        #         text = sprintf("%0.0f", max(data$mn)),
        #         textAlign = "right",
        #         x = 0,
        #         y = 0,
        #         verticalAlign = "middle",
      #         backgroundColor = "transparent",
      #         borderWidth = 1,
      #         borderColor = c_blue_lt,
      #         padding = 5,
      #         style = list(fontWeight = "bold", color = c_blue_lt)
      #       )
      #     )
      #   )
      # ) %>%
      
      
      hc_title(text = paste0(input$industry, " | ", sorted2()$KPI[1]), style = list(color = c_blue_dr)) %>%
        
        hc_xAxis(categories = sorted2()$Date,
                 labels = list(format = '{value: %d.%m.%Y}')) %>%
        
        hc_yAxis(title = list(text = ""),
                 labels = list(format = "{value}%")) %>%
        
        # hc_tooltip(valueDecimals = 2,
        #            pointFormat = "Value: {point.y}%") %>%
        
        hc_tooltip(enabled = FALSE) %>%
        
        hc_legend(enabled = TRUE) %>%
        
        hc_credits(enabled = TRUE,
                   text = "Source: S&P Capital IQ, WU Revision",
                   style = list(fontSize = "10px"),
                   href = "https://www.wu.ac.at/revision/") %>%
        
        hc_plotOptions(
          area=list(stacking="normal",
                    enableMouseTracking=FALSE
          ),
          states = list(hover = list(enabled = FALSE)))
      hc
      
    })
    
    ### Chart 3 ###
    output$hcontainer3 <- renderHighchart({
      hc <- highchart() %>%
        hc_add_series(data = as.numeric(sorted3()$q1_q3_delta),
                      type = "area",
                      name = "25 - 75 Quantile",
                      showInLegend = TRUE,
                      color=c_grey,
                      fillOpacity=1,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted3()$q1),
                      type = "area",
                      name = "",
                      showInLegend = FALSE,
                      color=c_grey,
                      fillOpacity=0,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted3()$median),
                      type = "line",
                      name = "Median",
                      dashStyle = "Dash",
                      lineWidth=2,
                      color = c_blue_dr,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE),
                      dataLabels = list(enabled = TRUE, 
                                        format = "{y:.1f}", 
                                        style = list(color = c_blue_dr))) %>%
        
        hc_add_series(data = as.numeric(sorted3()$mn),
                      type = "line",
                      name = "Long-Term Average",
                      dashStyle = "Dash",
                      lineWidth=1,
                      color = c_blue_lt,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE)) %>%
        
        # hc_annotations(
        #   list(
        #     labels = list(
        #       list(
        #         point = list(x=650, y=+120),
        #         text = sprintf("%0.0f", max(data$mn)),
        #         textAlign = "right",
        #         x = 0,
        #         y = 0,
        #         verticalAlign = "middle",
      #         backgroundColor = "transparent",
      #         borderWidth = 1,
      #         borderColor = c_blue_lt,
      #         padding = 5,
      #         style = list(fontWeight = "bold", color = c_blue_lt)
      #       )
      #     )
      #   )
      # ) %>%
      
      
      hc_title(text = paste0(input$industry, " | ", sorted3()$KPI[1]), style = list(color = c_blue_dr)) %>%
        
        hc_xAxis(categories = sorted3()$Date,
                 labels = list(format = '{value: %d.%m.%Y}')) %>%
        
        hc_yAxis(title = list(text = ""),
                 labels = list(format = "{value}%")) %>%
        
        # hc_tooltip(valueDecimals = 2,
        #            pointFormat = "Value: {point.y}%") %>%
        
        hc_tooltip(enabled = FALSE) %>%
        
        hc_legend(enabled = TRUE) %>%
        
        hc_credits(enabled = TRUE,
                   text = "Source: S&P Capital IQ, WU Revision",
                   style = list(fontSize = "10px"),
                   href = "https://www.wu.ac.at/revision/") %>%
        
        hc_plotOptions(
          area=list(stacking="normal",
                    enableMouseTracking=FALSE
          ),
          states = list(hover = list(enabled = FALSE)))
      hc
      
    })
    
    ### Chart 4 ###
    output$hcontainer4 <- renderHighchart({
      hc <- highchart() %>%
        hc_add_series(data = as.numeric(sorted4()$q1_q3_delta),
                      type = "area",
                      name = "25 - 75 Quantile",
                      showInLegend = TRUE,
                      color=c_grey,
                      fillOpacity=1,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted4()$q1),
                      type = "area",
                      name = "",
                      showInLegend = FALSE,
                      color=c_grey,
                      fillOpacity=0,
                      lineWidth=0,
                      lineColor="grey",
                      marker = list(enabled = FALSE)) %>%
        
        hc_add_series(data = as.numeric(sorted4()$median),
                      type = "line",
                      name = "Median",
                      dashStyle = "Dash",
                      lineWidth=2,
                      color = c_blue_dr,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE),
                      dataLabels = list(enabled = TRUE, 
                                        format = "{y:.1f}", 
                                        style = list(color = c_blue_dr))) %>%
        
        hc_add_series(data = as.numeric(sorted4()$mn),
                      type = "line",
                      name = "Long-Term Average",
                      dashStyle = "Dash",
                      lineWidth=1,
                      color = c_blue_lt,
                      enableMouseTracking=FALSE,
                      enableHover=FALSE,
                      marker = list(enabled = FALSE)) %>%
        
        # hc_annotations(
        #   list(
        #     labels = list(
        #       list(
        #         point = list(x=650, y=+120),
        #         text = sprintf("%0.0f", max(data$mn)),
        #         textAlign = "right",
        #         x = 0,
        #         y = 0,
        #         verticalAlign = "middle",
      #         backgroundColor = "transparent",
      #         borderWidth = 1,
      #         borderColor = c_blue_lt,
      #         padding = 5,
      #         style = list(fontWeight = "bold", color = c_blue_lt)
      #       )
      #     )
      #   )
      # ) %>%
      
      
      hc_title(text = paste0(input$industry, " | ", sorted4()$KPI[1]), style = list(color = c_blue_dr)) %>%
        
        hc_xAxis(categories = sorted4()$Date,
                 labels = list(format = '{value: %d.%m.%Y}')) %>%
        
        hc_yAxis(title = list(text = ""),
                 labels = list(format = "{value}%")) %>%
        
        # hc_tooltip(valueDecimals = 2,
        #            pointFormat = "Value: {point.y}%") %>%
        
        hc_tooltip(enabled = FALSE) %>%
        
        hc_legend(enabled = TRUE) %>%
        
        hc_credits(enabled = TRUE,
                   text = "Source: S&P Capital IQ, WU Revision",
                   style = list(fontSize = "10px"),
                   href = "https://www.wu.ac.at/revision/") %>%
        
        hc_plotOptions(
          area=list(stacking="normal",
                    enableMouseTracking=FALSE
          ),
          states = list(hover = list(enabled = FALSE)))
      hc
      
    })
    
  }
  
shinyApp(ui = ui, server= server)




























