#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

ui <- dashboardPage(
      dashboardHeader(title="User Data"),
  
  dashboardSidebar(    
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Download", tabName = "datadownload", icon = icon("download")),
      menuItem("Data Exploration", tabName = "dataexplore", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              titlePanel("About the Data"),
              fluidRow(
                column(12,
                       p("This app allows users to explore data related to user behaviors on various mobile devices."),
                       p("For more information on the data set: https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data"),
                       p("The sidebar allows users to subset the data."),
                       p("The Data Download tab displays the subsetted data and allows the file to be downloaded."),
                       p("The tab allows user to obtain the numeric and graphical summaries of the data."),
                       br(),
                       img(src = "https://cdn-icons-png.flaticon.com/512/0/191.png", height = "200px")
                )
              )
            ),
  
      tabItem(tabName = "datadownload",
              titlePanel("Data Download"),
              sidebarLayout(
                sidebarPanel(
                  h2("Select a subset of the data:"),
                 
                   radioButtons(
                    inputId = "device_model",
                    label = "Device Model",
                    choices = c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "iPhone 12", "Samsung Galaxy S21")
                  ),
                  
                  radioButtons(
                    inputId = "op_sys",
                    label = "Operating System",
                    choices = c("Android", "iOS")
                  ),
                  
                  radioButtons(
                    inputId = "gender",
                    label = "Gender",
                    choices = c("Male", "Female")
                  ),
                  
                  h2("Select Numeric Variables"),
                  
                  selectizeInput(
                    inputId = "numvar1",
                    label = "Numeric Variable 1",
                    choices = c("User.ID", "App.Usage.Time..min.day.","Screen.On.Time..hours.day.", "Battery.Drain..mAh.day.", "Number.of.Apps.Installed", "Data.Usage..MB.day.", "User.Behavior.Class")
                  ),
                  
                  selectizeInput(
                    inputId = "numvar2",
                    label = "Numeric Variable 2",
                    choices = c("User.ID", "App.Usage.Time..min.day.","Screen.On.Time..hours.day.", "Battery.Drain..mAh.day.", "Number.of.Apps.Installed", "Data.Usage..MB.day.", "User.Behavior.Class",
                    selected = "App.Usage.Time..min.day."
                  ),
                  # Action Button to apply the filters
                  actionButton("apply", "Apply Filters")
                ),
                
                mainPanel(
                  DT::dataTableOutput("data_table"),
                  downloadButton("download_data", "Download Filtered Data")
                )
              )
      ),
      
      tabItem(tabName = "dataexplore",
              titlePanel("Data Exploration"),
              h2("Select a subset of the data:"),
              
              radioButtons(
                inputId = "device_model",
                label = "Device Model",
                choices = c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "iPhone 12", "Samsung Galaxy S21")
              ),
              
              radioButtons(
                inputId = "op_sys",
                label = "Operating System",
                choices = c("Android", "iOS")
              ),
              
              radioButtons(
                inputId = "gender",
                label = "Gender",
                choices = c("Male", "Female")
              ),
              
              h2("Select Numeric Variables"),
              
              selectizeInput(
                inputId = "numvar1",
                label = "Numeric Variable 1",
                choices = c("User.ID", "App.Usage.Time..min.day.","Screen.On.Time..hours.day.", "Battery.Drain..mAh.day.", "Number.of.Apps.Installed", "Data.Usage..MB.day.", "User.Behavior.Class")
              ),
              
              selectizeInput(
                inputId = "numvar2",
                label = "Numeric Variable 2",
                choices = c("User.ID", "App.Usage.Time..min.day.","Screen.On.Time..hours.day.", "Battery.Drain..mAh.day.", "Number.of.Apps.Installed", "Data.Usage..MB.day.", "User.Behavior.Class",
                            selected = "App.Usage.Time..min.day."
                ),
                # Action Button to apply the filters
                actionButton("apply", "Apply Filters")
              ),
            
    
server <- function(input, output, session) {
  
  user_data <- reactive({
    read.csv("user_data.csv", header = TRUE)
  })
  
  filtered_data <- reactive({
    req(input$apply)
    
    data <- user_data()
    
    if (!is.null(input$DeviceModel)) {
      data <- data[data$DeviceModel %in% input$DeviceModel, ]
    }
    
    if (!is.null(input$OperatingSystem)) {
      data <- data[data$OperatingSystem %in% input$OperatingSystem, ]
    }
    
    if (!is.null(input$Gender)) {
      data <- data[data$Gender %in% input$Gender, ]
    }
    
    return(data)
  })
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$slider_ui_1 <- renderUI({
    req(input$numeric_)
    data <- user_data()
    range_vals <- range(data[[input$numvar1]], na.rm = TRUE)
    sliderInput("numeric_range_1", "Select Range for Numeric Variable 1:", 
                min = range_vals[1], max = range_vals[2], 
                value = range_vals, step = 1)
  })
  
  output$slider_ui_2 <- renderUI({
    req(input$numvar2)
    data <- user_data()
    range_vals <- range(data[[input$numvar2]], na.rm = TRUE)
    sliderInput("numeric_range_2", "Select Range for Numeric Variable 2:", 
                min = range_vals[1], max = range_vals[2], 
                value = range_vals, step = 1)
  })
  
  observeEvent(input$update_summary, {
    req(input$summary_var)
    data <- user_data()
 
    output$summary_stats <- renderPrint({
      summary(data[[input$summary_var]])
    })
    
    output$summary_plot <- renderPlot({
      ggplot(data, aes_string(x = input$summary_var)) + 
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Distribution of", input$summary_var))
    })
  })
  
    output$summary_stats <- renderPrint({
      summary(data[[input$summary_var]])
    })
    
    output$summary_plot <- renderPlot({
      ggplot(data, aes_string(x = input$summary_var)) + 
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Distribution of", input$summary_var))
    })
  }),
    
    output$summary_plot <- renderPlot({
      ggplot(data, aes_string(x = input$summary_var, y = input$numeric_summary_var)) + 
        geom_boxplot() +
        theme_minimal()
    })
    )
    
# Run the application 
shinyApp(ui = ui, server = server)
