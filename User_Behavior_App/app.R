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
                       p("This app allows users to explore data related to user behaviors on various mobile devices."),,
                       br(),
                       img(src = "https://example.com/image.png", height = "200px")
                )
              )
            ),
  
      tabItem(tabName = "datadownload",
              titlePanel("Data Download"),
              sidebarLayout(
                sidebarPanel(
                      checkboxGroupInput("DeviceModel", "Select Device Model(s):",
                                         choices = c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", 	
                                                     "iPhone 12", "Samsung Galaxy S21"),  # Replace with actual categories
                                         selected = c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", 	
                                                      "iPhone 12", "Samsung Galaxy S21")),
                      
                      checkboxGroupInput("OperatingSystem", "Select Operating System(s):",
                                         choices = c("Android", "iOS"),
                                         selected = c("Android", "iOS")),
                      
                      checkboxGroupInput("Gender", "Select Gender(s):",
                                         choices = c("Male", "Female"),
                                         selected = c("Male", "Female")),
                      
                      # Action Button to apply the filters
                      actionButton("apply", "Apply Filters")
                    ),
                  )
                  ),
                  
                mainPanel(
                  DT::dataTableOutput("data_table"),
                  downloadButton("download_data", "Download Filtered Data")
                )
              )
            ),
      
      tabItem(tabName = "dataexplore",
              titlePanel("Data Exploration"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("summary_var", "Select Variable for Summary:",
                              choices = c("DeviceModel", "OperatingSystem", "Gender")),
                  selectInput("numeric_summary_var", "Select Numeric Variable for Summary:",
                              choices = c("Usage", "Score")),
                  actionButton("update_summary", "Update Summary")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Summary Statistics", verbatimTextOutput("summary_stats")),
                    tabPanel("Plot", plotOutput("summary_plot"))
                  )
                )
              )
      )
)

    

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  user_data <- reactive({
    # Replace with the actual path to your dataset
    read.csv("user_behavior_dataset.csv", header = TRUE)
  })
  
  observe({
    data <- user_data()
    
    updateCheckboxGroupInput(session, "DeviceModel", 
                             choices = unique(data$DeviceModel), 
                             selected = unique(data$DeviceModel))
    
    updateCheckboxGroupInput(session, "OperatingSystem", 
                             choices = unique(data$OperatingSystem), 
                             selected = unique(data$OperatingSystem))
    
    updateCheckboxGroupInput(session, "Gender", 
                             choices = unique(data$Gender), 
                             selected = unique(data$Gender))
    
    updateCheckboxGroupInput(session, "DeviceModel_download", 
                             choices = unique(data$DeviceModel), 
                             selected = unique(data$DeviceModel))
    
    updateCheckboxGroupInput(session, "OperatingSystem_download", 
                             choices = unique(data$OperatingSystem), 
                             selected = unique(data$OperatingSystem))
    
    updateCheckboxGroupInput(session, "Gender_download", 
                             choices = unique(data$Gender), 
                             selected = unique(data$Gender))
  })
  
  output$slider_ui_1 <- renderUI({
    req(input$numeric_var_1)
    data <- user_data()
    range_vals <- range(data[[input$numeric_var_1]], na.rm = TRUE)
    sliderInput("numeric_range_1", "Select Range for Numeric Variable 1:", 
                min = range_vals[1], max = range_vals[2], 
                value = range_vals, step = 1)
  })
  
  output$slider_ui_2 <- renderUI({
    req(input$numeric_var_2)
    data <- user_data()
    range_vals <- range(data[[input$numeric_var_2]], na.rm = TRUE)
    sliderInput("numeric_range_2", "Select Range for Numeric Variable 2:", 
                min = range_vals[1], max = range_vals[2], 
                value = range_vals, step = 1)
  })
  
  filtered_data <- reactive({
    req(input$apply)  # Trigger only when "Apply Filters" is clicked
    data <- user_data()
    
    # Apply categorical filters
    data_filtered <- data[data$DeviceModel %in% input$DeviceModel, ]
    data_filtered <- data_filtered[data_filtered$OperatingSystem %in% input$OperatingSystem, ]
    data_filtered <- data_filtered[data_filtered$Gender %in% input$Gender, ]
    
    # Apply numeric filters
    data_filtered <- data_filtered[data_filtered[[input$numeric_var_1]] >= input$numeric_range_1[1] & 
                                     data_filtered[[input$numeric_var_1]] <= input$numeric_range_1[2], ]
    
    data_filtered <- data_filtered[data_filtered[[input$numeric_var_2]] >= input$numeric_range_2[1] & 
                                     data_filtered[[input$numeric_var_2]] <= input$numeric_range_2[2], ]
    
    return(data_filtered)
  })
  
  # Render filtered data table
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data.csv") },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
  
  # Summary statistics and plot rendering in Data Exploration Tab
  observeEvent(input$update_summary, {
    req(input$summary_var, input$numeric_summary_var)
    data <- filtered_data()
    
    # Render numeric summaries
    output$summary_stats <- renderPrint({
      summary(data[[input$numeric_summary_var]])
    })
    
    output$summary_plot <- renderPlot({
      ggplot(data, aes_string(x = input$summary_var, y = input$numeric_summary_var)) + 
        geom_boxplot() +
        theme_minimal()
    })
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
