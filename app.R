# app.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(sass)
library(jsonlite)

# Source the data pipeline script
tryCatch({
  source("data_pipeline.R")
}, error = function(e) {
  message("Error loading data_pipeline.R: ", e$message)
})


generate_sample_data <- function() {
  data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), as.POSIXct("2024-12-31"), by="month"),
    metric_1 = runif(12, 100, 1000),
    metric_2 = rnorm(12, 500, 100),
    category = sample(c("A", "B", "C"), 12, replace=TRUE)
  )
}


# Load data function with proper error handling
load_dashboard_data <- function() {
  # Check if the function exists after sourcing
  if(exists("load_historical_data")) {
    tryCatch({
      data <- load_historical_data()
      if(!is.null(data)) {
        return(data)
      }
    }, error = function(e) {
      message("Error loading historical data: ", e$message)
    })
  }
  
  # If anything fails, use sample data
  message("Using sample data")
  return(generate_sample_data())
}

# Custom CSS
css <- sass::sass(
  sass::sass_file("www/custom.scss")
)

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Data Analytics Dashboard",
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(css)
    ),
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Welcome",
            status = "primary",
            solidHeader = TRUE,
            p("This dashboard provides insights into our collected data."),
            p("Use the sidebar to navigate between different views.")
          )
        ),
        fluidRow(
          valueBoxOutput("metric1_box", width = 4),
          valueBoxOutput("metric2_box", width = 4),
          valueBoxOutput("categories_box", width = 4)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Trends Over Time",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("trend_plot")
          )
        )
      ),
      
      # Raw Data Tab
      tabItem(
        tabName = "raw_data",
        fluidRow(
          box(
            width = 12,
            title = "Historical Data",
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 15px;",
              textOutput("data_info")
            ),
            DTOutput("raw_table")
          )
        )
      ),
      
      # Analysis Tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            width = 6,
            title = "Category Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("category_plot")
          ),
          box(
            width = 6,
            title = "Metric Correlation",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("correlation_plot")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data load
  dashboard_data <- reactiveVal()
  
  # Initialize data
  observe({
    dashboard_data(load_dashboard_data())
  })
  
  # Add refresh button to UI
  output$refresh_button <- renderUI({
    actionButton("refresh_data", "Refresh Data", 
                 icon = icon("sync"),
                 class = "btn-primary")
  })
  
  # Refresh data when button clicked
  observeEvent(input$refresh_data, {
    dashboard_data(load_dashboard_data())
  })
  
  # 
  # # Value boxes
  # output$metric1_box <- renderValueBox({
  #   valueBox(
  #     round(mean(dashboard_data$metric_1), 2),
  #     "Average Metric 1",
  #     icon = icon("chart-bar"),
  #     color = "blue"
  #   )
  # })
  # 
  # output$metric2_box <- renderValueBox({
  #   valueBox(
  #     round(mean(dashboard_data$metric_2), 2),
  #     "Average Metric 2",
  #     icon = icon("chart-line"),
  #     color = "purple"
  #   )
  # })
  # 
  # output$categories_box <- renderValueBox({
  #   valueBox(
  #     length(unique(dashboard_data$category)),
  #     "Categories",
  #     icon = icon("list"),
  #     color = "green"
  #   )
  # })
  # 
  # # Trend plot
  # output$trend_plot <- renderPlotly({
  #   plot_ly(dashboard_data, x = ~timestamp) %>%
  #     add_lines(y = ~metric_1, name = "Metric 1", line = list(color = 'blue')) %>%
  #     add_lines(y = ~metric_2, name = "Metric 2", line = list(color = 'purple')) %>%
  #     layout(
  #       title = "Metrics Over Time",
  #       xaxis = list(title = "Date"),
  #       yaxis = list(title = "Value")
  #     )
  # })
  # 
  # Raw data table
  output$raw_table <- renderDT({
    # Get the current value of dashboard_data
    data <- dashboard_data()
    
    # Verify we have data before rendering
    req(data)
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        # Add some nice formatting options
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (type === 'display' && typeof data === 'number') {",
              "    return data.toLocaleString('en-US', {maximumFractionDigits: 2});",
              "  }",
              "  return data;",
              "}"
            )
          )
        )
      ),
      # Make the table more visually appealing
      class = 'cell-border stripe',
      rownames = FALSE,
      # Format specific columns if needed
      callback = JS("table.on('draw.dt', function() {
        $('table.dataTable thead th').css('background-color', '#f5f5f5');
        $('table.dataTable tbody tr:odd').css('background-color', '#f9f9f9');
      })")
    )
  })
  
  # # Category distribution plot
  # output$category_plot <- renderPlotly({
  #   plot_ly(dashboard_data, x = ~category, y = ~count, type = "bar",
  #           marker = list(color = 'rgb(158,202,225)')) %>%
  #     layout(
  #       title = "Category Distribution",
  #       xaxis = list(title = "Category"),
  #       yaxis = list(title = "Count")
  #     )
  # })
  
  # # Correlation plot
  # output$correlation_plot <- renderPlotly({
  #   plot_ly(dashboard_data, x = ~metric_1, y = ~metric_2, type = "scatter", mode = "markers",
  #           marker = list(color = 'rgb(158,202,225)')) %>%
  #     layout(
  #       title = "Metric 1 vs Metric 2",
  #       xaxis = list(title = "Metric 1"),
  #       yaxis = list(title = "Metric 2")
  #     )
  # })
}

# Run the app
shinyApp(ui = ui, server = server)