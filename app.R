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
            width = 12,
            title = "Monthly Energy Demand Patterns",
            status = "primary",
            solidHeader = TRUE,
            height = "600px",
            div(
              style = "position: absolute; right: 20px; top: 10px; z-index: 1000;",
              selectizeInput(
                "selected_months",
                "Select Months to Compare",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 3)
              )
            ),
            plotlyOutput("intraday_generation_plot", height = "500px")
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
  aggregated_data <- reactiveVal()
  
  # Initialize data
  observe({
    # Load and aggregate data
    raw_data <- load_historical_data(aggregate = FALSE)
    agg_data <- aggregate_by_month_halfhour(load_historical_data(aggregate = FALSE))
    aggregated_data(agg_data)
    
    # Update month choices
    if (!is.null(agg_data)) {
      months <- unique(agg_data$month)
      updateSelectizeInput(session, "selected_months",
                           choices = months,
                           selected = tail(months, 2))
    }
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
    data <- load_historical_data(aggregate = FALSE)()
    
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
  
  # Create the area plot
  output$intraday_generation_plot <- renderPlotly({
    # Get the data
    agg_data <- aggregated_data()
    selected <- input$selected_months
    
    req(agg_data, selected, length(selected) > 0)
    
    # Prepare data for plotting
    plot_data <- prepare_area_plot_data(agg_data, selected)
    
    # Create the plot
    plot <- plot_ly(plot_data, x = ~half_hour) %>%
      layout(
        template = "plotly_dark",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        title = list(
          text = "Average Intraday generation by Month",
          font = list(size = 20)
        ),
        xaxis = list(
          title = "Time of Day",
          tickangle = 45,
          gridcolor = "rgba(255,255,255,0.1)",
          tickfont = list(size = 10)
        ),
        yaxis = list(
          title = "Energy Demand (MW)",
          gridcolor = "rgba(255,255,255,0.1)"
        ),
        showlegend = TRUE,
        hovermode = "x unified"
      )
    
    # Add a trace for each selected month
    for (month in selected) {
      plot <- plot %>%
        add_trace(
          y = as.numeric(plot_data[[month]]),
          name = month,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = paste0("rgba(", 
                             sample(50:200, 1), ",",
                             sample(50:200, 1), ",",
                             sample(50:200, 1), ",0.2)"),
          line = list(width = 2)
        )
    }
    
    plot
  })
}

# Run the app
shinyApp(ui = ui, server = server)