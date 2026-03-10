# app.R
# Pre-requisite packages
packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "RSQLite", "DBI", "lubridate", "randomForest")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos="http://cran.rstudio.com/")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
library(lubridate)
library(randomForest)

# --- 1. Load Data & Models ---
db_file <- "../data/canteen_dw.sqlite"
if (!file.exists(db_file)) db_file <- "data/canteen_dw.sqlite"

models_dir <- "../models"
if (!dir.exists(models_dir)) models_dir <- "models"

# Connect to DB to get basic info for filters
wh_con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
categories <- dbGetQuery(wh_con, "SELECT DISTINCT Category FROM Dim_Item")$Category
items <- dbGetQuery(wh_con, "SELECT DISTINCT ItemName FROM Dim_Item")$ItemName
dbDisconnect(wh_con)

# Load Models (handling potential absence if not trained yet)
rf_model_path <- file.path(models_dir, "rf_demand_model.rds")
rules_path <- file.path(models_dir, "association_rules.rds")
segments_path <- file.path(models_dir, "customer_segments.rds")

rf_model <- if (file.exists(rf_model_path)) readRDS(rf_model_path) else NULL
rules_df <- if (file.exists(rules_path)) readRDS(rules_path) else data.frame()
segments_df <- if (file.exists(segments_path)) readRDS(segments_path) else data.frame()


# --- 2. UI Definition ---
header <- dashboardHeader(title = "Smart Canteen Analytics")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Demand Forecasting", tabName = "forecasting", icon = icon("chart-line")),
    menuItem("Market Basket Analysis", tabName = "mba", icon = icon("shopping-cart")),
    menuItem("Customer Insights", tabName = "customers", icon = icon("users"))
  ),
  hr(),
  # Global Filters
  selectInput("category_filter", "Select Category:", choices = c("All", categories), selected = "All"),
  dateRangeInput("date_filter", "Date Range:", start = "2024-01-01", end = "2024-12-31")
)

body <- dashboardBody(
  tabItems(
    # --- Tab 1: Overview ---
    tabItem(tabName = "overview",
            fluidRow(
              valueBoxOutput("total_revenue", width = 4),
              valueBoxOutput("total_orders", width = 4),
              valueBoxOutput("avg_order_val", width = 4)
            ),
            fluidRow(
              box(title = "Revenue Trend", status = "primary", solidHeader = TRUE, width = 8,
                  plotOutput("revenue_trend_plot")),
              box(title = "Top Items by Revenue", status = "warning", solidHeader = TRUE, width = 4,
                  plotOutput("top_items_plot"))
            )
    ),
    
    # --- Tab 2: Forecasting ---
    tabItem(tabName = "forecasting",
            fluidRow(
              box(title = "Forecast Control", status = "info", width = 4,
                  selectInput("forecast_item", "Select Item to Forecast:", choices = items),
                  selectInput("forecast_weather", "Forecast Weather:", choices = c("Sunny", "Rainy", "Cloudy")),
                  numericInput("forecast_price", "Current Price:", value = 100),
                  dateInput("forecast_date", "Target Date:", value = Sys.Date() + 1),
                  actionButton("run_forecast", "Generate Forecast", class = "btn-success")
              ),
              box(title = "Predicted Demand", status = "success", solidHeader = TRUE, width = 8,
                  h2(textOutput("forecast_result_text")),
                  p("This prediction uses the trained Random Forest model based on historical patterns.")
              )
            )
    ),
    
    # --- Tab 3: Market Basket Analysis ---
    tabItem(tabName = "mba",
            fluidRow(
              box(title = "Frequently Bought Together (Apriori Rules)", status = "danger", solidHeader = TRUE, width = 12,
                  dataTableOutput("rules_table")
              )
            )
    ),
    
    # --- Tab 4: Customer Insights ---
    tabItem(tabName = "customers",
            fluidRow(
              box(title = "Customer Revenue by Day Type", status = "primary", solidHeader = TRUE, width = 12,
                  plotOutput("segment_plot")
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Reactive Data Fetcher
  filtered_data <- reactive({
    con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
    cat_query <- if (input$category_filter == "All") "" else paste0(" AND i.Category = '", input$category_filter, "'")
    date_start <- as.character(input$date_filter[1])
    date_end <- as.character(input$date_filter[2])
    
    query <- paste0("
      SELECT d.FullDate, i.ItemName, i.Category, i.Price, f.Quantity, f.TotalAmount, c.CustomerType 
      FROM Fact_Sales f 
      JOIN Dim_Item i ON f.ItemKey = i.ItemKey 
      JOIN Dim_Date d ON f.DateKey = d.DateKey 
      JOIN Dim_Customer c ON f.CustomerKey = c.CustomerKey 
      WHERE d.FullDate >= '", date_start, "' AND d.FullDate <= '", date_end, "'", cat_query)
    
    df <- dbGetQuery(con, query)
    dbDisconnect(con)
    df
  })
  
  # --- Overview Tab Outputs ---
  output$total_revenue <- renderValueBox({
    df <- filtered_data()
    val <- formatC(sum(df$TotalAmount, na.rm=TRUE), format="d", big.mark=",")
    valueBox(paste0("₹", val), "Total Revenue", icon = icon("credit-card"), color = "green")
  })
  
  output$total_orders <- renderValueBox({
    df <- filtered_data()
    val <- formatC(sum(df$Quantity, na.rm=TRUE), format="d", big.mark=",")
    valueBox(val, "Units Sold", icon = icon("shopping-basket"), color = "aqua")
  })
  
  output$avg_order_val <- renderValueBox({
    df <- filtered_data()
    val <- round(sum(df$TotalAmount, na.rm=TRUE) / sum(df$Quantity, na.rm=TRUE), 2)
    val <- ifelse(is.nan(val), 0, val)
    valueBox(paste0("₹", val), "Avg Unit Revenue", icon = icon("money-bill-wave"), color = "yellow")
  })
  
  output$revenue_trend_plot <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    
    trend <- df %>% group_by(FullDate) %>% summarise(DailyRev = sum(TotalAmount)) %>% mutate(FullDate = as.Date(FullDate))
    ggplot(trend, aes(x=FullDate, y=DailyRev)) + 
      geom_line(color="#3c8dbc", size=1) + 
      geom_smooth(method="loess", color="red", se=FALSE) +
      theme_minimal() + 
      labs(x="Date", y="Revenue (₹)")
  })
  
  output$top_items_plot <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    
    top <- df %>% group_by(ItemName) %>% summarise(Rev = sum(TotalAmount)) %>% top_n(10, Rev)
    ggplot(top, aes(x=reorder(ItemName, Rev), y=Rev)) + 
      geom_bar(stat="identity", fill="#f39c12") + 
      coord_flip() + 
      theme_minimal() + 
      labs(x="", y="Revenue (₹)")
  })
  
  # --- Forecasting Tab Outputs ---
  observeEvent(input$run_forecast, {
    if (is.null(rf_model)) {
      output$forecast_result_text <- renderText("Model not trained yet. Please run mining scripts first.")
      return()
    }
    
    # Prepare input dataframe matching model features
    target_day <- wday(as.Date(input$forecast_date), label=TRUE, abbr=FALSE)
    
    # Create base data frame with character strings first
    input_df <- data.frame(
      ItemName = input$forecast_item,
      Price = as.numeric(input$forecast_price),
      Weather = input$forecast_weather,
      DayOfWeek = as.character(target_day),
      stringsAsFactors = FALSE
    )
    
    # Then explicitly factorize using the model's exact levels
    input_df$ItemName <- factor(input_df$ItemName, levels = rf_model$forest$xlevels$ItemName)
    input_df$Weather <- factor(input_df$Weather, levels = rf_model$forest$xlevels$Weather)
    input_df$DayOfWeek <- factor(input_df$DayOfWeek, levels = rf_model$forest$xlevels$DayOfWeek)
    
    # Predict
    tryCatch({
      pred_qty <- predict(rf_model, newdata = input_df)
      output$forecast_result_text <- renderText(paste("Predicted Demand:", round(pred_qty), "Units"))
    }, error = function(e) {
      output$forecast_result_text <- renderText(paste("Error in prediction:", e$message))
    })
  })
  
  # --- MBA Tab Outputs ---
  output$rules_table <- renderDataTable({
    if(nrow(rules_df) == 0) {
      return(data.frame(Message=c("No association rules could be generated with the current synthetic data and Apriori thresholds.")))
    }
    
    res <- rules_df %>%
      select(Rules=rules, Support=support, Confidence=confidence, Lift=lift) %>%
      mutate_if(is.numeric, round, 3) %>%
      head(20) # Show top 20
      
    as.data.frame(res)
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # --- Customer Tab Outputs ---
  output$segment_plot <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    
    seg <- df %>% group_by(CustomerType, Category) %>% summarise(Rev = sum(TotalAmount), .groups='drop')
    ggplot(seg, aes(x=CustomerType, y=Rev, fill=Category)) + 
      geom_bar(stat="identity", position="dodge") + 
      theme_minimal() + 
      labs(x="Customer Type", y="Total Revenue (₹)", fill="Category") +
      scale_fill_brewer(palette="Set2")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
