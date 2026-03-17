
# app.R
# Pre-requisite packages
# Logic
packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "RSQLite", "DBI", "lubridate", "randomForest", "corrplot", "DT")
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
library(corrplot)
library(DT)

# --- 1. Load Data & Models ---
db_file <- "../data/canteen_dw.sqlite"
if (!file.exists(db_file)) db_file <- "data/canteen_dw.sqlite"

models_dir <- "../models"
if (!dir.exists(models_dir)) models_dir <- "models"

# Connect to DB to get basic info for filters
wh_con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
categories <- dbGetQuery(wh_con, "SELECT DISTINCT Category FROM Dim_Item")$Category
items <- dbGetQuery(wh_con, "SELECT DISTINCT ItemName FROM Dim_Item")$ItemName
date_range <- dbGetQuery(wh_con, "SELECT MIN(FullDate) as min_date, MAX(FullDate) as max_date FROM Dim_Date")
dbDisconnect(wh_con)

min_date <- ifelse(is.na(date_range$min_date), "2020-01-01", date_range$min_date)
max_date <- ifelse(is.na(date_range$max_date), as.character(Sys.Date()), date_range$max_date)

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
    menuItem("Visual Analysis (EDA)", tabName = "eda", icon = icon("chart-bar")),
    menuItem("Demand Forecasting", tabName = "forecasting", icon = icon("chart-line")),
    menuItem("Market Basket Analysis", tabName = "mba", icon = icon("shopping-cart")),
    menuItem("Customer Insights", tabName = "customers", icon = icon("users"))
  ),
  hr(),
  # Global Filters
  selectInput("category_filter", "Select Category:", choices = c("All", categories), selected = "All"),
  dateRangeInput("date_filter", "Date Range:", start = min_date, end = max_date)
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
            ),
            fluidRow(
              box(title = "Dataset Overview (Recent Sales)", status = "info", solidHeader = TRUE, width = 12,
                  DTOutput("raw_data_table"))
            )
    ),
    
    # --- Tab 1.5: Visual Analysis (EDA) ---
    tabItem(tabName = "eda",
            fluidRow(
              box(title = "Boxplot: Total Amount", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("boxplot_total")),
              box(title = "Histogram: Quantity", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("hist_qty"))
            ),
            fluidRow(
              box(title = "Scatter Plot: Quantity vs Total Amount", status = "warning", solidHeader = TRUE, width = 6,
                  plotOutput("scatter_qty_amt")),
              box(title = "Correlation Heatmap", status = "danger", solidHeader = TRUE, width = 6,
                  plotOutput("corr_heatmap"))
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
                  p("This prediction uses the trained Random Forest model based on historical patterns."),
                  p("Note: Due to dynamic dataset updates, exact item/weather matching must correspond to training data.")
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
              box(title = "Customer Revenue by Category", status = "primary", solidHeader = TRUE, width = 12,
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
      SELECT d.FullDate, i.ItemName, i.Category, i.Price, f.UnitCost, f.Quantity, f.TotalAmount, c.CustomerType 
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
    
    trend <- df %>% group_by(FullDate) %>% summarise(DailyRev = sum(TotalAmount), .groups='drop') %>% mutate(FullDate = as.Date(FullDate))
    ggplot(trend, aes(x=FullDate, y=DailyRev)) + 
      geom_line(color="#3c8dbc", size=1) + 
      geom_smooth(method="loess", color="red", se=FALSE) +
      theme_minimal() + 
      labs(x="Date", y="Revenue (₹)")
  })
  
  output$top_items_plot <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    
    top <- df %>% group_by(ItemName) %>% summarise(Rev = sum(TotalAmount), .groups='drop') %>% top_n(10, Rev)
    ggplot(top, aes(x=reorder(ItemName, Rev), y=Rev)) + 
      geom_bar(stat="identity", fill="#f39c12") + 
      coord_flip() + 
      theme_minimal() + 
      labs(x="", y="Revenue (₹)")
  })
  
  output$raw_data_table <- renderDT({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # --- EDA Tab Outputs ---
  output$boxplot_total <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    ggplot(df, aes(x=Category, y=TotalAmount, fill=Category)) +
      geom_boxplot() +
      theme_minimal() +
      labs(y="Total Amount (₹)", x="") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")
  })
  
  output$hist_qty <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    ggplot(df, aes(x=Quantity)) +
      geom_histogram(binwidth=1, fill="#00a65a", color="black") +
      theme_minimal() +
      labs(x="Quantity", y="Frequency")
  })
  
  output$scatter_qty_amt <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    ggplot(df, aes(x=Quantity, y=TotalAmount, color=Category)) +
      geom_point(alpha=0.7, size=3) +
      theme_minimal() +
      labs(x="Quantity", y="Total Amount (₹)")
  })
  
  output$corr_heatmap <- renderPlot({
    df <- filtered_data()
    if(nrow(df) == 0) return(NULL)
    num_df <- df %>% select(Quantity, TotalAmount, Price, UnitCost) %>% na.omit()
    if(ncol(num_df) > 1 && nrow(num_df) > 1) {
      cor_matrix <- cor(num_df)
      corrplot(cor_matrix, method="color", type="upper", addCoef.col="black", tl.col="black", tl.srt=45, mar=c(0,0,0,0))
    }
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
