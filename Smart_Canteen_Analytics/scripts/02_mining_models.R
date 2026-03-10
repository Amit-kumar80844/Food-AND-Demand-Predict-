# 02_mining_models.R
# Pre-requisite packages
packages <- c("dplyr", "lubridate", "RSQLite", "DBI", "caret", "randomForest", "arules")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos="http://cran.rstudio.com/")

library(dplyr)
library(lubridate)
library(DBI)
library(RSQLite)
library(caret)
library(randomForest)
library(arules)

print("Starting Data Mining & ML Models...")

# 1. Connect to Data Warehouse and Extract Data
db_file <- "data/canteen_dw.sqlite"
if (!file.exists(db_file) && file.exists("../data/canteen_dw.sqlite")) {
    db_file <- "../data/canteen_dw.sqlite"
}

wh_con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Fetch consolidated data for modeling
query <- "
SELECT 
  f.SalesKey,
  d.FullDate,
  d.DayOfWeek,
  i.ItemName,
  i.Category,
  i.Price,
  c.CustomerType,
  f.Quantity,
  f.TotalAmount,
  f.Weather
FROM Fact_Sales f
JOIN Dim_Date d ON f.DateKey = d.DateKey
JOIN Dim_Item i ON f.ItemKey = i.ItemKey
JOIN Dim_Customer c ON f.CustomerKey = c.CustomerKey
"
sales_data <- dbGetQuery(wh_con, query)
dbDisconnect(wh_con)

print(paste("Data retrieved for modeling:", nrow(sales_data), "rows."))

# Models Directory
models_dir <- "models"
if (!dir.exists(models_dir) && dir.exists("../models")) {
    models_dir <- "../models"
}
if (!dir.exists(models_dir)) dir.create(models_dir)


# -----------------------------------------------------
# MODEL 1: Demand Forecasting (Random Forest)
# -----------------------------------------------------
print("Training Demand Forecasting Model (Random Forest)...")

# Feature Engineering: Aggregate daily demand per item
demand_data <- sales_data %>%
  group_by(FullDate, ItemName, Category, DayOfWeek, Weather, Price) %>%
  summarise(DailyQuantity = sum(Quantity, na.rm=TRUE), .groups = 'drop') %>%
  arrange(ItemName, FullDate)

# Convert strings to factors for Random Forest
demand_data$ItemName <- as.factor(demand_data$ItemName)
demand_data$Category <- as.factor(demand_data$Category)
demand_data$DayOfWeek <- as.factor(demand_data$DayOfWeek)
demand_data$Weather <- as.factor(demand_data$Weather)

# Simple Train/Test Split (80/20)
set.seed(42)
index <- createDataPartition(demand_data$DailyQuantity, p=0.8, list=FALSE)
train_set <- demand_data[index,]
test_set <- demand_data[-index,]

# Model Training
# Predicting daily quantity based on item, price, weather, day of week
rf_model <- randomForest(
  DailyQuantity ~ ItemName + Price + Weather + DayOfWeek, 
  data = train_set, 
  ntree = 100, 
  importance = TRUE
)

# Evaluation
predictions <- predict(rf_model, test_set)
rmse_val <- sqrt(mean((test_set$DailyQuantity - predictions)^2))
print(paste("Random Forest RMSE:", round(rmse_val, 2)))

# Save Model
saveRDS(rf_model, file.path(models_dir, "rf_demand_model.rds"))
print("Demand Forecasting Model saved.")


# -----------------------------------------------------
# MODEL 2: Market Basket Analysis (Apriori)
# -----------------------------------------------------
print("Running Market Basket Analysis (Apriori)...")

# We need transactions. Let's group items by CustomerType, Date, Weather, and an arbitrary "Order time" block. 
# Since we generated isolated transactions without a strict "Basket ID", we simulate baskets by grouping:
basket_data <- sales_data %>%
    mutate(BasketID = paste(CustomerType, FullDate, Weather, sep="-")) %>%
    select(BasketID, ItemName) %>%
    distinct()

# Ensure we have data
if (nrow(basket_data) > 0) {
    # Convert to transaction format
    baskets <- split(basket_data$ItemName, basket_data$BasketID)
    trans <- as(baskets, "transactions")

    # Generate Rules
    # Using low support/confidence since data is uniformly synthetic across items
    rules <- apriori(trans, parameter = list(supp=0.01, conf=0.1, minlen=2), control = list(verbose=FALSE))

    # Filter for interesting rules and save
    if (length(rules) > 0) {
        rules_df <- datatable <- as(rules, "data.frame")
        rules_df <- rules_df %>% arrange(desc(lift))
        
        saveRDS(rules_df, file.path(models_dir, "association_rules.rds"))
        print(paste("Apriori generated", length(rules), "rules. Saved to association_rules.rds"))
        print("Top 5 Rules:")
        print(head(rules_df, 5))
    } else {
        print("No association rules found with the specified parameters.")
    }
}


# -----------------------------------------------------
# MODEL 3: Customer Segmentation (K-Means)
# -----------------------------------------------------
print("Running Customer Segmentation (K-Means)...")

# We simulate unique customers since we only have "CustomerType". Let's aggregate by CustomerType for a macro view,
# OR generate pseudo customer IDs based on frequency in our synthetic setup.
# We'll just aggregate by Date & CustomerType to find purchasing patterns across days.

# Define RFM for synthetic "Customer Groups" (Days)
rfm_data <- sales_data %>%
  group_by(CustomerType, DayOfWeek) %>%
  summarise(
    frequency = n(),
    monetary = sum(TotalAmount),
    .groups = 'drop'
  )

# Scale features
scaled_features <- scale(rfm_data %>% select(frequency, monetary))

# K-means
set.seed(123)
k <- 3
clusters <- kmeans(scaled_features, centers = k)
rfm_data$Cluster <- as.factor(clusters$cluster)

saveRDS(rfm_data, file.path(models_dir, "customer_segments.rds"))
print(paste("K-Means trained with", k, "clusters. Saved to customer_segments.rds"))

print("All Data Mining Processes Complete!")
