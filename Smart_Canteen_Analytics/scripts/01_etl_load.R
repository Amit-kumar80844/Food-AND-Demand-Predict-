# 01_etl_load.R
# Pre-requisite: Install necessary packages if missing
if (!require("dplyr")) install.packages("dplyr")
if (!require("RSQLite")) install.packages("RSQLite")
if (!require("DBI")) install.packages("DBI")

library(dplyr)
library(DBI)
library(RSQLite)

print("Starting ETL Process...")

# 1. Extraction
# Determine paths based on execution directory
data_file <- "data/synthetic_canteen_data.csv"
db_file <- "data/canteen_dw.sqlite"
schema_file <- "warehouse/schema.sql"

if (!file.exists(data_file) && file.exists("../data/synthetic_canteen_data.csv")) {
    data_file <- "../data/synthetic_canteen_data.csv"
    db_file <- "../data/canteen_dw.sqlite"
    schema_file <- "../warehouse/schema.sql"
} else if (!file.exists(data_file)) {
    # Full paths fallback
    data_file <- "C:/Users/amits/Data WareHouse Model/Smart_Canteen_Analytics/data/synthetic_canteen_data.csv"
    db_file <- "C:/Users/amits/Data WareHouse Model/Smart_Canteen_Analytics/data/canteen_dw.sqlite"
    schema_file <- "C:/Users/amits/Data WareHouse Model/Smart_Canteen_Analytics/warehouse/schema.sql"
}

print(paste("Reading source data from:", data_file))
raw_sales <- read.csv(data_file)

# 2. Transformation & Cleaning
# Outlier handling for Price using IQR
Q1 <- quantile(raw_sales$price, 0.25, na.rm=TRUE)
Q3 <- quantile(raw_sales$price, 0.75, na.rm=TRUE)
IQR_val <- Q3 - Q1
clean_sales <- raw_sales %>%
  filter(!is.na(item_name) & !is.na(quantity)) %>%
  mutate(quantity = ifelse(quantity <= 0, 1, quantity)) %>%
  filter(price >= (Q1 - 1.5 * IQR_val) & price <= (Q3 + 1.5 * IQR_val))

print(paste("Cleaned rows:", nrow(clean_sales), "Original:", nrow(raw_sales)))

# Create Dimensions Dataframes
dim_item <- clean_sales %>%
  select(ItemName = item_name, Category = category, UnitCost = unit_cost, Price = price) %>%
  distinct()

dim_date <- clean_sales %>%
  select(FullDate = date, DayOfWeek = day_of_week) %>%
  distinct() %>%
  mutate(
    IsWeekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), TRUE, FALSE),
    IsHoliday = FALSE # Synthetic simplification
  )

dim_customer <- clean_sales %>%
  select(CustomerType = customer_type) %>%
  distinct()

# 3. Loading
# Connect to SQLite Data Warehouse
print(paste("Connecting to Data Warehouse at:", db_file))
wh_con <- dbConnect(RSQLite::SQLite(), dbname = db_file)

# Read and execute DDL
if (file.exists(schema_file)) {
    print("Reading and executing Schema...")
    schema_queries <- readLines(schema_file)
    queries_str <- paste(schema_queries, collapse = "\n")
    queries_list <- strsplit(queries_str, ";")[[1]]
    for (q in queries_list) {
        clean_q <- trimws(q)
        if (clean_q != "") {
            dbExecute(wh_con, clean_q)
        }
    }
} else {
    print("Warning: Schema SQL file not found. Ensuring tables exist via RSQLite...")
}

# Insert Dimension Data (handling auto-increment by inserting into empty table or matching)
print("Clearing old data if exists...")
tryCatch(dbExecute(wh_con, "DELETE FROM Fact_Sales"), error=function(e) print("Fact_Sales empty or missing"))
tryCatch(dbExecute(wh_con, "DELETE FROM Dim_Item"), error=function(e) print("Dim_Item empty or missing"))
tryCatch(dbExecute(wh_con, "DELETE FROM Dim_Date"), error=function(e) print("Dim_Date empty or missing"))
tryCatch(dbExecute(wh_con, "DELETE FROM Dim_Customer"), error=function(e) print("Dim_Customer empty or missing"))
tryCatch(dbExecute(wh_con, "DELETE FROM sqlite_sequence"), error=function(e) print("No sequences to clear"))

dbWriteTable(wh_con, "Dim_Item", dim_item, append = TRUE, row.names = FALSE)
dbWriteTable(wh_con, "Dim_Date", dim_date, append = TRUE, row.names = FALSE)
dbWriteTable(wh_con, "Dim_Customer", dim_customer, append = TRUE, row.names = FALSE)

# Retrieve Keys for Fact Table
db_dim_item <- dbGetQuery(wh_con, "SELECT ItemKey, ItemName FROM Dim_Item")
db_dim_date <- dbGetQuery(wh_con, "SELECT DateKey, FullDate FROM Dim_Date")
db_dim_customer <- dbGetQuery(wh_con, "SELECT CustomerKey, CustomerType FROM Dim_Customer")

# Map Keys to Fact Table
fact_sales <- clean_sales %>%
  left_join(db_dim_item, by = c("item_name" = "ItemName")) %>%
  left_join(db_dim_date, by = c("date" = "FullDate")) %>%
  left_join(db_dim_customer, by = c("customer_type" = "CustomerType")) %>%
  select(
    DateKey,
    ItemKey,
    CustomerKey,
    Quantity = quantity,
    TotalAmount = total_amount,
    UnitCost = unit_cost,
    Weather = weather
  )

# Write Fact Table
print("Writing Fact_Sales table...")
dbWriteTable(wh_con, "Fact_Sales", fact_sales, append = TRUE, row.names = FALSE)

print("ETL Process Complete!")
dbDisconnect(wh_con)
